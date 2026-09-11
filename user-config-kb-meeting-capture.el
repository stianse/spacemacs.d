;;; user-config-kb-meeting-capture.el --- Capture target for kb meeting notes -*- lexical-binding: t; -*-

;; Meeting notes live one file per meeting, one folder per recurring series.  The repo
;; files them by what defines the series, so those folders sit in two trees -
;; `meetings/<forum>/' for a standing forum, `products/<product>/meetings/' when every
;; meeting is about one product - which is more than anyone wants to remember at the
;; start of a meeting.  The per-day filename is what makes a note citable, but it also
;; leaves nothing to reopen from recent-files.
;;
;; `my/kb-meeting-capture' prompts for the series with fuzzy completion over both trees
;; and drops point in a fresh level-2 headline under the date heading, creating the file
;; and the heading when needed.
;;
;; lexical-binding is required: `my/kb-meeting--preview' returns a closure.
;;
;; Tested against consult 20250821.1739.  `consult--read' and `consult--file-preview'
;; are private consult API, used for the annotation and preview machinery, which have
;; no public equivalent.  If this breaks after a consult update, start there.

(declare-function consult--read "consult")
(declare-function consult--file-preview "consult")

(defvar my/kb-meeting-root (expand-file-name "~/Code/nf/kb/sources/")
  "Root of the sources tree.  Series folders are discovered beneath it.
One root, not two: `my/kb-meeting--series' finds series folders structurally,
in whichever tree they live.")

(defvar my/kb-meeting-trees '("meetings" "products")
  "Subtrees of the root whose directories are offered as capture targets.
Every directory below these is offered, whether or not it holds notes yet, so a
new series can be started by picking its folder instead of typing a path.  The
tree roots themselves are not offered.  Naming the trees here is also what keeps
`events/', `partners/', `planning/' and `people/' out, by construction rather
than by an exclusion list.")

(defvar my/kb-meeting-excluded-dirs '("people")
  "Directory names never descended into, matched anywhere under the root.
`people' is HR and interview material: gitignored, unbacked, and a command that
creates files must never offer it.")

(defvar my/kb-meeting-show-previous t
  "Non-nil to show the series' previous entry in a side window while capturing.")

(defvar my/kb-meeting--series-history nil
  "Minibuffer history of chosen meeting series, as paths relative to the root.")

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'my/kb-meeting--series-history))

(defvar my/kb-meeting--date nil
  "Capture date, bound by `my/kb-meeting-capture' and read by the preview.")

(defvar my/kb-meeting--pending nil
  "Plist of :series and :date handed to `my/kb-meeting-capture-target'.")

(defvar my/kb-meeting--previous-buffer nil
  "Previous-entry buffer this feature opened, to be killed when capture ends.")

(defconst my/kb-meeting--entry-re
  "\\`\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[^/]*\\.org\\'"
  "Match an entry filename, capturing its ISO date in group 1.
Accepts the suffixed historical names
`2022-10-31+_video.org' and `2023-03-27_uncertain.org'.  Rejects `_MANIFEST.md',
`_preamble_*.org', `.gitkeep' and `monthly-product-update_2026-05.org'.")


;;; Listing

(defun my/kb-meeting--root ()
  "The sources root, as a directory name."
  (file-name-as-directory (expand-file-name my/kb-meeting-root)))

(defun my/kb-meeting--descend-p (dir)
  "Non-nil when series discovery should descend into DIR."
  (let ((name (file-name-nondirectory (directory-file-name dir))))
    (not (or (string-prefix-p "." name)
             (member name my/kb-meeting-excluded-dirs)))))

(defun my/kb-meeting--subdirs (dir)
  "Every directory below DIR, recursively, skipping dotted and excluded ones."
  (let (result)
    (dolist (f (directory-files dir t "\\`[^.]"))
      (when (and (file-directory-p f) (my/kb-meeting--descend-p f))
        (push f result)
        (setq result (append (my/kb-meeting--subdirs f) result))))
    result))

(defun my/kb-meeting--series ()
  "Candidate directories as paths relative to the root.
Every directory under `my/kb-meeting-trees' is offered, not only those that
already hold entries, so a new series is started by picking its folder rather
than typing a path blind.  The count in the annotation is what tells the two
apart.

Returned in plain alphabetical order; the completion UI does the real ordering,
putting the series you have picked before first."
  (let ((root (my/kb-meeting--root)))
    (sort (mapcan (lambda (tree)
                    (let ((top (expand-file-name tree root)))
                      (when (file-directory-p top)
                        (mapcar (lambda (d) (file-relative-name d root))
                                (my/kb-meeting--subdirs top)))))
                  (copy-sequence my/kb-meeting-trees))
          #'string<)))

(defun my/kb-meeting--series-dir (series)
  "Absolute directory for SERIES, a path relative to the root.
Refuses to leave the root or to enter an excluded directory, so a typed path
can create a new series but cannot reach `people' or the wider filesystem."
  (let* ((root (my/kb-meeting--root))
         (dir (file-name-as-directory (expand-file-name series root))))
    (unless (string-prefix-p root dir)
      (user-error "Meeting series must live under %s" root))
    (when (seq-intersection (split-string (file-relative-name dir root) "/" t)
                            my/kb-meeting-excluded-dirs)
      (user-error "`%s' is excluded from meeting capture" series))
    dir))

(defun my/kb-meeting--entries (series)
  "Entries of SERIES as an ascending list of (DATE . PATH).
Ordered by the ISO date in the filename, which is authoritative; mtime is
not, since a clone or checkout stamps every entry with the same time."
  (let ((dir (expand-file-name series (my/kb-meeting--root))))
    (sort (delq nil
                (mapcar (lambda (f)
                          (when (string-match my/kb-meeting--entry-re f)
                            (cons (match-string 1 f) (expand-file-name f dir))))
                        (and (file-directory-p dir) (directory-files dir))))
          (lambda (a b) (string< (car a) (car b))))))

(defun my/kb-meeting--newest-entry (series &optional before)
  "Path of the newest entry of SERIES, strictly older than BEFORE if non-nil."
  (let ((entries (my/kb-meeting--entries series)))
    (cdr (car (last (if before
                        (seq-take-while (lambda (e) (string< (car e) before)) entries)
                      entries))))))


;;; Prompt

(defun my/kb-meeting--annotate (series)
  "Annotate SERIES with the date of its newest entry and its entry count.
A folder with no entries yet shows a dash, which is how a new series is told
apart from an established one in the picker."
  (let ((entries (my/kb-meeting--entries series)))
    (concat (make-string (max 2 (- 40 (length series))) ?\s)
            (if entries
                (format "%s  (%d)" (car (car (last entries))) (length entries))
              "-"))))

(defun my/kb-meeting--preview ()
  "Preview state mapping a series candidate to its newest previous entry.
`consult--file-preview' rather than `consult--file-state': the latter adds a
`return' action that actually opens the file.  Teardown is consult's own -
`exit' arrives with a nil candidate, which triggers its temporary-file cleanup."
  (let ((preview (consult--file-preview))
        (before my/kb-meeting--date))
    (lambda (action cand)
      (funcall preview action (and cand (my/kb-meeting--newest-entry cand before))))))

(defun my/kb-meeting--read-series ()
  "Prompt for a meeting series, creating its directory on confirmation.
Deliberately binds no `completion-styles', so the global orderless setup
applies, and passes no :sort, so vertico's own ordering stands: series you have
picked before come first, by recency and by how often you pick them."
  (require 'consult)
  (let ((series (consult--read (my/kb-meeting--series)
                               :prompt "Meeting series (path under sources/): "
                               :category 'my/kb-meeting-series
                               :require-match nil
                               :history 'my/kb-meeting--series-history
                               :annotate #'my/kb-meeting--annotate
                               :state (my/kb-meeting--preview))))
    (when (string= series "")
      (user-error "No meeting series selected"))
    ;; A typed path relative to the root is how a new subject-defined series
    ;; starts, e.g. "products/quartermaster/meetings".
    (let ((dir (my/kb-meeting--series-dir series)))
      (unless (file-directory-p dir)
        (unless (y-or-n-p (format "Create new meeting series `%s'? " series))
          (user-error "Aborted"))
        ;; No _MANIFEST.md: manifests describe splits, and this is not a split.
        (make-directory dir t)))
    series))


;;; Target

(defun my/kb-meeting--goto-date-heading (date)
  "Leave point on the top-level heading for DATE, appending it if absent.
Return a (BEG . END) marker pair covering a heading this inserted, else nil.
Point must end up ON the heading line: `org-capture-set-target-location' derives
`:target-entry-p' from `org-at-heading-p' here, and `org-capture-place-entry'
then takes the new headline's level from `org-outline-level' and calls
`org-end-of-subtree' itself.  Leaving point at the end of the subtree instead
would insert a second level-1 heading.

The `.*' before DATE accepts every heading shape in the corpus: `* <DATE fr.>',
`* DATE', `* Meeting notes <DATE>', `* Bi-weekly <DATE>' and
`* <DATE> Agenda: ...'."
  (goto-char (point-min))
  (if (re-search-forward (concat "^\\* .*" (regexp-quote date)) nil t)
      (progn (goto-char (line-beginning-position)) nil)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (let ((beg (point-marker)))
      (insert "* ")
      ;; org's own machinery, so the day name matches whatever this Emacs produces.
      (org-insert-time-stamp (org-time-string-to-time date) nil nil)
      (insert "\n")
      (forward-line -1)
      (cons beg (copy-marker (line-end-position) t)))))

(defun my/kb-meeting-capture-target ()
  "Capture target for the meeting-note template.
Uses the series and date stashed by `my/kb-meeting-capture', or prompts for a
series at today's date when the template is reached directly."
  (let* ((pending (or my/kb-meeting--pending
                      (let ((my/kb-meeting--date (format-time-string "%Y-%m-%d")))
                        (list :series (my/kb-meeting--read-series)
                              :date my/kb-meeting--date))))
         (series (plist-get pending :series))
         (date (plist-get pending :date))
         (file (expand-file-name (concat date ".org")
                                 (my/kb-meeting--series-dir series))))
    (setq my/kb-meeting--pending nil)
    ;; Normally `my/kb-meeting--read-series' has already created this after
    ;; confirming.  Repeat it here because `find-file-noselect' on a file in a
    ;; missing directory yields a read-only buffer and a cryptic capture failure.
    (make-directory (file-name-directory file) t)
    (org-capture-put :kb-meeting-previous (my/kb-meeting--newest-entry series date))
    ;; set-buffer, never find-file: for a `function' target org-capture inhibits
    ;; storing the window configuration, so window changes made here would leak.
    (set-buffer (org-capture-target-buffer file))
    (org-capture-put-target-region-and-position)
    (widen)
    (org-capture-put :kb-meeting-heading (my/kb-meeting--goto-date-heading date))))

(defun my/kb-meeting-capture (&optional arg)
  "Capture a note into a kb meeting series entry.
Prompts for the series, then opens a capture buffer on a new level-2 headline
under the date heading of <series>/YYYY-MM-DD.org, creating file and heading as
needed.  With prefix ARG, prompt for the date with `org-read-date'."
  (interactive "P")
  ;; The date is chosen before the series so annotations and preview can be
  ;; scoped to entries strictly before it.  org-capture eats C-u itself, so the
  ;; prefix argument has to be read here rather than in the target function.
  (let* ((my/kb-meeting--date (if arg
                                  (org-read-date nil nil nil "Meeting date")
                                (format-time-string "%Y-%m-%d")))
         (series (my/kb-meeting--read-series)))
    (setq my/kb-meeting--pending (list :series series :date my/kb-meeting--date))
    (org-capture nil "m")))

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("m" "Meeting note [kb]" entry
                 (function my/kb-meeting-capture-target)
                 "* %?")
               t))


;;; Aborting must not write to `sources/'

(defun my/kb-meeting--abort-cleanup ()
  "On an aborted capture, drop the date heading we inserted and skip the save.
`org-capture-finalize' runs `save-buffer' unconditionally before it discards an
aborted capture, so without this a `C-g' would leave behind a new file holding
nothing but a date heading, or write that heading into a pre-existing entry the
user never added to - and everything under `sources/' is append-only."
  (when (and org-note-abort
             (org-capture-get :kb-meeting-heading 'local))
    ;; `org-capture-finalize' overwrites `org-capture-plist' from the
    ;; buffer-local plist after this hook runs, so set :no-save in both.
    (org-capture-put :no-save t)
    (setq-local org-capture-current-plist
                (plist-put org-capture-current-plist :no-save t))
    (let* ((region (org-capture-get :kb-meeting-heading 'local))
           (buf (marker-buffer (car region))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (org-with-wide-buffer
           (delete-region (car region) (cdr region))))))))

(add-hook 'org-capture-prepare-finalize-hook #'my/kb-meeting--abort-cleanup)


;;; Previous entry beside the capture buffer

(defun my/kb-meeting--show-previous ()
  "Show the series' previous entry in a side window beside the capture buffer.
A no-op for every other capture template.  Never fatal: `org-capture-mode-hook'
runs outside org-capture's own error handling, so an unguarded error here would
abort the capture."
  (let ((file (and my/kb-meeting-show-previous
                   (org-capture-get :kb-meeting-previous))))
    (when file
      (condition-case err
          (let* ((existing (find-buffer-visiting file))
                 (buf (or existing (find-file-noselect file t))))
            (unless existing
              (setq my/kb-meeting--previous-buffer buf)
              (with-current-buffer buf (setq buffer-read-only t)))
            (let ((win (display-buffer-in-side-window
                        buf '((side . right)
                              (slot . 0)
                              (window-width . 0.4)
                              (preserve-size . (t . nil))
                              (window-parameters . ((no-other-window . t)
                                                    (no-delete-other-windows . t)))))))
              (when (window-live-p win)
                (with-selected-window win
                  (goto-char (point-min))
                  (when (fboundp 'org-fold-show-all) (org-fold-show-all))))))
        (error (message "kb-meeting: previous entry unavailable: %s"
                        (error-message-string err)))))))

(defun my/kb-meeting--cleanup-previous ()
  "Kill the previous-entry buffer, if this feature was the one to open it.
The side window itself needs no cleanup: `org-capture-finalize' restores the
window configuration captured before the target function ran."
  (when (buffer-live-p my/kb-meeting--previous-buffer)
    (kill-buffer my/kb-meeting--previous-buffer))
  (setq my/kb-meeting--previous-buffer nil))

(add-hook 'org-capture-mode-hook #'my/kb-meeting--show-previous)
(add-hook 'org-capture-after-finalize-hook #'my/kb-meeting--cleanup-previous)
