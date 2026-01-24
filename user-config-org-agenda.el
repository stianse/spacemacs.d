(setq org-agenda-files '("~/Dropbox/gtd/inbox.org"
                         "~/Dropbox/gtd/gtd.org"
                         "~/Dropbox/gtd/tickler.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Dropbox/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")))

(add-hook 'org-capture-mode-hook 'evil-insert-state)
(setq org-refile-targets '(("~/Dropbox/gtd/gtd.org" :maxlevel . 2)
                           ("~/Dropbox/gtd/someday.org" :level . 1)
                           ("~/Dropbox/gtd/tickler.org" :level . 1)))

;; Record time of todo-state changes in LOGBOOK
(setq org-log-into-drawer t)

;; Fetch tags completation from all agenda files
(setq org-complete-tags-always-offer-all-agenda-tags t)

(setq org-agenda-custom-commands
      '(("i" "Inbox" tags "-DONE- | -CANCELED-" ;; trick for a special catch all (with and without a todo state)
         ((org-agenda-files '("~/Dropbox/gtd/inbox.org"
                              "~/Dropbox/gtd/inbox-phone.org"))
          (org-agenda-overriding-header "Inbox items")))
        ("w" "Work"
         ((agenda ""
                  ((org-agenda-files '("~/Dropbox/gtd/gcal-work.org"))
                   (org-agenda-span 2)
                   (org-agenda-use-time-grid nil)
                   (org-agenda-prefix-format "  %?-12t% s")
                   (org-agenda-remove-tags t)))
          ;; For time critical tasks, show for all contexts (both work and private)
          (tags-todo "+SCHEDULED<=\"<today>\"|+DEADLINE={.+}"
                     ((org-agenda-overriding-header "Scheduled and deadlines")
                      (org-agenda-todo-keyword-format "%-4s")
                      (org-deadline-warning-days 14) ;; default, if no per-task warning time is set
                      (org-agenda-skip-function #'my/org-agenda-skip-deadline-if-not-in-warning-period)))
          (tags-todo "+project+@work"
                     ((org-agenda-overriding-header "Next for projects")
                      (org-agenda-prefix-format "%-42:(my/org-agenda-format-parent 40)")
                      (org-agenda-todo-keyword-format "%-4s")
                      (org-agenda-hide-tags-regexp (regexp-opt '("project" "@work")))
                      (org-agenda-skip-function #'my/org-agenda-skip-function-keep-next-or-first-todo)
                      (org-agenda-sorting-strategy '(user-defined-down))
                      (org-agenda-cmp-user-defined #'my/org-agenda-cmp-parent-priority)))
          (tags-todo "-project+@work-SCHEDULED={.+}-DEADLINE={.+}"
                     ((org-agenda-overriding-header "Next single actions")
                      (org-agenda-todo-keyword-format "%-4s")
                      (org-agenda-hide-tags-regexp (regexp-opt '("@work")))
                      (org-agenda-sorting-strategy '(user-defined-down))
                      (org-agenda-cmp-user-defined #'my/org-agenda-cmp-parent-priority)))
          ))
        ("o" "Office only" tags-todo "@office"
         ((org-agenda-overriding-header "Office")
          (org-agenda-prefix-format "%-42:(my/org-agenda-format-parent 40)")
          (org-agenda-todo-keyword-format "%-4s")))
        ("n" "Non-work"
         ((agenda ""
                  ((org-agenda-files '("~/Dropbox/gtd/gcal-private.org"))
                   (org-agenda-span 2)
                   (org-agenda-use-time-grid nil)
                   (org-agenda-prefix-format "  %?-12t% s")
                   (org-agenda-remove-tags t)))
          ;; For time critical tasks, show for all contexts (both work and private)
          (tags-todo "+SCHEDULED<=\"<today>\"|+DEADLINE={.+}"
                     ((org-agenda-overriding-header "Scheduled and deadlines")
                      (org-agenda-todo-keyword-format "%-4s")
                      (org-deadline-warning-days 14) ;; default, if no per-task warning time is set
                      (org-agenda-skip-function #'my/org-agenda-skip-deadline-if-not-in-warning-period)))
          (tags-todo "+project-@work"
                     ((org-agenda-overriding-header "Next for projects")
                      (org-agenda-prefix-format "%-42:(my/org-agenda-format-parent 40)")
                      (org-agenda-todo-keyword-format "%-4s")
                      (org-agenda-hide-tags-regexp (regexp-opt '("project" "@home")))
                      (org-agenda-skip-function #'my/org-agenda-skip-function-keep-next-or-first-todo)
                      (org-agenda-sorting-strategy '(user-defined-down))
                      (org-agenda-cmp-user-defined #'my/org-agenda-cmp-parent-priority)))
          (tags-todo "-project-@work-SCHEDULED={.+}-DEADLINE={.+}"
                     ((org-agenda-overriding-header "Next single actions")
                      (org-agenda-todo-keyword-format "%-4s")
                      (org-agenda-hide-tags-regexp (regexp-opt '("@home")))
                      (org-agenda-sorting-strategy '(user-defined-down))
                      (org-agenda-cmp-user-defined #'my/org-agenda-cmp-parent-priority)))
          ))
        ("p" "Projects"
         ((tags "project"
                ((org-agenda-overriding-header "All projects")
                 (org-agenda-hide-tags-regexp (regexp-opt '("project")))
                 (org-use-tag-inheritance nil)))))
        ("W" "Waiting" todo "WAIT"
         ((org-agenda-overriding-header "In WAIT state")
          (org-agenda-prefix-format "%-48:(my/org-agenda-format-wait-prefix 40)")
          (org-agenda-todo-keyword-format "%-5s")
          (org-agenda-sorting-strategy '(user-defined-down))
          (org-agenda-cmp-user-defined #'my/org-agenda-cmp-wait-timestamp)))
        ("u" "Untagged tasks" tags-todo "-{.*}")
        ))

(defun my/org-agenda-format-parent (n)
  ;; (s-truncate n (org-format-outline-path (org-get-outline-path)))
  (save-excursion
    (save-restriction
      (widen)
      (org-up-heading-safe)
      (s-truncate n (org-get-heading t t)))))

(defun my/org-get-parent-priority (marker)
  "Go to the marker, move up to parent, and return its numeric priority.
Returns the default priority if there is no parent."
  (if (not marker)
      0 ;; If no marker, assume lowest priority
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char marker)
          (if (org-up-heading-safe)
              ;; Return the numeric priority of the parent (A=2000, B=1000, etc.)
              (org-get-priority (thing-at-point 'line t))
            ;; If no parent (top level), return default priority (usually 1000)
            org-priority-default))))))

(defun my/org-get-task-priority (marker)
  "Go to the marker and return its numeric priority."
  (if (not marker)
      0
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char marker)
          (org-get-priority (thing-at-point 'line t)))))))

(defun my/org-agenda-cmp-parent-priority (a b)
  "Compare parent priorities first, then task priorities.
Used in org-agenda-sorting-strategy 'user-defined-down."
  (let* ((marker-a (get-text-property 0 'org-marker a))
         (marker-b (get-text-property 0 'org-marker b))
         (parent-prio-a (my/org-get-parent-priority marker-a))
         (parent-prio-b (my/org-get-parent-priority marker-b)))
    (cond ((> parent-prio-a parent-prio-b) +1)
          ((< parent-prio-a parent-prio-b) -1)
          (t
           ;; Tie-breaker: Compare task priorities
           (let ((task-prio-a (my/org-get-task-priority marker-a))
                 (task-prio-b (my/org-get-task-priority marker-b)))
             (cond ((> task-prio-a task-prio-b) +1)
                   ((< task-prio-a task-prio-b) -1)
                   (t nil)))))))

(defun my/org-agenda-skip-function-keep-next-or-first-todo ()
  "Show all NEXT items. If no NEXT items, show only the first TODO item.
Skip entry if it's not a NEXT item, or if it is a TODO item but there are NEXT items in the subtree siblings, or if it is a TODO item and there is a previous TODO item."
  (let ((state (org-get-todo-state))
        (should-skip nil))
    (cond
     ((string= "NEXT" state)
      ;; Always show NEXT
      (setq should-skip nil))

     ((string= "TODO" state)
      ;; It is a TODO. We skip if:
      ;; 1. Any sibling is NEXT
      ;; 2. OR, no sibling is NEXT, but a previous sibling is TODO

      (let ((has-next-sibling nil)
            (has-prev-todo-sibling nil))

        ;; Check siblings for NEXT
        (save-excursion
          (org-back-to-heading t)
          ;; Check previous siblings
          (while (and (not has-next-sibling) (org-goto-sibling t))
            (let ((sibling-state (org-get-todo-state)))
              (when (string= "NEXT" sibling-state)
                (setq has-next-sibling t))
              (when (string= "TODO" sibling-state)
                (setq has-prev-todo-sibling t))))

          ;; Check next siblings (only for NEXT check, we don't care about subsequent TODOs)
          (unless has-next-sibling
            (org-back-to-heading t)
            (while (and (not has-next-sibling) (org-goto-sibling))
              (when (string= "NEXT" (org-get-todo-state))
                (setq has-next-sibling t)))))

        (if has-next-sibling
            (setq should-skip t)
          (if has-prev-todo-sibling
              (setq should-skip t)))))

     (t (setq should-skip t)))

    (when should-skip
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defconst my/org-wait-state-timestamp-regexp
  "^[ \t]*- State \"WAIT\".*\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( [A-Za-z]+\\.?\\)?\\( [0-9:]+\\)?\\)\\]"
  "Regexp matching LOGBOOK entry for WAIT state change. Group 1 captures the timestamp.")

(defun my/org-get-wait-timestamp (marker)
  "Go to the marker and extract the timestamp when task entered WAIT state.
Searches the LOGBOOK drawer for a state change line matching 'State \"WAIT\"'.
Returns the timestamp as a time value, or nil if not found."
  (if (not marker)
      nil
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char marker)
          (org-back-to-heading t)
          (let ((end (save-excursion (org-end-of-subtree t) (point))))
            (if (re-search-forward my/org-wait-state-timestamp-regexp end t)
                (org-time-string-to-time (match-string 1))
              nil)))))))

(defun my/org-agenda-cmp-wait-timestamp (a b)
  "Compare WAIT timestamps so oldest WAIT appears first (longest wait at top).
Used with org-agenda-sorting-strategy 'user-defined-down."
  (let* ((marker-a (get-text-property 0 'org-marker a))
         (marker-b (get-text-property 0 'org-marker b))
         (time-a (my/org-get-wait-timestamp marker-a))
         (time-b (my/org-get-wait-timestamp marker-b)))
    (cond
     ;; If both have timestamps, older (smaller) timestamp should come first
     ((and time-a time-b)
      (cond ((time-less-p time-a time-b) +1)  ; a is older, should come first
            ((time-less-p time-b time-a) -1)  ; b is older, should come first
            (t nil)))
     ;; Items with timestamps come before items without
     (time-a +1)
     (time-b -1)
     ;; Neither has timestamp
     (t nil))))

(defun my/org-get-wait-days ()
  "Get the number of days since current entry entered WAIT state.
Must be called with point at the entry. Returns nil if no WAIT timestamp found."
  (save-excursion
    (save-restriction
      (widen)
      (org-back-to-heading t)
      (let ((end (save-excursion (org-end-of-subtree t) (point))))
        (if (re-search-forward my/org-wait-state-timestamp-regexp end t)
            (let* ((wait-time (org-time-string-to-time (match-string 1)))
                   (now (current-time))
                   (diff (time-subtract now wait-time)))
              (floor (/ (float-time diff) 86400)))  ; seconds per day
          nil)))))

(defun my/org-agenda-format-wait-prefix (parent-width)
  "Format prefix showing days waiting and parent name.
PARENT-WIDTH is the max width for the parent name."
  (let ((days (my/org-get-wait-days))
        (parent (my/org-agenda-format-parent parent-width)))
    (if days
        (format "%3dd  %s" days parent)
      (format "  ?  %s" parent))))

(defun my/org-agenda-skip-deadline-if-not-in-warning-period ()
  "Skip deadline entries if their warning period hasn't started yet.
Respects per-task warning days (e.g., -1d in DEADLINE). Falls back to
`org-deadline-warning-days' if no per-task warning is specified.
Does not skip scheduled items."
  (let ((deadline-str (org-entry-get nil "DEADLINE")))
    (when deadline-str
      (let* ((deadline-time (org-time-string-to-time deadline-str))
             (today (org-today))
             (deadline-day (time-to-days deadline-time))
             (days-until (- deadline-day today))
             ;; Parse per-task warning days from deadline string (e.g., "-1d")
             (warning-days (if (string-match "-\\([0-9]+\\)d" deadline-str)
                               (string-to-number (match-string 1 deadline-str))
                             org-deadline-warning-days)))
        (when (> days-until warning-days)
          (org-end-of-subtree t))))))
