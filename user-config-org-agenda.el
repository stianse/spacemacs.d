(setq org-agenda-files '("~/Dropbox/gtd/inbox.org"
                         "~/Dropbox/gtd/gtd.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Dropbox/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")))

(setq org-refile-targets '(("~/Dropbox/gtd/gtd.org" :maxlevel . 3)
                           ("~/Dropbox/gtd/someday.org" :level . 1)))

;; Fetch tags completation from all agenda files
(setq org-complete-tags-always-offer-all-agenda-tags t)

(setq org-agenda-custom-commands
      '(("i" "Inbox" tags "-DONE- | -CANCELED-" ;; trick for a special catch all (with and without a todo state)
         ((org-agenda-files '("~/Dropbox/gtd/inbox.org"))
          (org-agenda-overriding-header "Inbox items")))
        ("w" "Work"
         ((agenda ""
                  ((org-agenda-files '("~/Dropbox/gtd/gcal-work.org"))
                   (org-agenda-span 2)
                   (org-agenda-use-time-grid nil)
                   (org-agenda-prefix-format "  %?-12t% s")
                   (org-agenda-remove-tags t)))
          (tags-todo "+project+@work"
                     ((org-agenda-overriding-header "Next for projects")
                      (org-agenda-prefix-format "%-32:(my/org-agenda-format-parent 30)")
                      (org-agenda-todo-keyword-format "%-4s")
                      (org-agenda-hide-tags-regexp (regexp-opt '("project" "@work")))
                      (org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)
                      (org-agenda-sorting-strategy '(user-defined-down))
                      (org-agenda-cmp-user-defined #'my/org-agenda-cmp-parent-priority)))
          (tags-todo "-project+@work"
                     ((org-agenda-overriding-header "Next single actions")
                      (org-agenda-todo-keyword-format "%-4s")
                      (org-agenda-hide-tags-regexp (regexp-opt '("@work")))
                      (org-agenda-sorting-strategy '(user-defined-down))
                      (org-agenda-cmp-user-defined #'my/org-agenda-cmp-parent-priority)))
          ))
        ("o" "Office only" tags-todo "@office"
         ((org-agenda-overriding-header "Office")
          (org-agenda-prefix-format "%-32:(my/org-agenda-format-parent 30)")
          (org-agenda-todo-keyword-format "%-4s")))
        ("W" "Non-work"
         ((agenda ""
                  ((org-agenda-files '("~/Dropbox/gtd/gcal-private.org"))
                   (org-agenda-span 2)
                   (org-agenda-use-time-grid nil)
                   (org-agenda-prefix-format "  %?-12t% s")
                   (org-agenda-remove-tags t)))
          (tags-todo "+project-@work"
                     ((org-agenda-overriding-header "Next for projects")
                      (org-agenda-prefix-format "%-32:(my/org-agenda-format-parent 30)")
                      (org-agenda-todo-keyword-format "%-4s")
                      (org-agenda-hide-tags-regexp (regexp-opt '("project" "@home")))
                      (org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)
                      (org-agenda-sorting-strategy '(user-defined-down))
                      (org-agenda-cmp-user-defined #'my/org-agenda-cmp-parent-priority)))
          (tags-todo "-project-@work"
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
        ("u" "Untagged tasks" tags-todo "-{.*}")
        ))

(defun my/org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (my/org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (my/org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun my/org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

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
