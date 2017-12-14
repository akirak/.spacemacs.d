(defun define-org-agenda-custom-command (key &rest entry)
  (unless (boundp 'org-agenda-custom-commands)
    (setq org-agenda-custom-commands nil))
  (let ((v (assoc key org-agenda-custom-commands)))
    (if v
        (setcdr v entry)
      (push (cons key entry) org-agenda-custom-commands))))

(setq org-stuck-projects
      '("+PROJECT" ("NEXT") () "WAITING"))

(defun akirak/org-skip-non-stuck-projects-and-tasks ()
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (level (car (save-excursion (org-heading-components)))))
      (if (save-excursion
            (re-search-forward (format "^\\*\\{%d\\} NEXT " (1+ level))
                               subtree-end t))
          next-headline
        nil))))

(defun akirak/org-skip-except-active-project-tasks ()
  (save-restriction
    (widen)
    (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (subtree-end (save-excursion (org-end-of-subtree t)))
           (heading-components (org-heading-components))
           (todo (nth 2 heading-components))
           (tags (org-get-tags-at))
           (is-project )
           )
      (cond ((not todo)
             ;; if it is not a todo, skip the headline
             next-headline)
            ((member "PROJECT" tags)
             (if (string-equal todo "NEXT")
                 ;; if it is an active project, skip the headline
                 next-headline
               ;; if it is a non-active project, skip the entire subtree
               (save-excursion
                 (goto-char subtree-end)
                 (or (outline-next-heading) (point-max)))
               ))
            ;; if it is a task with a keyword and without subtasks, don't skip it
            ((bh/is-task-p) nil)
            ;; otherwise skip it
            (t next-headline)
            ))))

(defun akirak/org-agenda-truncated-breadcrumb (maxlen seglen)
  (let ((path (org-get-outline-path))
        (sep " > ")
        (truncate (lambda (len str)
                    (if (> (length str) len)
                        (concat (seq-take str (- len 3)) "...")
                      str))))
    (pcase (length path)
      (0 "")
      (1 (funcall truncate maxlen (car path)))
      (2
       (funcall truncate maxlen
                (string-join
                 (list (funcall truncate seglen (car path))
                       (nth 1 path))
                 sep)))
      (_
       (funcall truncate maxlen
                (string-join
                 (list (funcall truncate seglen (car path))
                       "..."
                       (car (last path)))
                 sep)))
      )))

(provide 'akirak-org-agenda)
