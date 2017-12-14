(defun akirak/declare-org-file-structure (dir-options)

  (let ((file-options (cl-loop for (dir . dir-options) in dir-options
                               append (cl-loop for (filename . file-options) in (plist-get dir-options :files)
                                               collect (cons (expand-file-name filename dir) file-options)))))

    (cl-loop for (dir . options) in dir-options
             with defuns = nil
             do (let ((sym (plist-get options :function)))
                  (when sym
                    (push
                     `(defun ,sym ()
                        (cl-loop for file in (directory-files ,dir t "\\.org$")
                                 unless (backup-file-name-p file)
                                 unless (member file (mapcar 'car org-refile-targets))
                                 collect file))
                     defuns)
                    ))
             finally return defuns
             )

    (setq org-refile-targets
          (let ((file-targets (cl-loop for (target . props) in file-options
                                       with targets = nil
                                       do (lexical-let ((refile (plist-get props :refile)))
                                            (when refile (push (cons target refile) targets)))
                                       finally return targets))
                (dir-targets (cl-loop for (_ . dirprops) in dir-options
                                      with targets = nil
                                      do (let ((fun (plist-get dirprops :function))
                                               (refile (plist-get dirprops :refile)))
                                           (when (and fun refile) (push (cons fun refile) targets)))
                                      finally return targets)))
            (cons '(nil . (:maxlevel . 9)) (append file-targets dir-targets))))

    (setq org-agenda-files
          (cl-loop for (target . options) in (append file-options
                                                     dir-options)
                   when (plist-get options :agenda)
                   collect target))
    ))

;; Function to define templates incrementally
(defun akirak/add-org-capture-templates (&rest entries)
  (unless (boundp 'org-capture-templates)
    (setq org-capture-templates '()))
  (cl-loop for entry in entries
           do (let ((v (assoc (car entry) org-capture-templates)))
                (if v
                    (setcdr v (cdr entry))
                  (push entry org-capture-templates)))))

;; Deprecated
(defun my/org-do-ag () (interactive) (helm-do-ag "~/org"))
(defun my/org-archive-do-ag () (interactive) (helm-do-ag "~/org/archives"))

(defun my/find-file-in-org-directory ()
  (interactive)
  (let ((default-directory "~/org"))
    (helm-projectile-find-file)))
