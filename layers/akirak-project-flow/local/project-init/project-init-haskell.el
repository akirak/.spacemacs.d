(defcustom project-init//haskell-stack-custom-templates '()
  "Custom templates for Haskell Stack.")

(defvar project-init//haskell-stack-standard-template-alist)

(defun project-init//get-haskell-stack-standard-template-alist ()
  (pcase-let ((cache 'project-init//haskell-stack-standard-template-alist))
    (if (boundp cache)
        (symbol-value cache)
      (progn (message "Obtaining a list of stack templates...")
             (setf (symbol-value cache) (cl-loop for s in (cdr (process-lines "stack"
                                                                              "--no-docker"
                                                                              "templates"))
                                                 collect `(,s . ,(if (string-match "^[^[:space:]]+" s)
                                                                     (match-string 0 s)
                                                                   s))))))))

(defconst project-init//helm-haskell-stack-standard-template-source
  (helm-build-sync-source "Haskell Stack standard templates"
    :candidates #'project-init//get-haskell-stack-standard-template-alist))

(defconst project-init//helm-haskell-stack-custom-template-source
  (helm-build-sync-source "Haskell Stack custom templates"
    :candidates project-init//haskell-stack-custom-templates))

(defun project-init//helm-haskell-stack-template ()
  (helm :sources
        '(project-init//helm-haskell-stack-standard-template-source
          project-init//helm-haskell-stack-custom-template-source)
        :buffer "*helm haskell stack templates*"
        :prompt "Haskell Stack template: "))

(defun project-init//haskell-stack-new-with-template-chosen-by-user ()
  (concat "stack new %s " (project-init//helm-haskell-stack-template)))

(project-init/define-backend project-init-haskell-stack-backend
                             "Haskell Stack"
                             project-init//haskell-stack-new-with-template-chosen-by-user)

(provide 'project-init-haskell)
