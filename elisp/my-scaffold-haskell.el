(defcustom akirak/haskell-stack-custom-templates '()
  "Custom templates for Haskell Stack.")

(defconst akirak/haskell-stack-template-list nil
  "List of standard templates for Haskell Stack.")

(defun akirak/haskell-stack-get-template-list ()
  (if (and (listp akirak/haskell-stack-template-list)
           (not (seq-empty-p akirak/haskell-stack-template-list)))
      akirak/haskell-stack-template-list
    (progn (message "Obtaining a list of stack templates...")
           (setq akirak/haskell-stack-template-list (cdr (process-lines "stack" "templates"))))))

(defun akirak/haskell-stack-choose-template ()
  (let ((inp (helm :sources
                   (list (helm-build-sync-source "Standard template"
                           :candidates (akirak/haskell-stack-get-template-list))
                         (helm-build-sync-source "Custom template"
                           :candidates akirak/haskell-stack-custom-templates))
                   :prompt "Stack template: ")))
    (if (and (stringp inp)
             (string-match "^[^[:space:]]+" inp))
        (match-string 0 inp))))

(defun akirak/scaffold-get-haskell-stack-command ()
  (let ((template-url (akirak/haskell-stack-choose-template)))
    (if (stringp template-url)
        (concat "stack new %s " template-url))))

(provide 'my-scaffold-haskell)
