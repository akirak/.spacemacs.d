(setq deft-extras-packages
      '(
        (my-deft-title :location local)
        ))

(defun deft-extras/init-my-deft-title ()
  (use-package my-deft-title))

(defun deft-extras/post-init-my-deft-title ()
  (advice-add 'deft-parse-title :override #'my-deft/parse-title)
  )
