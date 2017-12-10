(setq deft-extras-packages
      '(
        (akirak-deft-title :location local)
        ))

(defun deft-extras/init-akirak-deft-title ()
  (use-package akirak-deft-title))

(defun deft-extras/post-init-akirak-deft-title ()
  (advice-add 'deft-parse-title :override #'akirak/deft-parse-title)
  )
