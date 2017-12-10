(defconst akirak-emacs-packages
  '(
    writeroom-mode
    (akirak-scratch1 :location local)
    ))

(defun akirak-emacs/init-writeroom-mode ()
    (use-package writeroom-mode
      :defer t
      :commands (writeroom-mode)))

(defun akirak-emacs/init-akirak-scratch1 ()
  (use-package akirak-scratch1
    :defer t
    :commands (akirak/window-new-same-mode
               akirak/window-new-with-name)
    )
  )
