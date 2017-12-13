(defconst akirak-org-packages
  '(
    (org-tangle-bindings :location local)
    ))

(defun akirak-org/init-org-tangle-bindings ()
  (use-package org-tangle-bindings
    :commands (akirak/visit-custom-keybindings
               akirak/org-tangle-custom-keybindings))
  )

(defun akirak-org/post-init-org-tangle-bindings ()
  ;; Automatically tangle the Org file for custom keybindings
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'akirak/org-tangle-custom-keybindings
                        nil 'local))))
