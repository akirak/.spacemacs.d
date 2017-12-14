(defconst akirak-org-packages
  '(
    (org-tangle-bindings :location local)
    (akirak-org :location local)
    (akirak-org-gfm :location local)
    (browse-at-remote :location (recipe
                                 :repo "rmuslimov/browse-at-remote"
                                 :fetcher github))
    ))

(defun akirak-org/init-browse-at-remote ()
    (use-package browse-at-remote
      :defer t
      :commands (browse-at-remote-get-url)))

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

(defun akirak-org/init-akirak-org-gfm ()
  (use-package akirak-org-gfm
    :commands (akirak/org-copy-as-gfm
               akirak/org-append-as-gfm
               akirak/hugo-new-post-from-org)
    )
  )

(defun akirak-org/init-akirak-org ()
  (use-package akirak-org-capture
    :functions (akirak/org-capture-source-quote)
    )
  (use-package akirak-org-clock
    :functions (akirak/clock-in-to-next))
  )
