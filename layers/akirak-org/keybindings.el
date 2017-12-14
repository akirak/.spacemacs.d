(spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode "R" 'org-agenda-refile)

(spacemacs/declare-prefix "s o" "org")
(spacemacs/set-leader-keys "s o h" 'helm-org-agenda-files-headings)
(spacemacs/set-leader-keys "s o s" 'helm-multi-swoop-org)

(spacemacs/declare-prefix-for-mode 'org-mode "o" "user-defined")
(spacemacs/set-leader-keys-for-major-mode 'org-mode "oy" 'org-copy-subtree)
(spacemacs/set-leader-keys-for-major-mode 'org-mode "od" 'org-cut-subtree)
(spacemacs/set-leader-keys-for-major-mode 'org-mode "oC" 'org-copy)
(spacemacs/set-leader-keys-for-major-mode 'org-mode "om" 'akirak/org-copy-as-gfm)
