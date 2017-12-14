(spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode "R" 'org-agenda-refile)

(spacemacs/declare-prefix-for-mode 'org-mode "o" "user-defined")
(spacemacs/declare-prefix "oo" "org mode")

(spacemacs/declare-prefix "s o" "org")
(spacemacs/set-leader-keys "s o h" 'helm-org-agenda-files-headings)
(spacemacs/set-leader-keys "s o s" 'helm-multi-swoop-org)
(spacemacs/set-leader-keys "s o a" 'my/org-do-ag)
(spacemacs/set-leader-keys "s o A" 'my/org-archive-do-ag)
(spacemacs/set-leader-keys "s o f" 'my/find-file-in-org-directory)

(spacemacs/set-leader-keys-for-major-mode 'org-mode "oy" 'org-copy-subtree)
(spacemacs/set-leader-keys-for-major-mode 'org-mode "od" 'org-cut-subtree)
(spacemacs/set-leader-keys-for-major-mode 'org-mode "oC" 'org-copy)
(spacemacs/set-leader-keys-for-major-mode 'org-mode "oh" 'helm-org-in-buffer-headings)
(spacemacs/set-leader-keys-for-major-mode 'org-mode "om" 'akirak/org-copy-as-gfm)

(spacemacs/set-leader-keys "oog" 'my/org-repository-magit-status)

(spacemacs/set-leader-keys "|" 'org-clock-goto)
