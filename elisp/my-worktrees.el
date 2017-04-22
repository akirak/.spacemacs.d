(defcustom my/work-trees-root nil
  "The root of working trees.")

(defcustom my/github-personal-login nil
  "Your account name on GitHub.")

(defun my/github-personal-dir ()
  (expand-file-name (concat "github.com/" my/github-personal-login) my/work-trees-root))

(defun my/move-directory-to-github-work (dir)
  (if (string-prefix-p (expand-file-name my/work-trees-root) (expand-file-name dir))
      (message (concat dir " is already inside " my/work-trees-root " directory"))
    (let* ((parent (my/github-personal-dir))
           (default-dest (file-name-nondirectory (directory-file-name dir)))
           (not-exist (lambda (f) (not (file-exists-p (expand-file-name f parent)))))
           (dest (read-file-name "New location of the directory: " parent
                                 nil nil default-dest not-exist)))
      (rename-file dir dest)
      (projectile-remove-known-project dir)
      (projectile-add-known-project dest)
      )))

(provide 'my-worktrees)
