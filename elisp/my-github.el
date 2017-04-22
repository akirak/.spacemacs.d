;; Create remote repositories for repositories in ~/work/github.com/USER
;; Requires 'hub' command

(defun my-github/ensure-git-repository ()
  "Initialize a Git repository if missing. Returns t if it has a Git repository and nil otherwise."
  (cond ((file-directory-p ".git") t)
        ((yes-or-no-p (concat (abbreviate-file-name default-directory)
                              " does not have a Git repository. Initialize one? "))
         (progn (call-process "git" nil nil nil "init") t))))

(defun my-github/expected-repo-path-from-directory (dir)
  (if (string-match "/github\.com/\\([^/]+\\)/\\([^/]+\\)$" dir)
      (concat (match-string 1 dir) "/" (match-string 2 dir))))

(defun my-github/git-origin-remote ()
  (condition-case nil
      (let ((cmdoutput (process-lines "git" "config" "--local" "remote.origin.url")))
        (if cmdoutput (car cmdoutput)))
    (error nil)))

(defun my-github/create-remote-repository (path)
  (let ((description (read-from-minibuffer "Repository description (optional): ")))
    (apply 'process-lines "hub" "create" expected-repo-path "-p"
           (if (string-empty-p description) '() (list "-d" description)))))

(defun my-github/ensure-remote (dir)
  (let ((default-directory dir))
    (when (my-github/ensure-git-repository)
      (let ((expected-repo-path (my-github/expected-repo-path-from-directory dir))
            (actual-origin (my-github/git-origin-remote)))
        (cond ((and (not actual-origin) expected-repo-path)
               (when (yes-or-no-p (concat "Create github repository '" expected-repo-path "' as origin of " dir "?"))
                 (my-github/create-remote-repository expected-repo-path)))
              ((and actual-origin
                    expected-repo-path)
               (unless (equal actual-origin (concat "git@github.com:" expected-repo-path ".git"))
                 (message (concat dir " should have " expected-repo-path " as origin, but actually it is " actual-origin))
                 (shell)))
              ((not expected-repo-path) (error (concat "failed to generate a remote path from " dir))))))))

(defun my-ensure-github-remotes ()
  (interactive)
  (let ((parent (my/github-personal-dir)))
    (dolist (dir (directory-files parent t "^[^.]"))
      (when (f-directory-p dir)
        (my-github/ensure-remote dir)))))

(provide 'my-github)
