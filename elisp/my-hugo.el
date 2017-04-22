(defun get-hugo-project-directory ()
  (if (and (boundp 'hugo-project-directory) (stringp hugo-project-directory))
      hugo-project-directory
    (let ((dir (read-directory-name "Your hugo project directory: ")))
      (setq hugo-project-directory dir)
      dir)))

(defun hugo-create-content (path)
  (let* ((default-directory (get-hugo-project-directory))
         (output (process-lines "hugo" "new" path)))
    (if (and output (listp output))
        (let ((s (car output)))
          (if (string-match "^\\(.+\\) created$" s)
              (match-string 1 s)
            (:error output)))
      (:error "empty output from hugo new"))))

(defun hugo-create-and-edit-content (path &optional title)
  (let ((command-result (hugo-create-content path)))
    (if (stringp command-result)
        (progn
         (find-file command-result)
         ;;; replace the title
         (if title
             (progn
               (goto-char (point-min))
               (search-forward-regexp "^title:.*$")
               (replace-match (concat "title: " title))))
         (end-of-buffer)
         command-result)
        (message (cdr command-result)))))

(defun hugo-build-filename-from-title (title)
  (lexical-let
      ((funcs '(downcase
                (lambda (s) (replace-regexp-in-string "[^-[:alnum:]]" "" s))
                (lambda (s) (replace-regexp-in-string "\s" "-" s)))))
    (reduce 'funcall funcs :from-end t :initial-value title)))

(defun hugo-new-post (&optional title)
  (interactive)
  (let* ((title (if title title (read-from-minibuffer "Title of the blog post: ")))
         (filename-from-user (read-from-minibuffer
                              "File name (without 'post/'): "
                              (hugo-build-filename-from-title title)))
         (path (concat "post/" (file-name-sans-extension filename-from-user) ".md")))
    (hugo-create-and-edit-content path title)))

(defun hugo-undraft-projectile ()
  (interactive)
  (let* ((filename (file-truename (buffer-file-name)))
         (project-root (projectile-project-root))
         (post-path (file-relative-name filename project-root)))
    (when (not (string-suffix-p ".md" filename))
      (error "not a markdown file"))
    (when (string-prefix-p ".." post-path)
      (error (concat "failed to resolve the post path: " post-path
                     " in project root " project-root)))
    (let* ((default-directory project-root)
           (output (process-lines "hugo" "undraft" "--verbose" post-path)))
      (if (listp output)
          (message (concat "Post undrafted: " (car output)))))))

(setq hugo-server-buffer-name "*hugo-server*")

(defun hugo-get-server-process ()
  (let ((proc (get-buffer-process hugo-server-buffer-name)))
    (if (and proc (process-live-p proc))
        proc
        nil)))

;; based on hugo-server in https://blog.tohojo.dk/2015/10/integrating-hugo-into-emacs.html
;; changes:
;; - retrieve the project from projectile
;; - separate start and stop functions
;; - no `-d dev' option, as I am using continuous integration to publish the site
;; - open the site when running the start function even if the server is already running
(defun hugo-server-start-projectile (&optional arg)
  (interactive "P")
  (if (hugo-get-server-process)
      (message "Hugo server is already running")
    (let ((default-directory (projectile-project-root)))
      (start-process "hugo" hugo-server-buffer-name "hugo" "server" "--buildDrafts" "--watch")
      (message "Started Hugo server")))
  (unless arg (browse-url "http://localhost:1313/")))

(defun hugo-server-stop ()
  (interactive)
  (let ((proc (hugo-get-server-process)))
    (if proc
        (progn
          (interrupt-process proc)
          (message "Stopped Hugo server"))
      (message "Hugo server is not running"))))

(provide 'my-hugo)
