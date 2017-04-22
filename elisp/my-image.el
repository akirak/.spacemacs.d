(defcustom my-image-editor-program "xdg-open"
  "An external image editor program used by my-image-editor command.")

(defun my-image-editor ()
  (interactive)
  (async-start-process "image editor" my-image-editor-program nil buffer-file-name))

(defun pngquant-optimize (src out)
  (shell-command-to-string (format "pngquant --verbose --output %s %s"
                           (expand-file-name out)
                           (expand-file-name src)
                           )))

(defun optimize-image-to-tmp-file (src)
    (let* ((ext (file-name-extension src))
           (cachedir (or (getenv "XDG_USER_CACHE_HOME") (expand-file-name "~/.cache")))
           (out (concat (make-temp-name (concat cachedir "/emacs/my-image-ts-optimize")) "." ext)))
      (make-directory (file-name-directory out) t)
      (cond ((equal ext "png") (progn (pngquant-optimize src out)
                                      out)))))

(defun my/gs-publish-image-file (file)
  (let ((dest-site my/image-publish-gs-site)
        (dest-path (read-from-minibuffer
               "URL of the destination: "
               (if (boundp 'image-publish-gs-path) image-publish-gs-path
                 (concat my/image-publish-default-gs-path
                         (file-name-nondirectory buffer-file-name))))))
    (process-lines "gsutil" "cp" "-a" "public-read" "-n"
                   (expand-file-name file)
                   (concat "gs://" dest-site dest-path))
    (setq-local image-publish-gs-site dest-site)
    (setq-local image-publish-gs-path dest-path)
    (setq-local image-public-url (concat "https://storage.googleapis.com/" dest-site dest-path))
    (kill-new image-public-url)
    (message (concat "Saved the public URL to the kill ring: " image-public-url))))

(defun my-optimize-buffer-image ()
  (interactive)
  (if (not (equal major-mode 'image-mode))
      (message "not in image-mode")
    (setq-local optimized-image-file (optimize-image-to-tmp-file buffer-file-name))))

(defun my-optimize-buffer-image-and-publish ()
  (interactive)
  (if (not (equal major-mode 'image-mode))
      (message "not in image-mode")
    (progn
      (unless (boundp 'optimized-image-file)
        (my-optimize-buffer-image))
      (my/gs-publish-image-file optimized-image-file))))

(provide 'my-image)
