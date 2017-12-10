(defun image/pngquant-optimizer (src out)
  "Image optimization backend for PNG files using pngquant.
Optimize a PNG image file from SRC to OUT using pngquant.
An executable file of pngquant is required."
  (shell-command-to-string (format "pngquant --verbose --output %s %s"
                           (expand-file-name out)
                           (expand-file-name src)
                           )))

(defcustom image/optimize-backend-alist
  '(("png" . image/pngquant-optimizer))
  "Alist of backends for image optimization. The car of each item is a file extension, and the cdr is a function that takes SRC and OUT as its arguments."
  )

(defcustom image/default-user-cache-dir
  (or (getenv "XDG_USER_CACHE_HOME") (expand-file-name "~/.cache"))
  "The default cache directory of the user. ")

(defcustom image/image-cache-dir
  (expand-file-name "emacs-image-optimize" image/default-user-cache-dir)
  "A directory to save optimized image files temporarily.")

(defun image//generate-temporary-file-name (ext)
  "Generate a temporary file name that has EXT as the extension in image/optimize/image-cache-dir."
  (concat (make-temp-name (expand-file-name "my-image-ts-optimize"
                                            image/image-cache-dir))
          "." ext))

(defun image//save-optimized-image-to-tmp-file (src)
  "Optimize an image file from SRC and save its result in as a temporary file."
  (let* ((ext (file-name-extension src))
         (out (image//generate-temporary-file-name ext)))
    ;; Ensure that the parent directory of OUT exists
    (make-directory (file-name-directory out) t)
    (pcase (assoc ext image/optimize-backend-alist)
      ('nil (error (format "backend is undefined for %s" ext)))
      (`(,_ . ,func) (progn (funcall func src out)
                            out))
    )))

;;;###autoload
(defun image/optimize-buffer-image ()
  "Publish the buffer image."
  (interactive)
  (unless (eq major-mode 'image-mode)
      (error "not in image-mode"))
  (setq-local image/optimized-image-file
              (image//save-optimized-image-to-tmp-file buffer-file-name)))

(provide 'image-optimize)
