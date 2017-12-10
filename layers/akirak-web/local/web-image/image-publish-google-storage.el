(defcustom image/google-cloud-storage-site nil
  "Name of the site on Google Cloud Storage where you want to put your image files.")

(defcustom image/google-cloud-storage-path nil
  "Directory on the site on Google Cloud Storage where you want to put your image files.")

(defvar image/public-url nil
  "The published URL of the buffer.")

;;;###autoload
(defun image/publish-file-to-google-cloud-storage (file)
  "Publish a file to a designated site on Google Cloud Storage.

The destination is configured in image/google-cloud-storage-site
and image/google-cloud-storage-path variables. "
  (let ((dest-site (or image/google-cloud-storage-site
                       (error "image/google-cloud-storage-site variable is not set")))
        (dest-path (read-from-minibuffer
               "URL of the destination: "
               ;; Build the initial path from the custom variable and the base name of the buffer
               (concat (or image/google-cloud-storage-path
                           (error "image/google-cloud-storage-path is not set"))
                       (file-name-nondirectory buffer-file-name)))))
    ;; Copy the file to Google Storage with publicly readable attribute using gsutil
    (process-lines "gsutil" "cp" "-a" "public-read" "-n"
                   (expand-file-name file)
                   (concat "gs://" dest-site dest-path))
    (let ((public-url (concat "https://storage.googleapis.com/" dest-site dest-path)))
      ;; Copy the public URL to the clipboard
      (kill-new public-url)
      (message (concat "Saved the public URL to the kill ring: " public-url))
      public-url)))

;;;###autoload
(defun image/optimize-buffer-image-and-publish-to-google-storage ()
  (interactive)
  (unless (eq major-mode 'image-mode)
    (error "not in image-mode"))
  (when (or (null image/public-url)
            (yes-or-no-p "This buffer is already published. Are you sure you want to continue? "))
    (progn
      (unless (boundp 'image/optimized-image-file)
        (image/optimize-buffer-image)
        (sleep-for 1.5))
      (let ((public-url (image/publish-file-to-google-cloud-storage image/optimized-image-file)))
        (setq-local image/public-url public-url)))))

(provide 'image-publish-google-storage)
