(defcustom web-image-editor-program "xdg-open"
  "An external image editor program used by my-image-editor command.")

;;;###autoload
(defun web-image-editor ()
  ;; Run an image editor program in an image buffer.
  (interactive)
  (async-start-process "image editor"
                       web-image-editor-program
                       nil
                       buffer-file-name))

(provide 'image-editor)
