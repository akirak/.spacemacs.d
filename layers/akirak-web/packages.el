(setq akirak-web-packages
      '(

        (web-image :location local)

        ))

(defun akirak-web/init-web-image ()

  (use-package image-editor
    :defer t
    :commands (web-image-editor))

  (use-package image-optimize
    :defer t
    :commands (image/optimize-buffer-image)
    )

  (use-package image-publish-google-storage
    :defer t
    :commands (image/publish-file-to-google-cloud-storage
               image/optimize-buffer-image-and-publish-to-google-storage)
    )

  )
