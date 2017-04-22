(defun akirak/org-subtree-to-gfm (destination)
  (org-mark-subtree)
  (call-process-region
   (region-beginning) (region-end)
   "pandoc" nil destination nil
   "-f" "org" "-t" "markdown_github")
  (deactivate-mark))

(defun akirak/get-markdown-buffer-list ()
  (seq-filter
   (lambda (filename)
       (and (stringp filename) (string-suffix-p ".md" filename)))
   (mapcar 'buffer-file-name (buffer-list))))

(setq akirak/org-subtree-last-gfm-destination nil)

(defun akirak/org-subtree-to-gfm-buffer (destination)
  (setq akirak/org-subtree-last-gfm-destination destination)
  (akirak/org-subtree-to-gfm (get-file-buffer destination))
  (message (concat "Sent to " destination)))

(defun helm-org-subtree-to-gfm-buffer ()
  (interactive)
  (let*
      ((markdown-buffer-list (akirak/get-markdown-buffer-list))
       (latest akirak/org-subtree-last-gfm-destination)
       (latest-source
        '((name . "Latest destination")
          (candidates . (lambda () (if latest (list latest) '())))
          (action . akirak/org-subtree-to-gfm-buffer)))
       (markdown-buffer-source
        '((name . "Markdown buffers")
          (candidates . markdown-buffer-list)
          (action . akirak/org-subtree-to-gfm-buffer))))
    (helm :sources (list latest-source markdown-buffer-source))))

(provide 'my-org-to-markdown)
