(require 'ox-gfm)

(defun akirak/markdown-demote-all-headlines ()
  (let ((point (point)))
    ;; go to the beginning of the buffer
    (goto-char (point-min))
    ;; if the first line is a headline, demote it
    (when (markdown-on-heading-p) (markdown-demote))
    ;; demote the following headlines
    (letrec ((iter (lambda ()
                     (let ((next (markdown-next-heading)))
                       (if next
                           (progn (markdown-demote)
                                  (funcall iter)))))))
      (funcall iter))
    ;; restore the point
    (goto-char point)))

(defun akirak/org-export-as-gfm (subtreep)
  (let ((org-export-with-toc nil)
        (org-export-with-tags nil)
        (org-export-with-timestamps nil)
        (org-export-with-planning nil)
        (org-export-with-priority nil)
        (org-export-with-todo-keywords nil)
        (org-export-with-properties nil)
        (org-export-with-drawers nil)
        (org-md-headline-style 'setext)
        )
    (org-export-as 'gfm subtreep nil nil)))

;;;###autoload
(defun akirak/org-copy-as-gfm ()
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not in org mode"))
  (let ((gfm-text (akirak/org-export-as-gfm t)))
    (kill-new gfm-text)
    (message "Stored the text into the kill ring as GFM")))

;;;###autoload
(defun akirak/org-append-as-gfm ()
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not in org mode"))
  (let ((gfm-text (akirak/org-export-as-gfm t)))
    (kill-append gfm-text)
    (message "Stored the text into the kill ring as GFM")))

;;;###autoload
(defun akirak/hugo-new-post-from-org ()
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not in org mode"))
  (let* ((initial-content (akirak/org-export-as-gfm nil))
         (env (org-export-get-environment))
         (buffer-title-data (plist-get env :title))
         (buffer-title (if (sequencep buffer-title-data) (car buffer-title-data) nil))
         (post-title (read-from-minibuffer "Title of the post: " buffer-title))
         (new-post (hugo-new-post post-title))
         )
    (if (stringp new-post)
        (with-current-buffer (get-file-buffer new-post)
          (insert initial-content)
          (akirak/markdown-demote-all-headlines)
          (goto-char (point-min))
          ))))

(provide 'akirak-org-gfm)
