(defun my/scan-scratch-dir-template (dir name modified)
  (let ((abbr (abbreviate-file-name dir)))
    (format "* TODO [[file:%s][%s]]
  - Modified: %s
  :PROPERTIES:
  :FILE_MODIFIED: %d
  :END:

  #+BEGIN_SRC shell
cd %s
git remote -v
git status -sb
  #+END_SRC

" abbr name (current-time-string modified) (car modified) dir)))

(defun my/scan-scratch-dir ()
  (interactive)
  (let* ((parent (expand-file-name akirak/scaffold-project-root))
         (dirs (process-lines "find" parent "-mindepth" "2" "-maxdepth" "2" "-type" "d"))
         (buf (generate-new-buffer "*scan-scratch*")))
    (with-current-buffer buf
      (org-mode)
      (dolist (dir dirs)
        (let ((relative-name (file-relative-name dir parent))
              (modified (nth 5 (file-attributes dir))))
          (insert (my/scan-scratch-dir-template dir
                                                relative-name
                                                modified)))))
    (switch-to-buffer-other-window buf)))

(provide 'my-scan-scratch)
