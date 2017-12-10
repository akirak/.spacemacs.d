(defun akirak/deft-strip-quotes (str)
  (cond ((string-match "\"\\(.+\\)\"" str) (match-string 1 str))
        ((string-match "'\\(.+\\)'" str) (match-string 1 str))
        (t str)))

(defun akirak/deft-parse-title-from-front-matter-data (str)
  (if (string-match "^title: \\(.+\\)" str)
      (let* ((title-text (akirak/deft-strip-quotes (match-string 1 str)))
             (is-draft (string-match "^draft: true" str)))
        (concat (if is-draft "[DRAFT] " "") title-text))))

(defun akirak/deft-deft-file-relative-directory (filename)
  (file-name-directory (file-relative-name filename deft-directory)))

(defun akirak/deft-title-prefix-from-file-name (filename)
  (let ((reldir (akirak/deft-deft-file-relative-directory filename)))
    (if reldir
        (concat (directory-file-name reldir) " > "))))

(defun akirak/deft-parse-title-with-directory-prepended (orig &rest args)
  (let ((str (nth 1 args))
        (filename (car args)))
    (concat
      (akirak/deft-title-prefix-from-file-name filename)
      (let ((nondir (file-name-nondirectory filename)))
        (if (or (string-prefix-p "README" nondir)
                (string-suffix-p ".txt" filename))
            nondir
          (if (string-prefix-p "---\n" str)
              (akirak/deft-parse-title-from-front-matter-data
               (car (split-string (substring str 4) "\n---\n")))
            (apply orig args)))))))

(defun akirak/deft-parse-title (filename contents)
  (concat
   (akirak/deft-title-prefix-from-file-name filename)
   (let ((nondir (file-name-nondirectory filename)))
     (cond
      ;; If the filename is README.* or *.txt, return the file name as it is
      ((or (string-prefix-p "README" nondir)
           (string-suffix-p ".txt" filename))
       nondir)
      ;; If the content contains a YAML front matter, extract title from it
      ((string-prefix-p "---\n" contents)
       (akirak/deft-parse-title-from-front-matter-data
        (car (split-string (substring contents 4) "\n---\n"))))
      (t
       (let ((begin (string-match "^.+$" contents)))
         (if begin
             (deft-strip-title
               (substring contents begin (match-end 0)))
           nondir))
      )))))

(provide 'akirak-deft-title)
