(defun akirak/org-capture-source-quote ()
  (let* ((mode (string-remove-suffix "-mode" (symbol-name major-mode)))
         (url (condition-case nil
                  (browse-at-remote-get-url)
                (error nil))))
    (concat "\n"
            (format "[[%s][%%f]]:" (or url "file:%(abbreviate-file-name \"%F\")"))
            "\n#+BEGIN_SRC " mode "\n%i\n#+END_SRC\n")))

(provide 'akirak-org-capture)
