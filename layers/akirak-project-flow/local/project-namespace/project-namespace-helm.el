(require 'f)

(defun project-ns//helm-choose-namespace (prompt candidates)
  "An implementation of project-ns//choose-namespace using Helm."
  (let* ((fmt (concat "%-"
                      (int-to-string (seq-max (mapcar 'length (mapcar 'car candidates))))
                      "s   %s"))
         (build-candidate (lambda (c)
                            (when c (cons (format fmt
                                                  (f-short (car c))
                                                  (or (plist-get (cdr c) :description) ""))
                                          c))))
         (history-items (cl-loop for path in (delete-duplicates project-ns//namespace-selection-history
                                                                :test 'string-equal)
                                 collect (assoc path candidates)))
         (x (helm :sources (list (helm-build-sync-source "Namespace history"
                                   :candidates (mapcar build-candidate history-items))
                                 (helm-build-sync-source "Configured namespaces"
                                   :candidates (mapcar build-candidate candidates)))
                  :prompt prompt
                  :buffer "*helm project namespaces*")))
    (when x (add-to-list 'project-ns//namespace-selection-history (car x)))
    x))

(provide 'project-namespace-helm)
