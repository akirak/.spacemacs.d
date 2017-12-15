;; options for org-babel (ob)
(setq org-confirm-babel-evaluate nil
      org-src-tab-acts-natively t)

;; options for org-clock
(setq org-clock-history-length 23
      org-clock-in-resume t
      org-drawers '("PROPERTIES" "LOGBOOK")
      org-clock-into-drawer t
      org-clock-out-remove-zero-time-clocks t
      org-clock-out-when-done t
      org-clock-persist t
      org-clock-persist-query-resume nil
      org-clock-auto-clock-resolution 'when-no-clock-is-running
      org-clock-report-include-clocking-task t
      org-clock-in-switch-to-state #'akirak/clock-in-to-next
      )

(akirak/declare-org-file-structure
 '(("~/org/todo"
    :files
    (("Inbox.org" :agenda t))
    )
   ("~/org"
    :files
    (("WeeklyReviews.org" :refile (:level . 0))
     ))
   ("~/org/misc"
    :files
    (("DataMigration.org" :refile (:maxlevel . 3))
     ))
   ("~/org/practice"
    :files
    (("ProgrammingRecipes.org" :agenda t :refile (:maxlevel . 2))
     ("ProgrammingLanguages.org" :agenda t)
     ))
   ))

(setq org-default-notes-file "~/org/todo/Inbox.org")
(setq akirak/org-inbox-file  "~/org/todo/Inbox.org")
(setq org-capture-templates '())

;; Templates for entries in akirak/org-inbox-file
(akirak/add-org-capture-templates

 '("t" "todo" entry (file akirak/org-inbox-file)
   "* TODO %^{Title}
  :PROPERTIES:
  :CAPTURED_AT: %U
  :END:
  %?
" :clock-in t :clock-resume t)

 '("n" "note" entry (file akirak/org-inbox-file)
   "* %? :NOTE:
  :PROPERTIES:
  :CAPTURED_AT: %U
  :END:
  %i
" :clock-in t :clock-resume t)

 '("s" "quote selected source code" plain (file+function akirak/org-inbox-file end-of-buffer)
   (function akirak/org-capture-source-quote) :immediate-finish t)

 )

;; org-protocol templates (added to akirak/org-inbox-file)
(akirak/add-org-capture-templates

 '("Pn" "(Protocol quote)" entry (file akirak/org-inbox-file)
  "* %? :QUOTE:
[[%:link][%:description]]
#+BEGIN_QUOTE
%i
#+END_QUOTE
")

 '("Pb" "(Protocol bookmark)" plain (file+olp+datetree akirak/org-inbox-file "Bookmarks")
   "     - [[%:link][%:description]]%?\n")

)

;; templates for other files
(akirak/add-org-capture-templates

 '("p" "journal for tracking progress"
   entry
   (file+datetree+prompt "~/keybase-git/personal-journal/journal/reflection.org")
   "* %?
  :PROPERTIES:
  :CAPTURED_AT: %U
  :END:
")

 ;; Journal
 '("j" "journal" plain (file "~/Dropbox/Apps/jrnl.txt")
  "\n%<%F %R> %?")

 )

;; bunch of options
(setq org-startup-truncated nil)
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
;; (require 'bh-org-config)
;; (setq org-refile-target-verify-function 'bh/verify-refile-target)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-archive-location "~/org/archives/v1/%s_archive::datetree/")

;; org-reveal
;; ;FIXME: Bump the version or always use the latest version
(setq org-reveal-root "https://cdn.bootcss.com/reveal.js/3.4.1/")

;; tags
(setq org-fast-tag-selection-single-key t)
(setq org-tag-persistent-alist nil)
;; (setq org-tags-exclude-from-inheritance '())

;; Workflow states
(setq org-enforce-todo-dependencies t)
(setq org-use-fast-todo-selection t)

(setq my-org-todo-keyword-options
      '(("TODO"
         :faces (:foreground "DarkOrange" :weight bold)
         )
        ("NEXT"
         :faces (:foreground "LightSkyBlue" :weight bold)
         )
        ("DONE"
         :faces (:foreground "Green3" :weight bold)
         )
        ("WAITING"
         :faces (:foreground "Orange1" :weight bold)
         )
        ("CANCELLED"
         :faces (:foreground "Red3" :weight bold)
         )
        ))

(setq org-todo-state-tags-triggers
      (append
       '((done ("WAITING")))
       (cl-loop for (name . options) in my-org-todo-keyword-options
                collect (cons name (plist-get options :trigger)))))

(setq org-todo-keyword-faces
      (cl-loop for (name . options) in my-org-todo-keyword-options
               collect (cons name (plist-get options :faces))))

(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "NEXT(n!)"
         "WAITING(w@/!)"
         "|" "DONE(d)" "CANCELLED(c@/!)")
        ))

(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-protocol)
  )

(with-eval-after-load 'org-protocol
  (defun akirak/org-protocol-store-link-advice (orig &rest args)
    (raise-frame)
    (apply orig args))
  (advice-add 'org-protocol-store-link :around
              #'akirak/org-protocol-store-link-advice)
  )

(with-eval-after-load 'ob
  (require 'ob-ditaa)
  (require 'ob-dot)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (ditaa . t)
     (dot . t)
     (python . t)
     ))
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  )

(with-eval-after-load 'org-clock
  (org-clock-persistence-insinuate)
  (spacemacs/toggle-mode-line-org-clock-on)
  )
