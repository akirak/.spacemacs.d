(defcustom akirak/project-scaffolder-alist '(("plain" . t))
  "Alist of project scaffolding back-ends.")

(defcustom akirak/scaffold-project-root "~/scratch"
  "The root directory of scaffolded projects.")

(defun akirak/choose-scaffolder (name)
  (let* ((candidates akirak/project-scaffolder-alist)
         (k (completing-read
             (concat "How to scaffold " name ": ")
             (mapcar 'car candidates) nil t))
         (choice (cdr (assoc k candidates))))
    (cond ((stringp choice) choice)
          ((listp choice) choice)
          ((booleanp choice) choice)
          ((symbolp choice) (cond ((fboundp choice) (funcall choice))))
          )))

(defun akirak/open-scaffolded-project-default (dir)
  (if (seq-empty-p (directory-files dir nil "^[^.]" t))
      (helm-find-files-1 (file-name-as-directory dir))
    (projectile-find-file-in-directory dir)))

(defcustom akirak/open-scaffolded-project
  'akirak/open-scaffolded-project-default
  "The function used to open the directory of a scaffolt project.")

(defun akirak/on-scaffolding-command-finished (dir)
  (let ((default-directory dir))
    (message (shell-command-to-string "git init")))
  (if (boundp 'akirak/open-scaffolded-project)
      (cond ((symbolp akirak/open-scaffolded-project) (funcall akirak/open-scaffolded-project dir))
            ((functionp akirak/open-scaffolded-project) (funcall akirak/open-scaffolded-project dir))
            )))

(defun akirak/scaffold-project-with-command (command dir)
  (message (format "Scaffolding a new project %s with command '%s'..." dir command))
  (lexical-let ((proc (start-process-shell-command "project-scaffolding" "*scaffolding*" command))
                (dir dir))
    (popwin:display-buffer-1 "*scaffolding*")
    (set-process-sentinel
     proc
     (lambda (process event)
       (cond ((string= event "finished\n")
              (progn (popwin:close-popup-window)
                     (akirak/on-scaffolding-command-finished dir)))
             ((string-prefix-p "exited abnormally" event)
              (error (format "Error from scaffolding command %s: %s" process event)))
             (t (with-current-buffer "*scaffolding*" (princ event))))))
    ))

(defun akirak/scaffold-project-with-command-in-parent (parent-dir dir command)
  (make-directory parent-dir t)
  (let ((default-directory parent-dir))
    (akirak/scaffold-project-with-command command dir)))

(defun akirak/scaffold-project (&optional retry)
  (interactive)
  (let* ((parent-dir (concat akirak/scaffold-project-root (format-time-string "/%Y-%m-%d")))
         (prompt-prefix (when (stringp retry) (concat retry " already exists. ")))
         (project-name (read-from-minibuffer (concat prompt-prefix "Project name: ")))
         (project-path (expand-file-name project-name parent-dir)))
    (if (file-exists-p project-path)
        (akirak/scaffold-project project-name)
      (let ((scaffolder (akirak/choose-scaffolder project-name)))
        (cond ((stringp scaffolder)
               (let ((default-directory parent-dir))
                 (make-directory parent-dir t)
                 (akirak/scaffold-project-with-command
                  (format scaffolder project-name)
                  project-path)))
              ((and (booleanp scaffolder) scaffolder)
               (progn (make-directory project-path t)
                      (akirak/on-scaffolding-command-finished project-path)))
              ((and (listp scaffolder) (equal (car scaffolder) :command-in-directory))
               (let ((command-template (cdr scaffolder)))
                 (progn (make-directory project-path t)
                        (let ((default-directory project-path))
                          (akirak/scaffold-project-with-command
                           (format command-template project-name) project-path))))
                 ))))))

(provide 'my-scaffold)
