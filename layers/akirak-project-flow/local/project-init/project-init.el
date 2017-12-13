(require 'popwin)

(defcustom project-init/parent-directories nil
  "A list of parent directories with options in which you are likely to create new projects.

:structure    When nil, the project is created just below the parent directory.
              When date is specified, a directory named YYYY-MM-DD is created in the parent directory, and a new project is created inside it.
              When modifiable is specified, allows the user to interactively modify the path of the parent directory.
")

(defcustom project-init/backend-list '(project-init-plain-backend)
  "A list of backends used to initialise a project directory in project-init/create-new-project.")

(defmacro project-init/define-backend (name label &rest options)
  `(defconst ,name '(,label ,@options)))

(project-init/define-backend project-init-plain-backend "Empty directory")

(defun project-init//choose-backend (project-name)
  "Choose a backend to create a project named PROJECT-NAME."
  (let* ((candidates (mapcar 'symbol-value project-init/backend-list))
         (choice (completing-read (format "Choose a backend to initialise %s project: " project-name)
                                  (mapcar 'car candidates)
                                  nil t)))
    (cadr (assoc choice candidates))))

(defun project-init//directory-empty-p (dir)
  (seq-empty-p (directory-files dir nil "^[^.]" t)))

(defun project-init/open-project-directory-default (dir)
  "The default function used to open a project directory."
  (let ((directory-empty (project-init//directory-empty-p dir))
        (has-projectile (fboundp 'projectile-find-file-in-directory))
        (has-helm (fboundp 'helm-find-files)))
    (cond
     ((and has-projectile
           (not directory-empty)) (projectile-find-file-in-directory dir))
     (has-helm (helm-find-files-1 (file-name-as-directory dir)))
     (t (find-file (read-file-name "Open a file in the project: " dir))))))

(defcustom project-init/open-project-directory-function
  'project-init/open-project-directory-default
  "The function used to open project directories created by project-init/create-new-project.

If this value is nil, the project created by the function is not opened in Emacs. ")

(defcustom project-init/git-init-on-creation t
  "When non-nil, 'git init' is run after a project is created by project-init/create-new-project.")

(defun project-init//inside-git-repo-p (dir)
  "Check if DIR is inside a Git repository."
  (let ((default-directory dir))
    (condition-case nil
        (process-lines "git" "rev-parse" "--show-toplevel")
      (error nil))))

(defun project-init//git-init-on-directory (dir)
  "Run 'git init' in the directory DIR.

This function prevents from running 'git init' in either of the following cases:
- project-init/git-init-on-creation variable is nil.
- The directory is inside another Git repository. "
  (when (and project-init/git-init-on-creation
             (not (project-init//inside-git-repo-p dir)))
    (let ((default-directory dir))
      (process-lines "git" "init"))))

(defun project-init//open-project-directory (dir)
  "Open the directory given as DIR using a function specified in the variable project-init/open-project-directory-function."
  (pcase project-init/open-project-directory-function
    ('nil nil)
    (f (funcall f dir))))

(defcustom project-init/init-project-directory-hook
  '(project-init//git-init-on-directory
    project-init//open-project-directory)
  "Actions to run after a project directory is initialised by project-init/create-new-project function.")

(defun project-init//run-init-hook (dir)
  (cl-loop for func in project-init/init-project-directory-hook
           do (funcall func dir)))

(defcustom project-init//process-buffer-name "*project-init*"
  "Name of the buffer used to run a background process of project-init.")

(defun project-init//run-init-command (command dir &optional chdir)
  (message (format "Scaffolding a new project %s using command '%s'..." dir command))
  (let ((proc (let ((default-directory (pcase chdir
                                         ('self (progn (make-directory dir) dir))
                                         (_ (file-name-directory dir)))))
                (start-process-shell-command "project-init"
                                             project-init//process-buffer-name
                                             command)))
        (sentinel (lexical-let ((dir dir))
                    (lambda (process event)
                      (pcase event
                        ((pred (string= "finished\n")) (progn (popwin:close-popup-window)
                                                              (project-init//run-init-hook dir)))
                        ((pred (string-prefix-p "exited abnormally")) (error (format "Error from scaffolding command %s: %s"
                                                                                     process
                                                                                     event)))
                        (_ (with-current-buffer project-init//process-buffer-name (princ event))))))))
    (popwin:display-buffer-1 project-init//process-buffer-name)
    (set-process-sentinel proc sentinel)))

(defun project-init//run-backend (project-path backend)
  "Run BACKEND to initialise a project at PROJECT-PATH."
  (let ((project-name (file-name-nondirectory project-path))
        (parent-dir (file-name-directory project-path)))
    (pcase backend
      ;; string is passed as a command to run in the parent directory with %s parameter replaced with the name
      ((pred stringp) (project-init//run-init-command (format backend project-name)
                                                      project-path))
      ;; When nil, an empty directory is created
      ('nil (progn (make-directory project-path)
                   (project-init//run-init-hook project-path)))
      ;; symbol bound to a function is evaluated as a command
      ((and (pred symbolp)
            (pred fboundp)) (project-init//run-backend project-path (funcall backend)))
      ;; symbol bound to a variable is evaluated as a command
      ((and (pred symbolp)
            (pred boundp)) (project-init//run-backend project-path (symbol-value backend)))
      ;; lambda expression is evaluated as a command
      (`(lambda . ,_) (project-init//run-backend project-path (funcall backend)))
      ;; (TEMPLATE OPTION...)
      ((and `(,template . ,options)
            (guard (stringp template))) (project-init//run-init-command (format template project-name)
                                                                        project-path
                                                                        (when (eq (plist-get options :chdir) 'self)
                                                                          'self)))
      ;; Deprecated. (:command-in-directory TEMPLATE) run TEMPLATE in the project directory itself
      (`(:command-in-directory ,template) (project-init//run-init-command (format template project-name)
                                                                          project-path
                                                                          'self))
      (_ (error (concat "did not match any backend " (prin1-to-string backend))))
      )))

(defun project-init//get-parent-dir (name &optional parent-dir)
  (pcase (or (pcase parent-dir
               ('nil nil)
               (stringp (list parent-dir))
               (_ parent-dir))
             (let ((candidates (cl-loop for x in project-init/parent-directories
                                        collect (cond
                                                 ((stringp x) (list x))
                                                 ((listp x) x)))))
               (assoc (completing-read (format "Choose a parent directory for %s: " name)
                                       candidates
                                       nil t)
                      candidates)))
    (`(,initial-path . ,options)
     (let ((path (pcase (plist-get options :structure)
                   ('nil initial-path)
                   ('date (expand-file-name (format-time-string "%Y-%m-%d") initial-path))
                   ('modifiable (read-file-name "Choose/enter a parent directory: "
                                                (file-name-as-directory initial-path)))
                   (s (error (format "Unsupported structure option: %s" (symbol-name s))))
                   )))
       (make-directory path t)
       path))))

(defun project-init//verify-project-directory (parent name)
  (let ((path (expand-file-name name parent)))
    (if (file-exists-p path)
        (project-init//verify-project-directory parent
                                                (read-from-minibuffer (format "%s already exists. Enter another name: " path)
                                                                      name))
      path)))

;;;###autoload
(defun project-init/create-new-project (&rest options)
  "Initialise a new project directory using a backend. "
  (interactive)
  (let* ((project-name (read-from-minibuffer "Enter the name of a new project: "))
         (parent-dir (project-init//get-parent-dir project-name (plist-get options :parent-dir)))
         (real-path (project-init//verify-project-directory parent-dir project-name)))
    (project-init//run-backend real-path
                               (project-init//choose-backend (file-name-nondirectory real-path)))))

(provide 'project-init)
