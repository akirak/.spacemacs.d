(require 'f)
(require 'git)

(defun project-ns//move-project-directory (src dest)
  "Move a project directory (usually a Git repository) from SRC to DEST."
  (rename-file src dest)
  ;; Rename an entry in projectile
  (when (featurep 'projectile)
    (projectile-remove-known-project src)
    (projectile-add-known-project dest)))

(defun project-ns//read-name-in-directory (dir initial &optional message)
  "Read a filename that does not exist in DIR from the minibuffer."
  (pcase (read-from-minibuffer (format "%sEnter a new name in the directory %s: "
                                       (or message "")
                                       dir)
                               initial)
    ('nil (error "Aborted"))
    (name (let ((path (f-expand name dir)))
            ;; Ensure that there is no conflict in filename with other files and directories
            (if (f-exists? path)
                ;; Repeat until a non-existent name is given
                (project-ns//read-name-in-directory dir initial (format "%s exists. " name))
              path)))))

(cl-defun project-ns//move-project-to-directory (src dest-parent &rest options
                                                     &key
                                                     on-add
                                                     remote-url-patterns
                                                     create-remote
                                                     description
                                                     )
  "Move a project directory SRC to a directory DEST-PARENT.

For other options, see the description of project-ns-namespace variable. "

  ;; Ensure that SRC is not a directory inside DEST-PARENT
  (when (f-ancestor-of? (f-canonical dest-parent) (f-canonical src))
    (error (format "%s is already inside %s" src dest-parent)))
  ;; Read the destination path from the mini-buffer
  (let ((dest (project-ns//read-name-in-directory dest-parent (f-filename src))))
    (project-ns//move-project-directory (directory-file-name src) dest)
    ;; Run on-add action depending on its value
    (pcase on-add
      ;; A function symbol
      ((pred fboundp) (funcall on-add dest))
      ;; A lambda expression
      (`(lambda . ,_) (funcall on-add dest))
      )))

(defcustom project-ns-namespaces nil
  "A list of namespace configurations for project-namespace.

Each item in this list is a list whose contents is a cons cell of a parent directory and properties:

  (DIR &rest PROPERTIES...)

The property list has the following options:

:on-add
This property specifies an action to run after a directory/repository is added to the namespace.
It can be used to help you with operations like creating a repository on a server and adding it as a remote.
Note that this function/expression is not run after a remote repository is cloned to your local machine. It is run only after a directory is moved into this namespace. 

:remote-url-patterns
This property may be used both for adding remote repositories and cloning. 
")

(defvar project-ns//namespace-selection-history nil)

(defcustom project-ns/choose-namespace-function nil
  "Function used to choose an item from a list of namespaces like project-ns-namespaces variable.
This function should have the same arguments as project-ns//choose-namespace.")

(defun project-ns//choose-namespace-default (prompt candidates)
  "Default function used in project-ns//choose-namespace."
  (pcase (completing-read prompt candidates nil t nil)
    ('nil nil)
    (s (assoc s candidates))))

(defun project-ns//choose-namespace (prompt candidates)
  "Choose a namespace from CANDIDATES.

If project-ns/choose-namespace-function is not defined, project-ns//choose-namespace is used. "
  (funcall (if (fboundp 'project-ns//choose-namespace-function)
               project-ns//choose-namespace-function
             'project-ns//choose-namespace-default)
           prompt
           candidates))

(defun project-ns/move-project-to-namespace (src)
  ;; Different action depending on the namespace
  (pcase project-ns-namespaces
    ;; empty
    ('nil (error "project-ns-namespaces variable is not configured."))
    ;; If there is only one namespace, use it
    (`(nsconfig) (apply 'project-ns//move-project-to-directory src nsconfig))
    ;; If there are 2 or more namespaces in the list, choose one
    (items (pcase (project-ns//choose-namespace (format "Choose a directory to move %s to: " src)
                                                items)
             ;; Abort if the user didn't choose an option
             ('nil (message "Aborted"))
             ;; Pass the choice to the function
             (choice (apply 'project-ns//move-project-to-directory src choice))))))

(provide 'project-namespace)
