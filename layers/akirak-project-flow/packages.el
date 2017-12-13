(defconst akirak-project-flow-packages
  '(
    (project-init :location local)
    (project-namespace :location local)
    ))

(defun akirak-project-flow/init-project-init ()
  (use-package project-init
    :defer t
    :commands (project-init/create-new-project)))

(defun akirak-project-flow/post-init-project-init ()
  (with-eval-after-load 'project-init
    (require 'project-init-npm)
    (require 'project-init-haskell)
    (add-to-list 'project-init/backend-list 'project-init-javascript-npm-backend)
    (add-to-list 'project-init/backend-list 'project-init-haskell-stack-backend)
    ))

(defun akirak-project-flow/init-project-namespace ()
  (use-package project-namespace
    :defer t
    :commands (project-ns/move-project-to-namespace))
  )

(defun akirak-project-flow/post-init-project-namespace ()
  (with-eval-after-load 'helm-projectile
    (add-to-list 'helm-source-projectile-projects-actions
                 '("Move the project to a namespace" . project-ns/move-project-to-namespace)
                 t)
    )
  (when (featurep 'helm)
    (setq project-ns/choose-namespace-function 'project-ns//helm-choose-namespace))
  )
