(defconst hugo-packages
  '(
    (hugo :location local)
    ))

(defun hugo/init-hugo ()
  (use-package hugo
    :defer t
    :commands (hugo-server-start-projectile
               hugo-server-stop
               hugo-undraft-projectile
               hugo-new-post
               )
    )
  )
