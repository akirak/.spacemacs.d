(with-eval-after-load 'deft
  (define-key deft-mode-map (kbd "C-p") 'widget-backward)
  (define-key deft-mode-map (kbd "C-n") 'widget-forward)
  )
