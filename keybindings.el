(global-set-key (kbd "C-c C-g") 'evil-escape)

(global-set-key (kbd "<S-f11>") 'writeroom-mode)

(spacemacs/declare-prefix "o" "user-defined")

(define-key evil-window-map (kbd "t") 'eyebrowse-create-window-config)
(define-key evil-window-map (kbd "T") 'akirak/move-current-window-to-new-workspace)
(define-key evil-window-map (kbd "q") 'akirak/close-window-or-workspace)

(define-key evil-hybrid-state-map (kbd "C-u") 'backward-kill-sentence)
(define-key evil-hybrid-state-map (kbd "C-w") 'backward-kill-word)
(define-key evil-hybrid-state-map (kbd "C-h") 'evil-backward-char)
(define-key evil-hybrid-state-map (kbd "C-o") 'evil-execute-in-normal-state)

(with-eval-after-load 'helm
  (define-key helm-map (kbd "C-u") 'backward-kill-sentence)
  (define-key helm-map (kbd "C-w") 'backward-kill-word)
  (define-key helm-map (kbd "C-k") 'kill-line))

(define-key minibuffer-local-map (kbd "C-u") 'backward-kill-sentence)
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-h") (lambda () (interactive) (delete-char -1)))
