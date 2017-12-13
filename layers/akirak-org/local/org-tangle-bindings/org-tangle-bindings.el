(defcustom akirak/custom-keybinding-source-org-file
  (expand-file-name "doc/CustomKeybindings.org" dotspacemacs-directory)
  "The org file which defines my custom keybindings.")

(defcustom akirak/custom-keybinding-output-file
  (expand-file-name "keybindings.el" dotspacemacs-directory)
  "The Emacs lisp file which defines my custom keybindings.")

;;;###autoload
(defun akirak/visit-custom-keybindings ()
  (interactive)
  (find-file akirak/custom-keybinding-source-org-file))

(defun akirak/org-tangle-custom-keybindings ()
  (when (and (fboundp 'org-babel-tangle)
             (equal (expand-file-name buffer-file-name)
                    akirak/custom-keybinding-source-org-file))
    (org-babel-tangle nil akirak/custom-keybinding-output-file 'emacs-lisp)))

(provide 'org-tangle-bindings)
