(defun init-set-custom ()
  "Custom set and loaded from local (non-git) or regular (git-shared)"
  (let ((default-directory user-emacs-directory)
        (local-custom (expand-file-name "local/custom.el")))
    (if (file-readable-p local-custom)
        (setq custom-file local-custom)
      (setq custom-file (expand-file-name "custom/custom.el")))
    (load custom-file)))

(defun manage-toolbar-and-menubar ()
  "Turn off toolbar, and unless OSX Gui turn off menubar."
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (unless  (and (window-system) (eq system-type 'darwin))
    (menu-bar-mode -1)))

(defun manage-history ()
  "Manage history"
  (require 'savehist)
  (eval-after-load "savehist-mode"
    (progn
      (let ((default-directory user-emacs-directory))
        (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
        (setq savehist-file (expand-file-name "tmp/savehist")))))
  (savehist-mode 1))

(provide 'quick-init-helpers)


