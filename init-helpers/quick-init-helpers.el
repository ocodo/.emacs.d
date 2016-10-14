;;; Code:

(defun init-set-custom ()
  "Custom set and loaded from local (non-git) or regular (git-shared)."
  (let* ((default-directory user-emacs-directory)
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
  "Manage history."
  (require 'savehist)
  (eval-after-load "savehist-mode"
    (progn
      (let ((default-directory user-emacs-directory))
        (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
        (setq savehist-file (expand-file-name "tmp/savehist")))))
  (savehist-mode 1))

(defun simple-mode-line-modification ()
  "Very simple mode line modification that I like to use."
  (interactive)
  (set-face-attribute 'mode-line nil
                      :inherit 'mode-line-face
                      :font "SauceCodePro Nerd Font"
                      :weight 'ultra-light
                      :foreground "gray60"
                      :background "gray20"
                      :height 120
                      :inverse-video nil
                      :box '(
                             :line-width 6
                                         :color "gray20"
                                         :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :inherit 'mode-line-face
                      :font "SauceCodePro Nerd Font"
                      :weight 'ultra-light
                      :foreground "gray80"
                      :background "gray40"
                      :height 120
                      :inverse-video nil
                      :box '(
                             :line-width 6
                                         :color "gray40"
                                         :style nil)))

(provide 'quick-init-helpers)
;;; quick-init-helpers.el ends here
