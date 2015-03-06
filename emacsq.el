;; Emacs init for quick loading for use with git etc.

(menu-bar-mode -1)
(package-initialize)

(let ((~ user-emacs-directory))
  (load-file (concat ~ "custom/custom.el")))

(require 'gruvbox-theme)

(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path)
  (load-library "quick-funcs")
  (load-library "init-misc-settings")
  (load-library "init-rainbow-delimiters")
  (load-library "init-ido"))

(message "EmacsQ ready")
