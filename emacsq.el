;; Emacs init for quick loading for use with git etc.

;;(let ((default-directory user-emacs-directory))
;;  (normal-top-level-add-subdirs-to-load-path))

(package-initialize)
(let ((~ user-emacs-directory))
  (load-file (concat ~ "custom/custom.el"))
  (load-library (concat ~ "custom/quick-funcs"))
  (load-library (concat ~ "modes-init/init-misc-settings"))
  (load-library (concat ~ "modes-init/init-ido"))
)

(require 'gruvbox-theme)


(menu-bar-mode -1)
