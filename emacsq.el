;; Emacs init for quick loading for use with git etc.

;; Turn off menu
(global-unset-key (kbd "M-`"))

(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path)
  (require 'custom-keys)
  (require 'quick-init-helpers)
  (require 'gruvbox-theme)
  (require 'armitp-mode-line)
  (require 'quick-funcs)
  (require 'init-elpa)
  (require 'init-misc-settings)
  (require 'init-rainbow-delimiters)
  (require 'init-dired)
  (require 'init-ido)
  (require 'init-smartparens)
  (manage-toolbar-and-menubar)
  (init-set-custom)
  (armitp-mode-line))

(message "EmacsQ ready")
