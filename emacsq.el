;;; emacsq --- Quick init
;;; Commentary:
;; Emacs init for quick loading for use with git etc.
;;; Code:
(global-unset-key (kbd "M-`"))

(add-to-list
 'load-path
 (expand-file-name
  (concat user-emacs-directory "init-helpers")))

(require 'quick-init-helpers)
(init-set-custom)

(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path)
  (require 'custom-keys)
  (require 'remember-themes)
  (require 'amitp-mode-line)
  (require 'quick-funcs)
  (require 'elpa-init)
  (require 'use-misc-settings)
  (require 'use-rainbow-delimiters)
  (require 'use-dired)
  (require 'use-remember-theme)
  (require 'use-ido)
  (require 'use-smartparens)
  (require 'switch-window)
  (manage-toolbar-and-menubar))

(message "EmacsQ ready")
