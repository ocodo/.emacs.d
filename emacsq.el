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
  (require 'remember-theme)
  (require 'armitp-mode-line)
  (require 'quick-funcs)
  (require 'elpa-init)
  (require 'init-misc-settings)
  (require 'init-rainbow-delimiters)
  (require 'init-dired)
  (require 'init-remember-theme)
  (require 'init-ido)
  (require 'init-smartparens)
  (require 'switch-window)
  (manage-toolbar-and-menubar))

(message "EmacsQ ready")
