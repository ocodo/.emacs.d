(when (>= emacs-major-version 24)
  (message "Emacs %s - init packages" emacs-version)

  ;; elpa/package.el
  (require 'package)

  (setq package-archives
        '(
          ("melpa"        . "http://melpa.milkbox.net/packages/")
          ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
          ("marmalade"    . "http://marmalade-repo.org/packages/")
          ("e6h"          . "http://www.e6h.org/packages/") ;; Wanderlust
          ("gnu"          . "http://elpa.gnu.org/packages/")
          ))

  (package-initialize)) ;; init elpa packages

(provide 'init-elpa)
