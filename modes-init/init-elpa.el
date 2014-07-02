(when (string-match "Emacs 24" (version))
  (message "Running Emacs 24 - init ELPA")

  ;; elpa/package.el
  (require 'package)

  (setq package-archives
        '(
          ("melpa"      . "http://melpa.milkbox.net/packages/")
          ("marmalade"  . "http://marmalade-repo.org/packages/")
          ("e6h"        . "http://www.e6h.org/packages/") ;; Wanderlust
          ("gnu"        . "http://elpa.gnu.org/packages/")
          ))

  (package-initialize)) ;; init elpa packages

(provide 'init-elpa)
