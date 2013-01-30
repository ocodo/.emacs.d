;; elpa/package.el
(require 'package)

(setq package-archives 
      '( 
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("elpa" . "http://tromey.com/elpa/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ))

(package-initialize) ;; init elpa packages

(provide 'init-elpa)
