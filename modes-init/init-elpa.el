(when (>= emacs-major-version 24)
  (message "Emacs %s - init packages" emacs-version)

  ;; elpa/package.el
  (require 'package)

  (setq package-archives
        '(("melpa"        . "http://melpa.milkbox.net/packages/")
          ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
          ("gnu"          . "http://elpa.gnu.org/packages/")))

  (package-initialize)) ;; init elpa packages

(provide 'init-elpa)
