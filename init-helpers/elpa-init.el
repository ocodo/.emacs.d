(when (>= emacs-major-version 24)
  (message "Emacs %s - init packages" emacs-version)

  ;; elpa/package.el
  (require 'package)

  (setq package-archives
        '(("melpa"        . "https://melpa.org/packages/")
          ("melpa-stable" . "https://stable.melpa.org/packages/")
          ("org"          . "https://orgmode.org/elpa/")
          ("gnu"          . "https://elpa.gnu.org/packages/")))

  (package-initialize)) ;; init elpa packages

(provide 'elpa-init)
