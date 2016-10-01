;;; esq --- Emacs super quick init

;;; Commentary:
;;  just get  packages initialized and set up basic niceties.

;;; Code:

(package-initialize)
(require 'bind-key)
(require 'smex)
(require 'select-themes)

(bind-key "M-x" 'smex)

(ido-vertical-mode t)
(select-themes 'darktooth)

(provide 'esq)

;;; esq.el ends here
