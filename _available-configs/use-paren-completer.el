;;; use-paren-completer --- setup paren-completer
;;; Commentary:
;;  setup paren-completer...
;;; Code:
(require 'use-package)

(use-package paren-completer
  :bind (("s-]" . paren-completer-add-single-delimiter)
         ("s-}" . paren-completer-add-all-delimiters)))

(provide 'use-paren-completer)
;;; use-paren-completer.el ends here
