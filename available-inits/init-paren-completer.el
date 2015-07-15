;;; init-paren-completer --- setup paren-completer
;;; Commentary:
;;  setup paren-completer...
;;; Code:

(require 'paren-completer)

(global-set-key (kbd "s-]") 'paren-completer-add-single-delimiter)
(global-set-key (kbd "s-}") 'paren-completer-add-all-delimiters)

(provide 'init-paren-completer)

;;; init-paren-completer.el ends here
