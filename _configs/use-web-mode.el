;;; use-web-mode --- initialize web-mode
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package web-mode
  :mode ("\\.mustache\\'"
         "\\.rxml\\'"
         "\\.rhtml\\'"
         "\\.erb\\'"
         "\\.rjs\\'"))

(provide 'use-web-mode)
;;; use-web-mode.el ends here
