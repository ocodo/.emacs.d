;;; use-anzu --- Anzu mode, pretty isearch and query replace
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package anzu-mode
  :init (global-anzu-mode t)
  :bind (("M-%" . anzu-query-replace)
         ("s-%" . anzu-query-replace-at-cursor)
         ("M-C-%" . anzu-query-replace-regexp)))

(provide 'use-anzu)
;;; use-anzu ends here
