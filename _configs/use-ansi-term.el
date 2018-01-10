;;; use-aani-term --- initialize ansi-term
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package ansi-term
  :init
  (progn
    (bind-key
     "C-x y" 'term-paste)))

(provide 'use-aani-term)
;;; use-aani-term ends here
