;;; use-term --- initialize ansi-term
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package term
  :init
  (progn
    (bind-key
     "C-x y" 'term-paste)))

(provide 'use-term)
;;; use-aani-term ends here
