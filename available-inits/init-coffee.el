;;; init-coffee-mode --- initialize coffee-mode
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package coffee-mode
  :defer t
  :init
  (progn
    (use-package jasmine-coffee ))
  :bind
  (:map coffee-mode-map
        (("C-c C-n" . flymake-goto-next-error)
         ("C-c C-p" . flymake-goto-prev-error)
         ("C-c C-r" . coffee-compile-region)
         ("C-c C-," . coffee-indent-shift-left)
         ("C-c C-." . coffee-indent-shift-right))))

(provide 'init-coffee-mode)
;;; init-coffee-mode ends here
