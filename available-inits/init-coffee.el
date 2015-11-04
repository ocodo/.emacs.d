;;; init-coffee --- initialize coffee-mode
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package coffee-mode
  :defines coffee-mode-map
  :config
  (progn
    (use-package jasmine-coffee)
    (bind-keys :map coffee-mode-map
               ("C-c C-n" . flymake-goto-next-error)
               ("C-c C-p" . flymake-goto-prev-error)
               ("C-c C-r" . coffee-compile-region)
               ("C-c C-," . coffee-indent-shift-left)
               ("C-c C-." . coffee-indent-shift-right))))

(provide 'init-coffee)
;;; init-coffee ends here
