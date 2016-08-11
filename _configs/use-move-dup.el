;;; use-move-dup --- initialize move-dup
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package move-dup
  :config ;; before use
  (progn (global-move-dup-mode t)
         (bind-keys
          :map move-dup-mode-map
          ("s-<up>" . md/duplicate-up)
          ("s-<down>" . md/duplicate-down))))

(provide 'use-move-dup)
;;; use-move-dup ends here
