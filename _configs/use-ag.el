;;; use-ag --- initialize ag
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package ag
  :init
  (progn
    (add-to-list 'ag-arguments "--hidden")
    (bind-keys :prefix-map ag-global-prefix-map
               :prefix "C-x g"
               ("g" . ag)
               ("a" . ag)
               ("d" . ag-dired)
               ("R" . ag-dired-regexp)
               ("r" . ag-regexp)
               ("k" . ag-kill-buffers))))

(provide 'use-ag)
;;; use-ag ends here
