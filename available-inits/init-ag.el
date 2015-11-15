;;; init-ag --- initialize ag
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package ag
  :init
  (progn
    (bind-keys :prefix-map ag-global-prefix-map
               :prefix "C-x g"
               ("g" . ag)
               ("a" . ag)
               ("d" . ag-dired)
               ("R" . ag-dired-regexp)
               ("r" . ag-regexp)
               ("k" . ag-kill-buffers))))

(provide 'init-ag)
;;; init-ag ends here
