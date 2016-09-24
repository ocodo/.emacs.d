;;; use-ag --- initialize ag
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package ag
  :init
  (progn
    (add-to-list 'ag-arguments "--hidden")
    (setq ag-group-matches nil)
    (bind-key
     "C-x g" (defhydra ag-hydra (:hint nil) "
[_g_] : Ag Search for string       [_r_] : Ag Search for regex
[_d_] : Ag Dired search for string [_f_] : Ag Dired search for regex
[_k_] : Ag Close all Ag buffers    [Powered by Ag / The Silver Searcher]"
                ("g" ag)
                ("r" ag-regexp)
                ("d" ag-dired)
                ("f" ag-dired-regexp)
                ("k" ag-kill-buffers)))))

(provide 'use-ag)
;;; use-ag ends here
