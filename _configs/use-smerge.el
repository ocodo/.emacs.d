;;; use-smerge --- initialize smerge
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package smerge-mode
  :functions smerge-next smerge-prev smerge-keep-all smerge-keep-mine smerge-keep-other
  :config
  (progn
    (bind-key "C-c s" (defhydra hydra-smerge (:body-pre (smerge-mode 1) :color red)
                        "Smerge mode"
                        ("<down>" smerge-next        "Next conflict")
                        ("<up>"   smerge-prev        "Previous conflict")
                        ("M-a"    smerge-keep-all    "Keep all")
                        ("M-m"    smerge-keep-mine   "Keep mine")
                        ("M-o"    smerge-keep-other  "Keep other")))))

(provide 'use-smerge)
;;; use-smerge ends here
