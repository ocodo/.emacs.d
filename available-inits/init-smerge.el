;;; init-smerge --- initialize smerge
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package smerge-mode
  :init ;; before use
  (progn
    (global-set-key (kbd "C-c s")
                    (defhydra hydra-smerge (:body-pre (smerge-mode 1) :color red)
                      "Smerge mode"
                      ("<down>" smerge-next "Next conflict")
                      ("<up>" smerge-prev "Previous conflict")
                      ("<H-left>" smerge-keep-mine "Keep mine")
                      ("<H-right>" smerge-keep-other "Keep other")))))

(provide 'init-smerge)
;;; init-smerge ends here
