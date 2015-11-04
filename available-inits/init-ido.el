;;; init-ido --- initialize ido mode.
;;; Commentary:
;;; Code:

;;; init-ido --- initialize ido
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package ido
  :init
  (progn
    (use-package ido-vertical-mode)
    (ido-mode t)
    (ido-vertical-mode t)
    (ido-ubiquitous-mode t)
    (flx-ido-mode t)
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
    (setq ido-create-new-buffer 'always)
    (setq ido-enable-flex-matching t)
    (setq ido-everywhere t)
    (setq gc-cons-threshold 30000000))
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))

(provide 'init-ido)
;;; init-ido ends here
