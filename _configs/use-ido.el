;;; use-ido --- initialize ido mode.
;;; Commentary:
;;; Code:

;;; use-ido --- initialize ido
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package ido
  :init
  (progn
    (defun ido-tab-do-nothing () (interactive))

    (use-package ido-vertical-mode)
    (ido-mode t)
    (ido-vertical-mode t)
    (ido-everywhere 1)
    (ido-ubiquitous-mode t)
    (flx-ido-mode t)
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
    (setq ido-create-new-buffer 'always)
    (setq ido-enable-flex-matching t)
    (setq ido-auto-merge-work-directories-length -1))

  :bind
  (("M-x" . smex)
   ("M-s-x" . execute-extended-command)
   ("M-X" . smex-major-mode-commands)))

(provide 'use-ido)
;;; use-ido ends here
