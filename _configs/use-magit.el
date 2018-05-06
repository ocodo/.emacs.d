;;; use-magit --- Configuration for Magit
;;
;;; Commentary:
;;  Settings for Magit
;;
;;; Code:

(require 'magit)

(setq magit-commit-arguments '("--verbose"))
(setq magit-repository-directories (quote (("~/.emacs.d" . 0) ("~/workspace/" . 1))))
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

(put 'magit-clean 'disabled nil)
(define-key magit-mode-map (kbd "C-c C") 'magit-clean)

(provide 'use-magit)
;;; use-magit.el ends here
