;; Magit config

(require 'magit-extras)
(setq magit-repository-directories '(("~/.emacs.d" . 0) ("~/workspace/" . 1)))

(put 'magit-clean 'disabled nil)
(define-key magit-mode-map (kbd "C-c C") 'magit-clean)

(provide 'use-magit)
