;; Magit config

(require 'magit-extras)

(put 'magit-clean 'disabled nil)
(define-key magit-mode-map (kbd "C-c C") 'magit-clean)

(provide 'use-magit)
