(require 'jasmine-coffee)

(eval-after-load 'coffee-mode
  (lambda ()
    (define-key coffee-mode-map (kbd "C-c C-n") 'flymake-goto-next-error)
    (define-key coffee-mode-map (kbd "C-c C-p") 'flymake-goto-prev-error)
    (define-key coffee-mode-map (kbd "C-c C-r") 'coffee-compile-region)
    (define-key coffee-mode-map (kbd "C-c C-,") 'coffee-indent-shift-left)
    (define-key coffee-mode-map (kbd "C-c C-.") 'coffee-indent-shift-right)
    ))

(provide 'init-coffee)
