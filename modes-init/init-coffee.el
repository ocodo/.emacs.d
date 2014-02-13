(eval-after-load 'coffee-mode
  ;; Add flymake coffee minor mode to coffee-mode hook
  (add-hook 'coffee-mode-hook (lambda ()
                                ;; With a couple of useful flymake bindings...
                                (define-key coffee-mode-map (kbd "C-c C-n") 'flymake-goto-next-error)
                                (define-key coffee-mode-map (kbd "C-c C-p") 'flymake-goto-prev-error)
                                ;; Add a few coffee mode enhancements too
                                (define-key coffee-mode-map (kbd "C-c C-,") 'coffee-indent-shift-left)
                                (define-key coffee-mode-map (kbd "C-c C-.") 'coffee-indent-shift-right))))

(provide 'init-coffee)
