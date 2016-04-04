(when (window-system)
  (require 'hideshowvis)

  (add-to-list 'hs-special-modes-alist
       '(ruby-mode
         "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
         (lambda (arg) (ruby-end-of-block)) nil))

  (dolist (hook (list 'emacs-lisp-mode-hook
                      'lisp-mode-hook
                      'ruby-mode-hook
                      'perl-mode-hook
                      'php-mode-hook
                      'python-mode-hook
                      'lua-mode-hook
                      'c-mode-hook
                      'java-mode-hook
                      'js-mode-hook
                      'css-mode-hook
                      'c++-mode-hook))
    (add-hook hook 'hideshowvis-enable)))

(provide 'use-hideshowvis)
