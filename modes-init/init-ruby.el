(dolist (pattern '("\\.rb$" "Guardfile" "^Rakefile$" "\.rake$" "\.rxml$" "\.rjs$" ".irbrc$" "\.builder$" "\.ru$" "\.rabl$" "\.gemspec$" "Gemfile$" "^.pryrc$" "^config.ru$"))
   (add-to-list 'auto-mode-alist (cons pattern 'ruby-mode)))

(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-mode-hook 'ruby-end-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))
;; (add-hook 'ruby-mode-hook 'company-robe)

(add-hook 'ruby-mode-hook #'(lambda()
                              (define-key ruby-mode-map (kbd "C-c #") 'ruby-make-interpolated-string-at-point-or-region)
                              (define-key ruby-mode-map (kbd "C-c :") 'ruby-toggle-symbol-name)
                              (define-key ruby-mode-map (kbd "C-c {") 'ruby-toggle-block)
                              (define-key ruby-mode-map (kbd "C-c +") 'ruby-toggle-hash-syntax)
                              )

(provide 'init-ruby)
