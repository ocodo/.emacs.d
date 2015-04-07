(dolist (p '("\\.rb$"
                   "Guardfile"
                   "^Rakefile$"
                   "\.rake$"
                   "\.rxml$"
                   "\.rhtml$"
                   "\.rjs$"
                   "\.irbrc$"
                   "\.builder$"
                   "\.ru$"
                   "\.rabl$"
                   "\.gemspec$"
                   "Gemfile$"
                   "^.pryrc$"
                   "^config.ru$"))
  (add-to-list 'auto-mode-alist (cons p 'ruby-mode)))

(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-mode-hook 'ruby-end-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))
;; (add-hook 'ruby-mode-hook 'company-robe)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c #") 'ruby-make-interpolated-string-at-point-or-region)
            (local-set-key (kbd "C-c :") 'ruby-toggle-symbol-name)
            (local-set-key (kbd "C-c {") 'ruby-toggle-block)
            (local-set-key (kbd "C-c +") 'ruby-toggle-hash-syntax)))

(provide 'init-ruby)
