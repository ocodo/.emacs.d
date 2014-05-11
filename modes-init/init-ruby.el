(dolist (pattern '("\\.rb$" "Guardfile" "^Rakefile$" "\.rake$" "\.rxml$" "\.rjs$" ".irbrc$" "\.builder$" "\.ru$" "\.rabl$" "\.gemspec$" "Gemfile$" "^.pryrc$" "^config.ru$"))
   (add-to-list 'auto-mode-alist (cons pattern 'ruby-mode)))

(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-mode-hook 'ruby-end-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'company-robe)

(provide 'init-ruby)
