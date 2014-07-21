(dolist (pattern '("\\.rb$" "Guardfile" "^Rakefile$" "\.rake$" "\.rxml$" "\.rjs$" ".irbrc$" "\.builder$" "\.ru$" "\.rabl$" "\.gemspec$" "Gemfile$" "^.pryrc$" "^config.ru$"))
  (add-to-list 'auto-mode-alist (cons pattern 'enh-ruby-mode)))

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(add-hook 'enh-ruby-mode-hook
          'flymake-ruby-load)

(add-hook 'enh-ruby-mode-hook
          '(ruby-end-mode))

;; (add-hook 'enh-ruby-mode-hook 'robe-mode)

(provide 'init-enh-ruby)
