;; Ruby mode filetype hooks ------------------------------------------------------------------------
;; -- this will need migrating to init-ruby-mode.el or sumthin'

(dolist (pattern '("\\.rb$" "Guardfile" "^Rakefile$" "\.rake$" "\.rxml$" "\.rjs$" ".irbrc$" "\.builder$" "\.ru$" "\.rabl$" "\.gemspec$" "Gemfile$" "^.pryrc$" "^config.ru$"))
   (add-to-list 'auto-mode-alist (cons pattern 'ruby-mode)))

(add-hook 'ruby-mode-hook
          'flymake-ruby-load)

(add-hook 'ruby-mode-hook
          (lambda ()
            (ruby-end-mode)))

(provide 'init-ruby)
