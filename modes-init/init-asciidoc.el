;; init-asciidoc
(autoload 'asciidoc-mode "asciidoc-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . asciidoc-mode))
(provide 'init-asciidoc)
