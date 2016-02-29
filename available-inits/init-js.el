;; init-js-mode
(dolist (pattern '("\\.jsx\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'js2-jsx-mode)))
(provide 'init-js)
