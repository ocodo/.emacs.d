;; use-json-mode
(dolist (pattern '(
                   "\\.jshintrc\\'"

                   "\\.jslint\\'"

                   "\\.eslintrc\\'"

                   ))
  (add-to-list 'auto-mode-alist (cons pattern 'json-mode)))
(provide 'use-json-mode)
