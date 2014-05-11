;; init-sh
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(dolist (pattern '("\\.zsh"))
  (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))
(provide 'init-sh)
