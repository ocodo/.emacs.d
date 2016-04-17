;; use-sh
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(eval-after-load "sh-mode"
  (setq
   sh-indentation 2
   sh-basic-offset 2))

(dolist (pattern '("\\.zsh$"))
  (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))
(provide 'use-sh)
