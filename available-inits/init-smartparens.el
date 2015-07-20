;; init-smartparens
(smartparens-global-mode t)

(unless (fboundp 'cua-replace-region)
  (defun cua-replace-region ()
    "Replace the active region with the character you type."
    (interactive)
    (let ((not-empty (and cua-delete-selection (cua-delete-region))))
      (unless (eq this-original-command this-command)
        (let ((overwrite-mode
               (and overwrite-mode
                    not-empty
                    (not (eq this-original-command 'self-insert-command)))))
          (cua--fallback))))))

;; Global pairs with wrap shortcuts...
(sp-pair "(" ")"   :wrap "C-c (")
(sp-pair "[" "]"   :wrap "C-c [")
(sp-pair "{" "}"   :wrap "C-c {")
(sp-pair "\"" "\"" :wrap "C-c \"")
(sp-pair "'" "'"   :wrap "C-c '")
(sp-pair "`" "`"   :wrap "C-c `")

;; Major mode specific pairs
(sp-local-pair 'emacs-lisp-mode "`" "'")

;; Smartparens slurp / barf - use instead of paredit
(global-set-key (kbd "C-x r <left>")        'sp-forward-slurp-sexp)
(global-set-key (kbd "C-x r <right>")       'sp-forward-barf-sexp)
(global-set-key (kbd "C-x a <left>")        'sp-backward-slurp-sexp)
(global-set-key (kbd "C-x a <right>")       'sp-backward-barf-sexp)
(global-set-key (kbd "C-x r r")             'sp-rewrap-sexp)
(global-set-key (kbd "C-x r s")             'sp-split-sexp)
(global-set-key (kbd "C-x r j")             'sp-join-sexp)

(provide 'init-smartparens)
