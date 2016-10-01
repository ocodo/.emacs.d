;;; use-smartparens --- configure smartparens
;;; Commentary:
;;; Code:

(require 'cua-base)
(require 'smartparens)

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
(sp-local-pair 'markdown-mode "<kbd>" "</kbd>" :wrap "C-c k")

;; lisp
(sp-with-modes sp--lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))
  (sp-local-pair "`" nil
                 :skip-match (lambda (ms mb me)
                               (cond
                                ((equal ms "'")
                                 (or (sp--org-skip-markup ms mb me)
                                     (not (sp-point-in-string-or-comment))))
                                (t (not (sp-point-in-string-or-comment)))))))

;; Ruby
(sp-local-pair 'ruby-mode "|" "|" :wrap "C-c |")

;; Smartparens slurp / barf - use instead of paredit
(bind-keys
 ("C-x r <left>"    . sp-forward-slurp-sexp)
 ("C-x r <right>"   . sp-forward-barf-sexp)
 ("C-x r <S-left>"  . sp-backward-slurp-sexp)
 ("C-x r <S-right>" . sp-backward-barf-sexp)
 ("C-x r r"         . sp-rewrap-sexp)
 ("C-x r s"         . sp-split-sexp)
 ("C-x r j"         . sp-join-sexp)
 ("C-x r d"         . embrace-delete)
 ("C-x r a"         . embrace-add)
 ("C-x r c"         . embrace-change))

(provide 'use-smartparens)

;;; use-smartparens.el ends here
