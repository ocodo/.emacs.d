;; use-js-mode
(dolist (pattern '("\\.jsx?\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'js2-jsx-mode)))
;; js2-jsx mode + eslint

(defun insert-semi-colon-at-eol ()
  "Insert semi-colon at end of line and return cursor to current
    point."
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))

(bind-key "C-c C-c" 'insert-semi-colon-at-eol js2-mode-map)

(defun use-eslint-js2 ()
  (interactive)
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  (setq sgml-basic-offset 2
        js2-basic-offset 2)

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")

  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist)))

  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
)

(add-hook 'js2-jsx-mode-hook #'(lambda () (use-eslint-js2)))

(provide 'use-js)
