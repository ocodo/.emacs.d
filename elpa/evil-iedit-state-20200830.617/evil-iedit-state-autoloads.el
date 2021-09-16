;;; evil-iedit-state-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-iedit-state" "evil-iedit-state.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from evil-iedit-state.el

(eval-after-load 'expand-region '(progn (defun evil-iedit-state/iedit-mode-from-expand-region (&optional arg) "Start `iedit-mode'." (interactive "P") (evil-iedit-state/iedit-mode arg) (setq overriding-terminal-local-map nil)) (defadvice er/prepare-for-more-expansions-internal (around iedit/prepare-for-more-expansions-internal activate) ad-do-it (let ((default-msg (car ad-return-value)) (default-bindings (cdr ad-return-value))) (setq ad-return-value (cons (concat default-msg ", e to edit") (add-to-list 'default-bindings '("e" evil-iedit-state/iedit-mode-from-expand-region))))))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-iedit-state" '("evil-iedit-state" "iedit-done")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-iedit-state-autoloads.el ends here
