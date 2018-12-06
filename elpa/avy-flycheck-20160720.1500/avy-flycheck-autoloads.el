;;; avy-flycheck-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "avy-flycheck" "avy-flycheck.el" (0 0 0 0))
;;; Generated autoloads from avy-flycheck.el

(autoload 'avy-flycheck-goto-error "avy-flycheck" "\
Jump to a flycheck syntax error.
The window scope is determined by `avy-all-windows' (ARG negates it).

\(fn &optional ARG)" t nil)

(autoload 'avy-flycheck-setup "avy-flycheck" "\
Set up default keybindings.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "avy-flycheck" '("avy-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; avy-flycheck-autoloads.el ends here
