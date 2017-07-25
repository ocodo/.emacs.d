;;; lispyscript-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "lispyscript-mode" "lispyscript-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from lispyscript-mode.el

(autoload 'lispyscript-mode "lispyscript-mode" "\
Major mode for LispyScript

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons "\\.ls\\'" 'lispyscript-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lispyscript-mode" '("lispyscript-font-lock-defaults")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; lispyscript-mode-autoloads.el ends here
