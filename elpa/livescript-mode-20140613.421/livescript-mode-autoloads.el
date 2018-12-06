;;; livescript-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "livescript-mode" "livescript-mode.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from livescript-mode.el

(autoload 'livescript-mode "livescript-mode" "\
Major mode for editing LiveScript code.

Commands:

\\{livescript-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.ls\\'" . livescript-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "livescript-mode" '("livescript-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; livescript-mode-autoloads.el ends here
