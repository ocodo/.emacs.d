;;; js3-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "js3" "js3.el" (0 0 0 0))
;;; Generated autoloads from js3.el
 (add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))

(autoload 'js3-mode "js3" "\
Major mode for editing JavaScript code.

\\{js3-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js3" '("js3" "lazy-detect" "char-is-" "with-buffer" "neq" "deflocal")))

;;;***

;;;### (autoloads nil "js3-mode" "js3-mode.el" (0 0 0 0))
;;; Generated autoloads from js3-mode.el
 (add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))

(autoload 'js3-mode "js3-mode" "\
Major mode for editing JavaScript code.

\\{js3-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js3-mode" '("js3" "lazy-detect" "char-is-" "with-buffer" "neq" "deflocal")))

;;;***

;;;### (autoloads nil nil ("js3-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; js3-mode-autoloads.el ends here
