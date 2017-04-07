;;; kotlin-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "kotlin-mode" "kotlin-mode.el" (0 0 0 0))
;;; Generated autoloads from kotlin-mode.el

(autoload 'kotlin-mode "kotlin-mode" "\
Major mode for editing Kotlin.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode) t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kotlin-mode" '("kotlin-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; kotlin-mode-autoloads.el ends here
