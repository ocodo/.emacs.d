;;; j-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "j-console" "j-console.el" (23138 48312 485803
;;;;;;  799000))
;;; Generated autoloads from j-console.el

(autoload 'j-console "j-console" "\
Ensures a running j-console-cmd session and switches focus to
the containing buffer

\(fn)" t nil)

;;;***

;;;### (autoloads nil "j-help" "j-help.el" (23138 48312 477803 785000))
;;; Generated autoloads from j-help.el

(autoload 'j-help-lookup-symbol "j-help" "\
Lookup symbol in dictionary

\(fn SYMBOL)" t nil)

(autoload 'j-help-lookup-symbol-at-point "j-help" "\
Determine the symbol nearest to POINT and look it up in the dictionary

\(fn POINT)" t nil)

;;;***

;;;### (autoloads nil "j-mode" "j-mode.el" (23138 48312 481803 792000))
;;; Generated autoloads from j-mode.el

(autoload 'j-mode "j-mode" "\
Major mode for editing J

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode))

;;;***

;;;### (autoloads nil nil ("j-font-lock.el" "j-mode-pkg.el") (23138
;;;;;;  48312 485803 799000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; j-mode-autoloads.el ends here
