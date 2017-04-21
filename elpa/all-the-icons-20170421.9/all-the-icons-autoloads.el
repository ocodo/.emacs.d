;;; all-the-icons-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "all-the-icons" "all-the-icons.el" (0 0 0 0))
;;; Generated autoloads from all-the-icons.el

(autoload 'all-the-icons-install-fonts "all-the-icons" "\
Helper function to download and install the latests fonts based on OS.
When PFX is non-nil, ignore the prompt and just install

\(fn &optional PFX)" t nil)

(autoload 'all-the-icons-insert "all-the-icons" "\
Interactive icon insertion function.
When Prefix ARG is non-nil, insert the propertized icon.
When FAMILY is non-nil, limit the candidates to the icon set matching it.

\(fn &optional ARG FAMILY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "all-the-icons" '("material" "wicon" "octicon" "fileicon" "faicon" "all" "define-icon")))

;;;***

;;;### (autoloads nil nil ("all-the-icons-faces.el" "all-the-icons-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; all-the-icons-autoloads.el ends here
