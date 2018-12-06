;;; replace-pairs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "replace-pairs" "replace-pairs.el" (0 0 0 0))
;;; Generated autoloads from replace-pairs.el

(autoload 'query-replace-pairs "replace-pairs" "\
Query-replace pairs of things

For example replace `(' and `)' with `[' and `]' respectively.

Interface is identical to `query-replace'.

\(fn FROM-ITEM TO-ITEM DELIMITED START END BACKWARD)" t nil)

(autoload 'replace-pairs "replace-pairs" "\
Replace pairs of things

For example replace `(' and `)' with `[' and `]' respectively.

Interface is identical to `replace-string'.

\(fn FROM-ITEM TO-ITEM &optional DELIMITED START END BACKWARD)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "replace-pairs" '("replace-pairs-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; replace-pairs-autoloads.el ends here
