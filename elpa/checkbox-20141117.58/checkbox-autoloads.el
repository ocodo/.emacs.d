;;; checkbox-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "checkbox" "checkbox.el" (0 0 0 0))
;;; Generated autoloads from checkbox.el

(autoload 'checkbox-toggle "checkbox" "\
Toggle checkbox (\"[ ]\" or \"[x]\") on the current line.
If checkbox does not exist, an empty checkbox will be inserted
before the first word constituent.
In programming modes, checkboxes will be inserted in comments.
With prefix ARG, delete checkbox.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "checkbox" '("checkbox-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; checkbox-autoloads.el ends here
