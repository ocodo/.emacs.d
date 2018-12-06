;;; format-sql-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "format-sql" "format-sql.el" (0 0 0 0))
;;; Generated autoloads from format-sql.el

(autoload 'format-sql-region "format-sql" "\
Uses the \"format-sql\" tool to reformat the current region.

\(fn)" t nil)

(autoload 'format-sql-buffer "format-sql" "\
Uses the \"format-sql\" tool to reformat the current buffer.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "format-sql" '("format-sql-" "get-file-type")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; format-sql-autoloads.el ends here
