;;; eno-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eno" "eno.el" (0 0 0 0))
;;; Generated autoloads from eno.el

(autoload 'eno "eno" "\
show matching regexp with hints then return the beginning and end of the selected hint(overlay).

\(fn RE &optional AT-HEAD ASIDE)" nil nil)

(autoload 'eno-word-goto "eno" "\


\(fn)" t nil)

(autoload 'eno-word-cut "eno" "\


\(fn)" t nil)

(autoload 'eno-word-copy "eno" "\


\(fn)" t nil)

(autoload 'eno-word-paste "eno" "\


\(fn)" t nil)

(autoload 'eno-symbol-goto "eno" "\


\(fn)" t nil)

(autoload 'eno-symbol-cut "eno" "\


\(fn)" t nil)

(autoload 'eno-symbol-copy "eno" "\


\(fn)" t nil)

(autoload 'eno-symbol-paste "eno" "\


\(fn)" t nil)

(autoload 'eno-str-goto "eno" "\


\(fn)" t nil)

(autoload 'eno-str-cut "eno" "\


\(fn)" t nil)

(autoload 'eno-str-copy "eno" "\


\(fn)" t nil)

(autoload 'eno-str-paste "eno" "\


\(fn)" t nil)

(autoload 'eno-line-goto "eno" "\


\(fn)" t nil)

(autoload 'eno-line-cut "eno" "\


\(fn)" t nil)

(autoload 'eno-line-copy "eno" "\


\(fn)" t nil)

(autoload 'eno-line-paste "eno" "\


\(fn)" t nil)

(autoload 'eno-line-comment "eno" "\


\(fn)" t nil)

(autoload 'eno-line-return "eno" "\
simulate return at line end

\(fn)" t nil)

(autoload 'eno-paren-goto "eno" "\


\(fn)" t nil)

(autoload 'eno-paren-copy "eno" "\


\(fn)" t nil)

(autoload 'eno-paren-cut "eno" "\


\(fn)" t nil)

(autoload 'eno-paren-delete "eno" "\


\(fn)" t nil)

(autoload 'eno-paren-paste "eno" "\


\(fn)" t nil)

(autoload 'eno-line-comment-to "eno" "\


\(fn)" t nil)

(autoload 'eno-line-copy-to "eno" "\


\(fn)" t nil)

(autoload 'eno-line-cut-to "eno" "\


\(fn)" t nil)

(autoload 'eno-line-delete-to "eno" "\


\(fn)" t nil)

(autoload 'eno-line-paste-to "eno" "\


\(fn)" t nil)

(autoload 'eno-symbol-copy-to "eno" "\


\(fn)" t nil)

(autoload 'eno-symbol-cut-to "eno" "\


\(fn)" t nil)

(autoload 'eno-symbol-delete-to "eno" "\


\(fn)" t nil)

(autoload 'eno-symbol-paste-to "eno" "\


\(fn)" t nil)

(autoload 'eno-word-goto-inline "eno" "\


\(fn)" t nil)

(autoload 'eno-word-copy-to-inline "eno" "\


\(fn)" t nil)

(autoload 'eno-word-cut-to-inline "eno" "\


\(fn)" t nil)

(autoload 'eno-word-paste-to-inline "eno" "\


\(fn)" t nil)

(autoload 'eno-line-comment-from-to "eno" "\


\(fn)" t nil)

(autoload 'eno-line-copy-from-to "eno" "\


\(fn)" t nil)

(autoload 'eno-line-cut-from-to "eno" "\


\(fn)" t nil)

(autoload 'eno-line-delete-from-to "eno" "\


\(fn)" t nil)

(autoload 'eno-line-paste-from-to "eno" "\


\(fn)" t nil)

(autoload 'eno-symbol-copy-from-to "eno" "\


\(fn)" t nil)

(autoload 'eno-symbol-cut-from-to "eno" "\


\(fn)" t nil)

(autoload 'eno-symbol-delete-from-to "eno" "\


\(fn)" t nil)

(autoload 'eno-symbol-paste-from-to "eno" "\


\(fn)" t nil)

(autoload 'eno-word-swap "eno" "\


\(fn)" t nil)

(autoload 'eno-symbol-swap "eno" "\


\(fn)" t nil)

(autoload 'eno-url-open "eno" "\


\(fn)" t nil)

(autoload 'eno-clear-overlay "eno" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eno" '("eno-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eno-autoloads.el ends here
