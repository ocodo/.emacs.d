;;; academic-phrases-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "academic-phrases" "academic-phrases.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from academic-phrases.el

(autoload 'academic-phrases "academic-phrases" "\
Insert a phrase from a list of academic phrases by topic.

\(fn)" t nil)

(autoload 'academic-phrases-by-section "academic-phrases" "\
Insert a phrase from a list of academic phrases by the paper section.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "academic-phrases" '("academic-phrases--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; academic-phrases-autoloads.el ends here
