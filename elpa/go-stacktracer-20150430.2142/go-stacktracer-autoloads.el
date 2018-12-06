;;; go-stacktracer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "go-stacktracer" "go-stacktracer.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from go-stacktracer.el

(autoload 'go-stacktracer-region "go-stacktracer" "\
Parse a Go stacktrace from START to END.

\(fn START END)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "go-stacktracer" '("go-stacktracer-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; go-stacktracer-autoloads.el ends here
