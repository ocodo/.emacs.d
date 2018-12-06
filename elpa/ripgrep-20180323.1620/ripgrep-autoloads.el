;;; ripgrep-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ripgrep" "ripgrep.el" (0 0 0 0))
;;; Generated autoloads from ripgrep.el

(autoload 'ripgrep-regexp "ripgrep" "\
Run a ripgrep search with `REGEXP' rooted at `DIRECTORY'.
`ARGS' provides Ripgrep command line arguments.

\(fn REGEXP DIRECTORY &optional ARGS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ripgrep" '("ripgrep")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ripgrep-autoloads.el ends here
