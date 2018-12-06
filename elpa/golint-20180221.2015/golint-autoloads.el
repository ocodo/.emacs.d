;;; golint-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "golint" "golint.el" (0 0 0 0))
;;; Generated autoloads from golint.el

(autoload 'golint "golint" "\
Run golint on the current file and populate the fix list.
Pressing \"C-x `\" jumps directly to the line in your code which
caused the first message.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "golint" '("go-lint-buffer-name" "golint-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; golint-autoloads.el ends here
