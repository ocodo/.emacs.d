;;; flymake-json-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flymake-json" "flymake-json.el" (0 0 0 0))
;;; Generated autoloads from flymake-json.el

(autoload 'flymake-json-load "flymake-json" "\
Configure flymake mode to check the current buffer's javascript syntax.

\(fn)" t nil)

(autoload 'flymake-json-maybe-load "flymake-json" "\
Call `flymake-json-load' if this file appears to be json.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flymake-json" '("flymake-json-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flymake-json-autoloads.el ends here
