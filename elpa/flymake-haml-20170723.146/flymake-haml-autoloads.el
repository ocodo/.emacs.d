;;; flymake-haml-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flymake-haml" "flymake-haml.el" (0 0 0 0))
;;; Generated autoloads from flymake-haml.el

(autoload 'flymake-haml-load "flymake-haml" "\
Configure flymake mode to check the current buffer's haml syntax.

This function is designed to be called in `haml-mode-hook'; it
does not alter flymake's global configuration, so function
`flymake-mode' alone will not suffice.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flymake-haml" '("flymake-haml-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flymake-haml-autoloads.el ends here
