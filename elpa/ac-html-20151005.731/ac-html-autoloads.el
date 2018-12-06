;;; ac-html-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ac-haml" "ac-haml.el" (0 0 0 0))
;;; Generated autoloads from ac-haml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ac-haml" '("ac-haml-")))

;;;***

;;;### (autoloads nil "ac-html" "ac-html.el" (0 0 0 0))
;;; Generated autoloads from ac-html.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ac-html" '("ac-html-")))

;;;***

;;;### (autoloads nil "ac-html-core" "ac-html-core.el" (0 0 0 0))
;;; Generated autoloads from ac-html-core.el

(autoload 'ac-html-define-data-provider "ac-html-core" "\
Define ac-html data provider with this macro.
This macro is buggy and cannot be used now.

\(fn PROVIDER &rest PAIRS)" nil t)

(function-put 'ac-html-define-data-provider 'lisp-indent-function '1)

(autoload 'ac-html-enable-data-provider "ac-html-core" "\
Enable data provider PROVIDER.

\(fn PROVIDER)" nil nil)

(autoload 'ac-html-define-ac-source "ac-html-core" "\
Define ac-html lang with this macro.

\(fn LANG &rest PAIRS)" nil t)

(function-put 'ac-html-define-ac-source 'lisp-indent-function '1)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ac-html-core" '("ac-html-")))

;;;***

;;;### (autoloads nil "ac-html-default-data-provider" "ac-html-default-data-provider.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ac-html-default-data-provider.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ac-html-default-data-provider" '("ac-html-" "web-completion-data-")))

;;;***

;;;### (autoloads nil "ac-html-testing-data-provider" "ac-html-testing-data-provider.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ac-html-testing-data-provider.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ac-html-testing-data-provider" '("ac-html-testing-")))

;;;***

;;;### (autoloads nil "ac-jade" "ac-jade.el" (0 0 0 0))
;;; Generated autoloads from ac-jade.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ac-jade" '("ac-jade-")))

;;;***

;;;### (autoloads nil "ac-slim" "ac-slim.el" (0 0 0 0))
;;; Generated autoloads from ac-slim.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ac-slim" '("ac-slim-")))

;;;***

;;;### (autoloads nil nil ("ac-html-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ac-html-autoloads.el ends here
