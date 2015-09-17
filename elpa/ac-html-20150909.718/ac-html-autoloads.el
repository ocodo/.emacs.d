;;; ac-html-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ac-html-core" "ac-html-core.el" (22010 16227
;;;;;;  65140 0))
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

;;;***

;;;### (autoloads nil nil ("ac-haml.el" "ac-html-default-data-provider.el"
;;;;;;  "ac-html-pkg.el" "ac-html-testing-data-provider.el" "ac-html.el"
;;;;;;  "ac-jade.el" "ac-slim.el") (22010 16227 96316 266000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ac-html-autoloads.el ends here
