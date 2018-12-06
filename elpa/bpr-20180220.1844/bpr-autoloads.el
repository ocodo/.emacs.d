;;; bpr-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bpr" "bpr.el" (0 0 0 0))
;;; Generated autoloads from bpr.el

(autoload 'bpr-spawn "bpr" "\
Executes string CMD asynchronously in background.

\(fn CMD)" t nil)

(autoload 'bpr-open-last-buffer "bpr" "\
Opens the buffer of the last spawned process.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bpr" '("bpr-")))

;;;***

;;;### (autoloads nil "test-bpr" "test-bpr.el" (0 0 0 0))
;;; Generated autoloads from test-bpr.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "test-bpr" '("with-fake-buffer")))

;;;***

;;;### (autoloads nil nil ("bpr-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bpr-autoloads.el ends here
