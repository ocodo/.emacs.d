;;; pt-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "pt" "pt.el" (22628 53780 0 0))
;;; Generated autoloads from pt.el

(autoload 'pt-regexp "pt" "\
Run a pt search with REGEXP rooted at DIRECTORY.

\(fn REGEXP DIRECTORY &optional ARGS)" t nil)

(autoload 'pt-regexp-file-pattern "pt" "\
Run a pt search with REGEXP rooted at DIRECTORY with FILE-FILTER.

\(fn REGEXP DIRECTORY PATTERN)" t nil)

(autoload 'projectile-pt "pt" "\
Run a pt search with REGEXP rooted at the current projectile project root.

\(fn REGEXP)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; pt-autoloads.el ends here
