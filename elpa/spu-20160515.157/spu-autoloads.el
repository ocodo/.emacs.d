;;; spu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "spu" "spu.el" (0 0 0 0))
;;; Generated autoloads from spu.el

(autoload 'spu-package-upgrade "spu" "\
Upgrade package.

\(fn &optional PREFIX)" t nil)

(autoload 'spu-package-upgrade-daily "spu" "\
Upgrade package daily.  Don't call it by any interactive way.

\(fn)" nil nil)

(autoload 'spu-view-upgrade-log "spu" "\
Open the last package upgrade log.
With PREFIX, open the directory containing the upgrade logs.

\(fn PREFIX)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "spu" '("spu-")))

;;;***

;;;### (autoloads nil "spu-dark" "spu-dark.el" (0 0 0 0))
;;; Generated autoloads from spu-dark.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "spu-dark" '("spu-")))

;;;***

;;;### (autoloads nil nil ("spu-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; spu-autoloads.el ends here
