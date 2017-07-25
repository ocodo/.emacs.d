;;; ibuffer-projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ibuffer-projectile" "ibuffer-projectile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ibuffer-projectile.el

(autoload 'ibuffer-projectile-generate-filter-groups "ibuffer-projectile" "\
Create a set of ibuffer filter groups based on the projectile root dirs of buffers.

\(fn)" nil nil)

(autoload 'ibuffer-projectile-set-filter-groups "ibuffer-projectile" "\
Set the current filter groups to filter by vc root dir.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ibuffer-projectile" '("projectile-root" "ibuffer-projectile-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ibuffer-projectile-autoloads.el ends here
