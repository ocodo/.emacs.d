;;; helm-make-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "helm-make" "helm-make.el" (21642 20343 63401
;;;;;;  0))
;;; Generated autoloads from helm-make.el

(autoload 'helm-make "helm-make" "\
Use `helm' to select a Makefile target and `compile'.
If makefile is specified use it as path to Makefile

\(fn &optional MAKEFILE)" t nil)

(autoload 'helm-make-projectile "helm-make" "\
Call `helm-make' for `projectile-project-root'.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; helm-make-autoloads.el ends here
