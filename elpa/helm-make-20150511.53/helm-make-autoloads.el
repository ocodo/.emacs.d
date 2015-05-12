;;; helm-make-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "helm-make" "helm-make.el" (21841 22661 999401
;;;;;;  0))
;;; Generated autoloads from helm-make.el

(autoload 'helm-make "helm-make" "\
Call \"make -j ARG target\". Target is selected with completion.

\(fn &optional ARG)" t nil)

(autoload 'helm-make-projectile "helm-make" "\
Call `helm-make' for `projectile-project-root'.
ARG specifies the number of cores.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; helm-make-autoloads.el ends here
