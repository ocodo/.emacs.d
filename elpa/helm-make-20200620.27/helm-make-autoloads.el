;;; helm-make-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-make" "helm-make.el" (0 0 0 0))
;;; Generated autoloads from helm-make.el

(autoload 'helm-make "helm-make" "\
Call \"make -j ARG target\". Target is selected with completion.

\(fn &optional ARG)" t nil)

(autoload 'helm-make-reset-cache "helm-make" "\
Reset cache, see `helm-make-cache-targets'." t nil)

(autoload 'helm-make-projectile "helm-make" "\
Call `helm-make' for `projectile-project-root'.
ARG specifies the number of cores.

By default `helm-make-projectile' will look in `projectile-project-root'
followed by `projectile-project-root'/build, for a makefile.

You can specify an additional directory to search for a makefile by
setting the buffer local variable `helm-make-build-dir'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-make" '("helm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-make-autoloads.el ends here
