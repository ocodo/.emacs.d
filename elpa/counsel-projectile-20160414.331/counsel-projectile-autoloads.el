;;; counsel-projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "counsel-projectile" "counsel-projectile.el"
;;;;;;  (22291 18949 151768 419000))
;;; Generated autoloads from counsel-projectile.el

(autoload 'counsel-projectile-find-file "counsel-projectile" "\
Jump to a project's file using completion.

Replacement for `projectile-find-file'.
With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(autoload 'counsel-projectile-find-dir "counsel-projectile" "\
Jump to a project's directory using completion.

With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(autoload 'counsel-projectile-switch-to-buffer "counsel-projectile" "\
Switch to a project buffer.

\(fn)" t nil)

(autoload 'counsel-projectile "counsel-projectile" "\
Use projectile with Ivy instead of ido.

With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; counsel-projectile-autoloads.el ends here
