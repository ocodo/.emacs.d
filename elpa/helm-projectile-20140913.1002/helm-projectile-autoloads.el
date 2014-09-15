;;; helm-projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "helm-projectile" "helm-projectile.el" (21526
;;;;;;  16686 907797 0))
;;; Generated autoloads from helm-projectile.el

(autoload 'helm-projectile "helm-projectile" "\
Use projectile with Helm instead of ido.

With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(eval-after-load 'projectile '(progn (define-key projectile-command-map (kbd "h") 'helm-projectile) (define-key projectile-command-map (kbd "H") 'helm-projectile-switch-project)))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; helm-projectile-autoloads.el ends here
