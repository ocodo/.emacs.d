;;; nginx-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (nginx-mode) "nginx-mode" "nginx-mode.el" (21304
;;;;;;  53651 0 0))
;;; Generated autoloads from nginx-mode.el

(autoload 'nginx-mode "nginx-mode" "\
Major mode for highlighting nginx config files.

The variable nginx-indent-level controls the amount of indentation.
\\{nginx-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("nginx.conf$" . nginx-mode) '("/etc/nginx/.*" . nginx-mode))

;;;***

;;;### (autoloads nil nil ("nginx-mode-pkg.el") (21304 53651 156771
;;;;;;  0))

;;;***

(provide 'nginx-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nginx-mode-autoloads.el ends here
