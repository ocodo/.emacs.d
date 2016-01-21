;;; vdirel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "vdirel" "vdirel.el" (22176 24816 0 0))
;;; Generated autoloads from vdirel.el

(autoload 'vdirel-vdirsyncer-sync-server "vdirel" "\
Ask vdirsyncer to sync REPOSITORY with the server.
You probably want to call `vdirel-refresh-cache' right after
this.  Currently, REPOSITORY is ignored and \"vdirsyncer sync\" is called
without further argument.

\(fn &optional REPOSITORY)" t nil)

(autoload 'vdirel-helm-select-email "vdirel" "\
Let user choose an email address from (REFRESH'ed) REPOSITORY.

\(fn &optional REFRESH REPOSITORY)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; vdirel-autoloads.el ends here
