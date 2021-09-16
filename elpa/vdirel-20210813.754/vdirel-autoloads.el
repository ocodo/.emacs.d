;;; vdirel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vdirel" "vdirel.el" (0 0 0 0))
;;; Generated autoloads from vdirel.el

(autoload 'vdirel-switch-repository "vdirel" "\
Change current vdir folder to REPOSITORY.
Let the user choose a repository from `vdirel-repositories` and set
`vdirel-repository` accordingly.

\(fn REPOSITORY)" t nil)

(autoload 'vdirel-vdirsyncer-sync-server "vdirel" "\
Ask vdirsyncer to sync REPOSITORY with the server.
You probably want to call `vdirel-refresh-cache' right after
this.  Currently, REPOSITORY is ignored and \"vdirsyncer sync\" is called
without further argument.

\(fn &optional REPOSITORY)" t nil)

(autoload 'vdirel-helm-select-email "vdirel" "\
Let user choose an email address from (REFRESH'ed) REPOSITORY.

\(fn &optional REFRESH REPOSITORY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vdirel" '("vdirel-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vdirel-autoloads.el ends here
