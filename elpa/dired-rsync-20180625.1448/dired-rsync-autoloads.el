;;; dired-rsync-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "dired-rsync" "dired-rsync.el" (23360 14621
;;;;;;  744107 112000))
;;; Generated autoloads from dired-rsync.el

(autoload 'dired-rsync "dired-rsync" "\
Asynchronously copy files in dired to DEST using rsync.

This function runs the copy asynchronously so Emacs won't block whilst
the copy is running.  It also handles both source and destinations on
ssh/scp tramp connections.

\(fn DEST)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dired-rsync-autoloads.el ends here
