;;; docean-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "docean" "docean.el" (23344 45077 857261 145000))
;;; Generated autoloads from docean.el

(autoload 'docean-reboot-droplet "docean" "\
Reboot a droplet identified by ID.

\(fn ID)" t nil)

(autoload 'docean-power-cicle-droplet "docean" "\
Power cicle a droplet identified by ID.

\(fn ID)" t nil)

(autoload 'docean-shutdown-droplet "docean" "\
Shutdown a droplet identified by ID.

\(fn ID)" t nil)

(autoload 'docean-power-off-droplet "docean" "\
Power off a droplet identified by ID.

\(fn ID)" t nil)

(autoload 'docean-power-on-droplet "docean" "\
Reboot a droplet identified by ID.

\(fn ID)" t nil)

(autoload 'docean-password-reset-droplet "docean" "\
Reset password a droplet identified by ID.

\(fn ID)" t nil)

(autoload 'docean-enable-ipv6-droplet "docean" "\
Enable ipv6 to a droplet identified by ID.

\(fn ID)" t nil)

(autoload 'docean-disable-backups-droplet "docean" "\
Disable backups a droplet identified by ID.

\(fn ID)" t nil)

(autoload 'docean-enable-private-networking-droplet "docean" "\
Enable private networking a droplet identified by ID.

\(fn ID)" t nil)

(autoload 'docean-rename-droplet "docean" "\
Rename a droplet with ID and NAME.

\(fn ID &optional NAME)" t nil)

(autoload 'docean-snapshot-droplet "docean" "\
Snapshot a droplet with ID and NAME.

\(fn ID &optional NAME)" t nil)

(autoload 'docean-droplet-list "docean" "\
Show user droplets.

\(fn)" t nil)

(autoload 'docean-refetch-droplets "docean" "\
Refetch droplets information.

\(fn)" t nil)

(autoload 'docean-action-list "docean" "\
Show user actions.

\(fn)" t nil)

(autoload 'docean-refetch-actions "docean" "\
Refetch actions information.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; docean-autoloads.el ends here
