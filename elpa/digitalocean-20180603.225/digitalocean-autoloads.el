;;; digitalocean-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "digitalocean" "digitalocean.el" (23360 14623
;;;;;;  752154 930000))
;;; Generated autoloads from digitalocean.el

(autoload 'digitalocean-droplet-open-shell "digitalocean" "\
Open a shell for selected droplet.

\(fn)" t nil)

(autoload 'digitalocean-droplet-snapshot "digitalocean" "\
Create a snapshot of the selected droplet.

\(fn)" t nil)

(autoload 'digitalocean-droplet-restart "digitalocean" "\
Restart the selected droplet.

\(fn)" t nil)

(autoload 'digitalocean-droplet-shutdown "digitalocean" "\
Shutdown the selected droplet.

\(fn)" t nil)

(autoload 'digitalocean-droplet-startup "digitalocean" "\
Start the selected droplet.

\(fn)" t nil)
 (autoload 'digitalocean-droplet-destroy)

(autoload 'digitalocean-droplet-simple-create "digitalocean" "\
Create a droplet quickly using minimum inputs.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; digitalocean-autoloads.el ends here
