;;; digitalocean-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "digitalocean" "digitalocean.el" (0 0 0 0))
;;; Generated autoloads from digitalocean.el

(autoload 'digitalocean-droplet-open-shell "digitalocean" "\
Open a shell for selected droplet." t nil)

(autoload 'digitalocean-droplet-snapshot "digitalocean" "\
Create a snapshot of the selected droplet." t nil)

(autoload 'digitalocean-droplet-restart "digitalocean" "\
Restart the selected droplet." t nil)

(autoload 'digitalocean-droplet-shutdown "digitalocean" "\
Shutdown the selected droplet." t nil)

(autoload 'digitalocean-droplet-startup "digitalocean" "\
Start the selected droplet." t nil)

(autoload 'digitalocean-droplet-destroy "digitalocean" "\
Destroy the selected droplet." t nil)

(autoload 'digitalocean-droplet-simple-create "digitalocean" "\
Create a droplet quickly using minimum inputs." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "digitalocean" '("digitalocean-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; digitalocean-autoloads.el ends here
