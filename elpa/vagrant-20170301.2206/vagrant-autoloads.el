;;; vagrant-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vagrant" "vagrant.el" (0 0 0 0))
;;; Generated autoloads from vagrant.el

(autoload 'vagrant-up "vagrant" "\
Bring up the vagrant box.

\(fn)" t nil)

(autoload 'vagrant-provision "vagrant" "\
Provision the vagrant box.

\(fn)" t nil)

(autoload 'vagrant-destroy "vagrant" "\
Destroy the vagrant box.

\(fn)" t nil)

(autoload 'vagrant-reload "vagrant" "\
Reload the vagrant box.

\(fn)" t nil)

(autoload 'vagrant-resume "vagrant" "\
Resume the vagrant box.

\(fn)" t nil)

(autoload 'vagrant-ssh "vagrant" "\
SSH to the vagrant box.

\(fn)" t nil)

(autoload 'vagrant-status "vagrant" "\
Show the vagrant box status.

\(fn)" t nil)

(autoload 'vagrant-suspend "vagrant" "\
Suspend the vagrant box.

\(fn)" t nil)

(autoload 'vagrant-halt "vagrant" "\
Halt the vagrant box.

\(fn)" t nil)

(autoload 'vagrant-rsync "vagrant" "\
Rsync the vagrant box.

\(fn)" t nil)

(autoload 'vagrant-edit "vagrant" "\
Edit the Vagrantfile.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vagrant" '("vagrant-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vagrant-autoloads.el ends here
