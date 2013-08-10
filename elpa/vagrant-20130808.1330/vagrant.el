;;; vagrant.el --- Manage a vagrant box from emacs

;; Version: 20130808.1330
;;; X-Original-Version: 0.5.0
;;; Author: Robert Crim <rob@servermilk.com>
;;; Url: https://github.com/ottbot/vagrant.el
;;; Keywords: vagrant chef
;;; Created: 08 August 2013

;;; Commentary:

;; This package lets you send vagrant commands while working within a
;; project containing a Vagrantfile.
;;
;; It will traverse the directory tree until a Vagrantfile is found
;; and assume this is the box you want to work with. It can be handy
;; to bring a box up, (re)provision, or even ssh to without leaving
;; emacs.
;;
;; The emacs command `vagrant-up` will run `vagrant up` in a shell,
;; other commands follow the pattern `vagrant-X` emacs command runs
;; `vagrant X` in the shell. An exception is vagrant-edit, which will
;; open the Vagrantfile for editing.

;;; Code:

;;;###autoload
(defun vagrant-up ()
  "Bring up the vagrant box."
  (interactive)
  (vagrant-command "vagrant up"))

;;;###autoload
(defun vagrant-provision ()
  "Provision the vagrant box."
  (interactive)
  (vagrant-command "vagrant provision"))

;;;###autoload
(defun vagrant-destroy ()
  "Destroy the vagrant box."
  (interactive)
  (vagrant-command "vagrant destroy"))

;;;###autoload
(defun vagrant-reload ()
  "Reload the vagrant box."
  (interactive)
  (vagrant-command "vagrant reload"))

;;;###autoload
(defun vagrant-resume ()
  "Resume the vagrant box."
  (interactive)
  (vagrant-command "vagrant resume"))

;;;###autoload
(defun vagrant-ssh ()
  "SSH to the vagrant box."
  (interactive)
  (vagrant-command "vagrant ssh"))

;;;###autoload
(defun vagrant-status ()
  "Show the vagrant box status."
  (interactive)
  (vagrant-command "vagrant status"))

;;;###autoload
(defun vagrant-suspend ()
  "Suspend the vagrant box."
  (interactive)
  (vagrant-command "vagrant suspend"))

;;;###autoload
(defun vagrant-halt ()
  "Halt the vagrant box."
  (interactive)
  (vagrant-command "vagrant halt"))

;;;###autoload
(defun vagrant-edit ()
  "Edit the Vagrantfile."
  (interactive)
  (find-file (vagrant-locate-vagrantfile)))


(defun vagrant-locate-vagrantfile (&optional dir)
  "Find Vagrantfile for DIR."
  (or (locate-dominating-file (or dir default-directory) "Vagrantfile")
      (error "No Vagrantfile found in %s or any parent directory" dir)))

(defun vagrant-command (cmd)
  "Run the vagrant command CMD in an async buffer."
  (let ((default-directory (file-name-directory (vagrant-locate-vagrantfile))))
    (async-shell-command cmd "*Vagrant*")))

(provide 'vagrant)

;;; vagrant.el ends here
