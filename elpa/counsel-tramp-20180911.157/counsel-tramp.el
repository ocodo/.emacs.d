;;; counsel-tramp.el --- Tramp ivy interface for ssh, docker, vagrant -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 by Masashı Mıyaura

;; Author: Masashı Mıyaura
;; URL: https://github.com/masasam/emacs-counsel-tramp
;; Package-Version: 20180911.157
;; Version: 0.5.2
;; Package-Requires: ((emacs "24.3") (counsel "0.10"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; counsel-tramp provides interfaces of Tramp
;; You can also use tramp with counsel interface as root
;; If you use it with docker-tramp, you can also use docker with counsel interface
;; If you use it with vagrant-tramp, you can also use vagrant with counsel interface

;;; Code:

(require 'counsel)
(require 'tramp)
(require 'cl-lib)

(defgroup counsel-tramp nil
  "Tramp with ivy interface for ssh, docker, vagrant"
  :group 'counsel)

(defcustom counsel-tramp-docker-user nil
  "If you want to use login user name when `docker-tramp' used, set variable."
  :group 'counsel-tramp
  :type 'string)

(defcustom counsel-tramp-localhost-directory "/"
  "Initial directory when connecting with /sudo:root@localhost:."
  :group 'counsel-tramp
  :type 'string)

(defcustom counsel-tramp-pre-command-hook nil
  "Hook run before `counsel-tramp'.
The hook is called with one argument that is non-nil."
  :type 'hook)

(defcustom counsel-tramp-post-command-hook nil
  "Hook run after `counsel-tramp'.
The hook is called with one argument that is non-nil."
  :type 'hook)

(defcustom counsel-tramp-quit-hook nil
  "Hook run when `counsel-tramp-quit'.
The hook is called with one argument that is non-nil."
  :type 'hook)

(defcustom counsel-tramp-custom-connections '()
  "A list to manually add extra connections.
E.g.: '(\"/ssh:domain|sudo:user@localhost:/\")."
  :type 'string)

(defun counsel-tramp-quit ()
  "Quit counsel-tramp.
Kill all remote buffers."
  (interactive)
  (run-hooks 'counsel-tramp-quit-hook)
  (tramp-cleanup-all-buffers))

(defun counsel-tramp--candidates ()
  "Collect candidates for counsel-tramp."
  (let ((source (split-string
                 (with-temp-buffer
                   (insert-file-contents "~/.ssh/config")
                   (buffer-string))
                 "\n"))
        (hosts counsel-tramp-custom-connections))
    (dolist (host source)
      (when (string-match "[H\\|h]ost +\\(.+?\\)$" host)
	(setq host (match-string 1 host))
	(if (string-match "[ \t\n\r]+\\'" host)
	    (replace-match "" t t host))
	(if (string-match "\\`[ \t\n\r]+" host)
	    (replace-match "" t t host))
        (unless (string= host "*")
	  (if (string-match "[ ]+" host)
	      (let ((result (split-string host " ")))
		(while result
		  (push
		   (concat "/" tramp-default-method ":" (car result) ":")
		   hosts)
		  (push
		   (concat "/ssh:" (car result) "|sudo:root@" (car result) ":/")
		   hosts)
		  (pop result)))
	    (push
	     (concat "/" tramp-default-method ":" host ":")
	     hosts)
	    (push
	     (concat "/ssh:" host "|sudo:root@" host ":/")
	     hosts)))))
    (when (require 'docker-tramp nil t)
      (cl-loop for line in (cdr (ignore-errors (apply #'process-lines "docker" (list "ps"))))
	       for info = (reverse (split-string line "[[:space:]]+" t))
	       collect (progn (push
			       (concat "/docker:" (car info) ":/")
			       hosts)
			      (when counsel-tramp-docker-user
				(if (listp counsel-tramp-docker-user)
				    (let ((docker-user counsel-tramp-docker-user))
				      (while docker-user
					(push
					 (concat "/docker:" (car docker-user) "@" (car info) ":/")
					 hosts)
					(pop docker-user)))
				  (push
				   (concat "/docker:" counsel-tramp-docker-user "@" (car info) ":/")
				   hosts))))))
    (when (require 'vagrant-tramp nil t)
      (cl-loop for box-name in (map 'list 'cadr (vagrant-tramp--completions))
	       do (progn
		    (push (concat "/vagrant:" box-name ":/") hosts)
		    (push (concat "/vagrant:" box-name "|sudo:root@" box-name ":/") hosts))))
    (push (concat "/sudo:root@localhost:" counsel-tramp-localhost-directory) hosts)
    (reverse hosts)))

;;;###autoload
(defun counsel-tramp ()
  "Open your ~/.ssh/config with counsel interface.
You can connect your server with tramp"
  (interactive)
  (unless (file-exists-p "~/.ssh/config")
    (error "There is no ~/.ssh/config"))
  (when (require 'docker-tramp nil t)
    (unless (executable-find "docker")
      (error "'docker' is not installed")))
  (when (require 'vagrant-tramp nil t)
    (unless (executable-find "vagrant")
      (error "'vagrant' is not installed")))
  (run-hooks 'counsel-tramp-pre-command-hook)
  (counsel-find-file (ivy-read "Tramp: " (counsel-tramp--candidates)))
  (run-hooks 'counsel-tramp-post-command-hook))

(provide 'counsel-tramp)

;;; counsel-tramp.el ends here
