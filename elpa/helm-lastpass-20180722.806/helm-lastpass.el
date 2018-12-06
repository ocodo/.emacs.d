;;; helm-lastpass.el --- Helm interface of LastPass  -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/helm-lastpass
;; Package-Requires: ((emacs "25.1") (helm "2.0") (csv "2.1"))
;; Package-Version: 20180722.806
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Helm interface of LastPass.

;;; Code:

(require 'csv)
(require 'helm)
(require 'auth-source)
(require 'cl-lib)
(require 'subr-x)

(defgroup helm-lastpass nil
  "Helm interface of LastPass."
  :group 'tools)

(defcustom helm-lastpass-cli "lpass"
  "The program name of the LastPass command line tool."
  :type 'string
  :group 'helm-lastpass)

(defcustom helm-lastpass-actions nil
  "Actions for `helm-lastpass'."
  :group 'helm-lastpass
  :type '(alist :key-type string :value-type function))

(defun helm-lastpass-copy-password (al)
  (let-alist al
    (if .password
        (progn
          (kill-new .password)
          (message "Copied: %s" .password))
      (message "No password for this entry"))))

(defun helm-lastpass-copy-username (al)
  (let-alist al
    (if .username
        (progn
          (kill-new .username)
          (message "Copied: %s" .username))
      (message "No username for this entry"))))

(defun helm-lastpass-copy-note (al)
  (let-alist al
    (if .extra
        (progn
          (kill-new .extra)
          (message "Copied: %s" .extra))
      (message "No note for this entry"))))

(defun helm-lastpass-browse-url (al)
  (let-alist al
    (if .url
        (browse-url .url)
      (message "No URL for this entry"))))

(defun helm-lastpass-cli ()
  (or (executable-find helm-lastpass-cli)
      (error "Error: `lpass' is not found, please install it first")))

(defun helm-lastpass-logged-in-p ()
  (zerop (call-process (helm-lastpass-cli) nil nil nil "status")))

(defun helm-lastpass-login (&optional email password)
  (let ((plist (car (auth-source-search :max 1 :host "lastpass.com"))))
    (cl-flet ((value (k) (let ((v (plist-get plist k)))
                           (if (functionp v) (funcall v) v))))
      (setq email (value :user)
            password (value :secret))))
  (let* ((email
          (or email (read-string "Email: " user-mail-address)))
         (password
          (or password (read-passwd "Password: ")))
         (infile (make-temp-file "helm-lastpass-password-")))
    (write-region password nil infile)
    (with-temp-buffer
      (message "helm-lastpass: Logging as %s..." email)
      (let ((process-environment
             (append
              '("LPASS_DISABLE_PINENTRY=1")
              process-environment)))
        (if (zerop (call-process (helm-lastpass-cli) infile t nil "login" "--color=never" email))
            (message "helm-lastpass: Logging as %s...done" email)
          (error "%s" (buffer-string)))))
    (delete-file infile)))

(defun helm-lastpass-export (&optional sync)
  "Run lpass export subcommand, SYNC is for --sync.
Return a list of alist which contain all account information."
  (let* ((sync (pcase sync
                 (`nil   "--sync=auto")
                 (`auto "--sync=auto")
                 (`now  "--sync=now")
                 (`no   "--sync=no")
                 (_     (error "Invalid argument '%s'" sync))))
         (fields (concat
                  "--fields="
                  (mapconcat #'identity
                             '("id"
                               "url"
                               "username"
                               "password"
                               "extra"
                               "name"
                               "fav"
                               "id"
                               "grouping"
                               "group"
                               "fullname"
                               "last_touch"
                               "last_modified_gmt"
                               "attachpresent")
                             ",")))
         (args (list "export" "--color=never" sync fields)))
    (with-temp-buffer
      (message "helm-lastpass: Retrieving data...")
      (if (zerop (apply #'call-process (helm-lastpass-cli) nil t nil args))
          (progn
            (message "helm-lastpass: Retrieving data...done")
            (let ((list-of-alist (csv-parse-buffer t)))
              (mapcar
               (lambda (alist)
                 (mapcar
                  (pcase-lambda (`(,field . ,value))
                    ;; Strip 
                    (when (and (string-suffix-p "" field)
                               (string-suffix-p "" value))
                      (setq field (substring field 0 -1)
                            value (substring value 0 -1)))
                    (when (string= "" value)
                      (setq value nil))
                    (cons (intern field) value))
                  alist))
               list-of-alist)))
        (error "%s" (buffer-string))))))

(defun helm-lastpass-candidates ()
  (mapcar
   (lambda (alist)
     (cons (alist-get 'fullname alist) alist))
   (helm-lastpass-export 'no)))

(defun helm-lastpass-action-transformer (actions candidate)
  (append
   (delq
    nil
    (let-alist candidate
      (list (and .password '("Copy Password" . helm-lastpass-copy-password))
            (and .username '("Copy Username" . helm-lastpass-copy-username))
            (and .extra '("Copy Notes" . helm-lastpass-copy-note))
            (and .url
                 ;; 'sn' stands for Secure Notes, I guess.
                 (not (string= .url "http://sn"))
                 '("Browse URL"    . helm-lastpass-browse-url)))))
   actions))

(defvar helm-lastpass-source
  (helm-build-sync-source "LastPass"
    :candidates #'helm-lastpass-candidates
    :action 'helm-lastpass-actions
    :action-transformer #'helm-lastpass-action-transformer)
  "Source for `helm-lastpass'.")

;;;###autoload
(defun helm-lastpass ()
  "Helm interface of LastPass."
  (interactive)
  (unless (helm-lastpass-logged-in-p)
    (helm-lastpass-login))
  (helm :sources helm-lastpass-source
        :buffer "*helm LastPass*"))

(provide 'helm-lastpass)
;;; helm-lastpass.el ends here
