;;; helm-lastpass.el --- Helm interface of LastPass  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; URL: https://github.com/xuchunyang/helm-lastpass
;; Package-Version: 20170914.142
;; Package-Requires: ((emacs "24.1") (helm-core "2.8.1") (csv "2.1"))
;; Keywords: LastPass
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; About:
;;
;; This package uses lpass(1) (the LastPass command line tool) to let you access
;; your LastPass with Helm.

;; Prerequisites:
;;
;; - lpass(1), see https://github.com/lastpass/lastpass-cli

;; Installation:
;;
;; [![MELPA](https://melpa.org/packages/helm-lastpass-badge.svg)](https://melpa.org/#/helm-lastpass)
;;
;; This package is available from MELPA, thus you can easily install it with
;; `package.el'. If you decide to install it manually, you are on your own
;; (Don't forget the dependencies, i.e., helm and csv).

;; Usage:
;;
;; - `M-x helm-lastpass'

;;; Code:

(require 'csv)
(require 'helm)

(defgroup helm-lastpass nil
  "Helm interface of LastPass."
  :group 'tools)

(defcustom helm-lastpass-cli "lpass"
  "The program name of the LastPass command line tool."
  :type 'string
  :group 'helm-lastpass)

(defun helm-lastpass-cli ()
  (or (executable-find helm-lastpass-cli)
      (error "Error: `lpass' is not found, please install it first")))

(defun helm-lastpass-logged-in-p ()
  (zerop (call-process (helm-lastpass-cli) nil nil nil "status")))

(defun helm-lastpass-login (&optional email password)
  (let* ((email
          (or email (read-string "Email: " user-mail-address)))
         (password
          (or password (read-passwd "Password: ")))
         (command
          ;; XXX Is there any better solution?
          (format "echo -n '%s' | LPASS_DISABLE_PINENTRY=1 %s login --color=never %s"
                  password
                  (shell-quote-argument (helm-lastpass-cli))
                  email)))
    (with-temp-buffer
      (if (zerop (call-process-shell-command command nil t nil))
          (message "Success: Logged in as %s" email)
        (error "%s" (buffer-string))))))

(defun helm-lastpass-export (&optional sync)
  "Return a list of alist which contains all account information."
  (let ((sync (pcase sync
                ('nil   "--sync=auto")
                ('auto "--sync=auto")
                ('now  "--sync=now")
                ('no   "--sync=no")
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
                            ","))))
    (with-temp-buffer
      (if (zerop (call-process (helm-lastpass-cli) nil t nil "export" "--color=never" sync fields))
          (csv-parse-buffer t)
        (error "%s" (buffer-string))))))

;;;###autoload
(defun helm-lastpass ()
  "Helm interface of LastPass."
  (interactive)
  (unless (helm-lastpass-logged-in-p)
    (helm-lastpass-login))
  (helm :sources
        (helm-build-sync-source "LastPass"
          :candidates
          (lambda ()
            (mapcar (lambda (item)
                      (cons (cdr (assoc "fullname" item))
                            item))
                    (helm-lastpass-export)))
          :action
          '(("Visit site" .
             (lambda (candidate)
               (browse-url (cdr (assoc "url" candidate)))))
            ("Copy username" .
             (lambda (candidate)
               (let ((username (cdr (assoc "username" candidate))))
                 (unless (string= "" username)
                   (kill-new username)
                   (message "Copied: %s" username)))))
            ("Copy password" .
             (lambda (candidate)
               (let ((password (cdr (assoc "password" candidate))))
                 (unless (string= "" password)
                   (kill-new password)
                   (message "Copied: %s" password)))))
            ("Copy URL" .
             (lambda (candidate)
               (let ((url (cdr (assoc "url" candidate))))
                 (kill-new url)
                 (message "Copied: %s" url))))))
        :buffer "*helm LastPass*"))

(provide 'helm-lastpass)
;;; helm-lastpass.el ends here
