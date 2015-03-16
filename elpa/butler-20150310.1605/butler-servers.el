;; -*- lexical-binding: t -*-
;;; butler-servers.el --- Code to deal with Jenkins Servers

;; Copyright © 2012-2013 Ashton Kemerling
;;
;; Author: Ashton Kemerling <ashtonkemerling@gmail.com>
;; URL: http://www.github.com/AshtonKem/Butler.git
;; Keywords: Jenkins, Hudson, CI

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Deals with storing & parsing Butler servers


;;; Code:


(defvar butler-hash (make-hash-table :test #'equal))
(defvar butler-server-list nil)

(defun parse-authinfo-file (filename servername)
  (if (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (search-forward (concat "machine " servername))
        (let* ((line-start (line-beginning-position))
               (line-end (line-end-position))
               (line (buffer-substring line-start line-end))
               (splitted (split-string line " "))
               (filtered (delq "" splitted))
               (username (car (cdr (member "login" filtered))))
               (password (car (cdr (member "password" filtered)))))
          (if (and username password)
              (generate-basic-auth username password))))))

(defun generate-basic-auth (username password)
  (replace-regexp-in-string "\n" ""
                            (concat "Basic "
                                    (base64-encode-string
                                     (concat username ":" password)))))

(defun prepare-servers ()
  (when (= 0 (hash-table-count butler-hash))
    (dolist (server butler-server-list)
      (let* ((name (car (cdr server)))
             (args (cdr (cdr server)))
             (url (cdr (assoc 'server-address args)))
             (username (cdr (assoc 'server-user args)))
             (password (cdr (assoc 'server-password args)))
             (auth-file (cdr (assoc 'auth-file args)))
             (auth-string (if auth-file
                              (parse-authinfo-file auth-file name)
                            (generate-basic-auth username password)))
             (server-hash (make-hash-table :test #'equal)))
        (puthash 'name name server-hash)
        (puthash 'username username server-hash)
        (puthash 'auth auth-string  server-hash)
        (puthash 'url url server-hash)
        (puthash 'jobs (make-hash-table :test #'equal) server-hash)
        (puthash name
                 server-hash
                 butler-hash)))))

(defun get-server (name)
  (prepare-servers)
  (gethash name butler-hash))

(defun get-job (server name)
  (gethash name (gethash 'jobs server)))


(provide 'butler-servers)

;; butler-servers.el ends here
