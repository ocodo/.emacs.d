;;; google-org-contacts --- Google Contacts to org-contacts -*- lexical-binding: t -*-

;; Copyright (C) 2013 Rüdiger Sonderfeld

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; Created: 13 Sep 2013
;; Version: 1.0
;; Keywords: comm org
;; URL: http://julien.danjou.info/projects/emacs-packages#google-contacts

;; This file is NOT part of GNU Emacs.

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

;;; Commentary:

;; Converts Google Contacts into org-contacts.  There currently is no support
;; for sync or anything fancy.  Calling `google-contacts-to-org-contacts' will
;; simply dump all contacts into the current buffer.

;; TODO: Check if org-sync could be used at least to sync existing org entries.
;;       http://orgmode.org/worg/org-contrib/gsoc2012/student-projects/org-sync/

;;; Code:

(require 'google-contacts)
(require 'org)

(defun google-org-contacts--insert (entity contact attr-name)
  "Write ENTITY of CONTACT with ATTR-NAME at point in current buffer."
  (let ((data (cdr (assq entity contact))))
    (when (and data (not (and (stringp data) (string= data ""))))
      (insert (format ":%s: " attr-name))
      (cond
       ((stringp data)
        (insert data))
       ((listp data)
        (dolist (i data)
          (insert (format "%s " (cdr i))))
        (delete-char -1)))        ; delete trailing space
      (insert "\n"))))

;;;###autoload
(defun google-contacts-to-org-contacts (&optional buffer query-string)
  "Insert contacts in org-contacts format into BUFFER.
If QUERY-STRING is nil insert all contacts."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (google-contacts-async-api
   (or query-string "")
   #'(lambda (contacts buffer)
       (with-current-buffer buffer
         (dolist (contact contacts)
           (insert (format "* %s\n:PROPERTIES:\n" (cdr (assq 'fullname contact))))
           (google-org-contacts--insert 'emails contact "EMAIL")
           (google-org-contacts--insert 'phones contact "PHONE")
           (google-org-contacts--insert 'postal-addresses contact "ADDRESS")
           (google-org-contacts--insert 'nickname contact "NICKNAME")
           (google-org-contacts--insert 'websites contact "WEB")
           (when (cdr (assq 'birthday contact))
             (org-insert-time-stamp (org-read-date nil 'totime
                                                   (cdr (assq 'birthday contact)))
                                    nil nil
                                    ":BIRTHDAY: " "\n"))
           (insert ":END:\n"))))
   buffer)
  buffer)

(provide 'google-org-contacts)

;;; google-org-contacts.el ends here
