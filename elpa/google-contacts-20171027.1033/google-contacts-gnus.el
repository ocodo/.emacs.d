;;; google-contacts-gnus.el --- Support for Google Contacts in Gnus

;; Copyright © 2011 Julien Danjou <julien@danjou.info>

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This allows you to press a key in any Gnus message to jump to a Google
;; Contacts entry matching the email address, or the name of the sender.
;;

;;; Code:

(require 'gnus-art)

(defun google-contacts-gnus-get-name-email ()
  "Get name and email address from Gnus message."
  (when (gnus-alive-p)
    (gnus-with-article-headers
      (mail-extract-address-components
       (or (mail-fetch-field "From") "")))))

(defun google-contacts-gnus-article-from-goto ()
  "Go to contact in the From address of current Gnus message."
  (interactive)
  (let ((from (google-contacts-gnus-get-name-email)))
    (when from
      (let ((name (car from))
            (email (cadr from)))
        (or (google-contacts email)
            (google-contacts name))))))

(eval-after-load "gnus"
  '(define-key gnus-summary-mode-map ";" 'google-contacts-gnus-article-from-goto))

(provide 'google-contacts-gnus)
