;;; google-oauth.el --- Utility functions and variables for Google OAuth

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
;; This file defines some commonly used variable and functions to access
;; OAuth based services.
;;

;;; Code:

(defconst google-oauth-auth-url "https://accounts.google.com/o/oauth2/auth"
  "Google OAuth2 server URL.")

(defconst google-oauth-token-url "https://accounts.google.com/o/oauth2/token"
  "Google OAuth2 server URL.")

;;;###autoload
(defun google-oauth-auth (resource-url client-id client-secret)
  "Request access to a resource."
  (oauth2-auth google-oauth-auth-url google-oauth-token-url client-id client-secret resource-url))

;;;###autoload
(defun google-oauth-auth-and-store (resource-url client-id client-secret)
  "Request access to a Google resource and store it using `auth-source'."
  (oauth2-auth-and-store google-oauth-auth-url google-oauth-token-url
                         resource-url client-id client-secret))

(provide 'google-oauth)
