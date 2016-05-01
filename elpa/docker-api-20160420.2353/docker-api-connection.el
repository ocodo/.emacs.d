;;; docker-api-connection.el --- Emacs interface to the Docker API

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 's)
(require 'dash)
(require 'json)

(defcustom docker-api-connection-url "unix:///var/run/docker.sock"
  "Docker connection url."
  :group 'docker-api)

(defcustom docker-api-connection-process-name "docker-http"
  "Docker connection process name."
  :group 'docker-api)

(defcustom docker-api-connection-process-buffer "*docker-http*"
  "Docker connection process buffer name."
  :group 'docker-api)

(defvar docker-api-connection--http-data nil
  "Temporary buffer holding HTTP data.")

(defvar docker-api-connection--request-finished nil
  "Temporary boolean flag holding wether the request is finished or not.")

(defun docker-api-connection-process-filter(process string)
  "Append received data to `docker-api-connection--http-data'."
  (setq docker-api-connection--http-data (concat docker-api-connection--http-data string)))

(defun docker-api-connection-process-sentinel (process event)
  "Notify that all data was received with `docker-api-connection--request-finished'."
  (when (memq (process-status process) '(closed exit signal))
    (setq docker-api-connection--request-finished t)))

(defun docker-api-connection-process-components (url)
  "Parse URL and extract `make-network-process' family, host & service components."
  (let* ((components (url-generic-parse-url url))
         (is-local (string-equal "unix" (url-type components))))
    (if is-local
        (list 'local nil (url-filename components))
      (list 'ipv4 (url-host components) (url-port components)))))

(defun docker-api-make-connection-process ()
  "Create the docker connection process."
  (-let [(family host service) (docker-api-connection-process-components docker-api-connection-url)]
    (make-network-process
     :name     docker-api-connection-process-name
     :buffer   docker-api-connection-process-buffer
     :family   family
     :host     host
     :service  service
     :filter   #'docker-api-connection-process-filter
     :sentinel #'docker-api-connection-process-sentinel)))

(defun docker-api-http-request (method path)
  "Make a docker HTTP request using METHOD at PATH."
  (let ((request (format "%s %s HTTP/1.0\r\n\r\n" (upcase (symbol-name method)) path))
        (docker-api-connection--http-data nil)
        (docker-api-connection--request-finished nil)
        (process (docker-api-make-connection-process)))
    (process-send-string process request)
    (while (not docker-api-connection--request-finished)
      (accept-process-output process 1))
    (let* ((index (s-index-of "\r\n\r\n" docker-api-connection--http-data))
           (headers (substring docker-api-connection--http-data 0 index))
           (data (substring docker-api-connection--http-data (+ index 4))))
      data)))

(defun docker-api-json-request (method path)
  "Make a docker HTTP request using METHOD at PATH, with results parsed as json.

See `docker-api-http-request'."
  (let ((json (docker-api-http-request method path))
        (json-array-type 'list))
    (json-read-from-string json)))

(defmacro docker-api-with-connection (url &rest body)
  "Execute BODY with `docker-api-connection-url' set to URL."
  `(let ((docker-api-connection-url ,url))
     ,@body))

(put 'docker-api-with-connection 'lisp-indent-function 'defun)

(provide 'docker-api-connection)

;;; docker-api-connection.el ends here
