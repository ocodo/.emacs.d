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
(require 'request)

(defcustom docker-api-connection-url "unix:///var/run/docker.sock"
  "Docker connection url."
  :group 'docker-api)

(defun docker-api-connection-process-components (url)
  "Parse URL and extract `make-network-process' family, host & service components."
  (let* ((components (url-generic-parse-url url))
         (is-local (string-equal "unix" (url-type components))))
    (if is-local
        (list 'local nil (url-filename components))
      (list 'ipv4 (url-host components) (url-port components)))))

(defun docker-api-handle-response (&rest data)
  "Helper that checks if DATA contains errors for `docker-api-http-request'."
  (let* ((response (plist-get data :response))
         (data (request-response-data response)))
    (when (request-response-error-thrown response)
      (error data))
    data))

(defun docker-api-http-request (method path)
  "Make a docker HTTP request using METHOD at PATH."
  (let* ((components (url-generic-parse-url docker-api-connection-url))
         (is-local (string-equal "unix" (url-type components)))
         (http-data))
    (request (if is-local (format "http:%s" path) docker-api-connection-url)
             :unix-socket (when is-local (url-filename components))
             :type (upcase (symbol-name method))
             :parser 'buffer-string
             :sync t
             :complete (lambda (&rest data) (setq http-data (apply #'docker-api-handle-response data))))
    http-data))

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
