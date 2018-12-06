;;; docker-api-containers.el --- Emacs interface to the Docker API

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

(require 'docker-api-connection)

;; TODO add parameters: limit since before filters
(defun docker-api-containers (&optional all size)
  (let ((path (format "/containers/json?all=%d&size=%d" (if all 1 0) (if size 1 0))))
    (docker-api-json-request 'get path)))

(defun docker-api-container-start (id &optional detach-keys)
  (let ((path (format "/containers/%s/start" id)))
    (unless (null detach-key)
      (setq path (format "%s?detachKeys=%s" path detach-keys)))
    (docker-api-json-request 'post path)))

(defun docker-api-container-stop (id &optional timeout)
  (let ((path (format "/containers/%s/stop" id)))
    (unless (null timeout)
      (setq path (format "%s?t=%d" path timeout)))
    (docker-api-json-request 'post path)))

(defun docker-api-container-restart (id &optional timeout)
  (let ((path (format "/containers/%s/restart" id)))
    (unless (null timeout)
      (setq path (format "%s?t=%d" path timeout)))
    (docker-api-json-request 'post path)))

(defun docker-api-container-kill (id &optional signal)
  (let ((path (format "/containers/%s/kill" id)))
    (unless (null timeout)
      (setq path (format "%s?signal=%s" path signal)))
    (docker-api-json-request 'post path)))

(defun docker-api-container-rename (id name)
  (let ((path (format "/containers/%s/rename?name=%s" id name)))
    (docker-api-json-request 'post path)))

(defun docker-api-container-pause (id)
  (let ((path (format "/containers/%s/pause" id)))
    (docker-api-json-request 'post path)))

(defun docker-api-container-unpause (id)
  (let ((path (format "/containers/%s/unpause" id)))
    (docker-api-json-request 'post path)))

(defun docker-api-container-wait (id)
  (let ((path (format "/containers/%s/wait" id)))
    (docker-api-json-request 'post path)))

(defun docker-api-container-remove (id &optional force remove-volumes)
  (let ((path (format "/containers/%s?force=%d&v=%d" id (if force 1 0) (if remove-volumes 1 0))))
    (docker-api-json-request 'delete path)))

(provide 'docker-api-containers)

;;; docker-api-containers.el ends here
