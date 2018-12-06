;;; docker-api.el --- Emacs interface to the Docker API

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/docker-api.el
;; Version: 0.0.1
;; Package-Requires: ((dash "2.12.1") (request "0.2.0") (s "1.11.0"))

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

(defgroup docker-api nil
  "Customization group for docker-api."
  :group 'docker
  :group 'convenience)

(require 'docker-api-connection)
(require 'docker-api-images)
(require 'docker-api-containers)
(require 'docker-api-networks)
(require 'docker-api-volumes)

(provide 'docker-api)

;;; docker-api.el ends here
