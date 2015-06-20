;;; es-lib.el --- A collection of emacs utilities
;;; Version: 0.4
;;; Author: sabof
;;; URL: https://github.com/sabof/es-lib
;;; Package-Requires: ((cl-lib "0.3"))

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; The project is hosted at https://github.com/sabof/es-lib

;;; Code:

(require 'es-lib-core-macros)
(require 'es-lib-core-functions)
(require 'es-lib-text-navigate)
(require 'es-lib-buffer-local-set-key)
(require 'es-lib-lexical)
(require 'es-lib-total-line)

(provide 'es-lib)
;;; es-lib.el ends here
