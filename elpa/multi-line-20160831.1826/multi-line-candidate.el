;;; multi-line-candidate.el --- multi-line statements -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Ivan Malison

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

;; multi-line-candidate defines a wrapper object for a marker with some
;; additional context

;;; Code:

(require 'eieio)

(defclass multi-line-candidate ()
  ((marker :initarg :marker :initform (point-marker))
   (original-spacing :initarg :original-spacing :initform nil)))

(defmethod multi-line-candidate-position ((candidate multi-line-candidate))
  (marker-position (oref candidate marker)))

(provide 'multi-line-candidate)
;;; multi-line-candidate.el ends here
