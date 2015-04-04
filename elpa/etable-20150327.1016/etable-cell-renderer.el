;;; etable-cell-renderer.el --- Rendering routines for cells in ETable columns.

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 04 Dec 2013
;; Keywords: convenience
;; URL: https://github.com/Fuco1/ETable

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of ETable.

;; ETable is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ETable is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ETable  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains classes that render various types of data, such
;; as strings, dates or file sizes.

;;; Code:


;;; cell-renderer interface
(defclass etable-cell-renderer ()
  ()
  :abstract t)

(defmethod etable-draw-cell ((this etable-cell-renderer) table value is-selected has-focus row col)
  (error "Not implemented yet"))


;;; default cell-renderer rendering everything as string representation
(defclass etable-string-cell-renderer (etable-cell-renderer eieio-singleton)
  ())

(defmethod etable-draw-cell ((this etable-string-cell-renderer) table value is-selected has-focus row col)
  (format "%s" value))


;;; cell renderer that quotes the string representation.
;;; This mostly serves as an example of how to extend simpler
;;; renderers with some decorations
(defclass etable-quoted-string-cell-renderer (etable-string-cell-renderer eieio-singleton)
  ())

(defmethod etable-draw-cell ((this etable-quoted-string-cell-renderer) table value is-selected has-focus row col)
  (concat "\"" (call-next-method) "\""))


;;; cell renderer rendering dates
(defclass etable-date-cell-renderer (etable-cell-renderer eieio-singleton)
  ((date-format :initarg :date-format
                :initform "%b %e %R"
                :documentation "Format according to which the date is formatted.")))

(defmethod etable-draw-cell ((this etable-date-cell-renderer) table value is-selected has-focus row col)
  (format-time-string (etable-this date-format) value))


;;; cell renderer rendering file sizes, like -h switch in ls
(defclass etable-file-size-cell-renderer (etable-cell-renderer eieio-singleton)
  ((human-readable :initarg :human-readable
                   :initform t
                   :documentation "If non-nil, format text in human-readable form (like -h output of `ls').")))

(defun etable-format-file-size (file-size human-readable)
  (if (or (not human-readable)
          (< file-size 1024))
      (format (if (floatp file-size) " %.0f" " %d") file-size)
    (cl-do ((file-size (/ file-size 1024.0) (/ file-size 1024.0))
            ;; kilo, mega, giga, tera, peta, exa
            (post-fixes (list "k" "M" "G" "T" "P" "E") (cdr post-fixes)))
        ((< file-size 1024) (format " %.0f%s" file-size (car post-fixes))))))

(defmethod etable-draw-cell ((this etable-file-size-cell-renderer) table value is-selected has-focus row col)
  (etable-format-file-size value (etable-this human-readable)))


(provide 'etable-cell-renderer)
;;; etable-cell-renderer.el ends here
