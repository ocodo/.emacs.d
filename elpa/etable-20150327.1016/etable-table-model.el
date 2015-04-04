;;; etable-table-model.el --- Data model for ETable.

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

;; This file contains definition of class representing a data model
;; for a table.  This allows us to have many independent views of the
;; same underlying data.  Data model models a rectangular "grid" data
;; with rows and columns, similar to content of a database.

;;; Code:

;; TODO: add listeners


;;; table-model interface
(defclass etable-table-model ()
  ()
  :abstract t)

(defmethod etable-get-column-count ((this etable-table-model))
  (error "Not implemented yet"))

(defmethod etable-get-row-count ((this etable-table-model))
  (error "Not implemented yet"))

(defmethod etable-get-value-at ((this etable-table-model) row col)
  (error "Not implemented yet"))

(defmethod etable-get-column-name ((this etable-table-model) col)
  (error "Not implemented yet"))

(defmethod etable-is-cell-editable ((this etable-table-model) row col)
  (error "Not implemented yet"))

(defmethod etable-set-value-at ((this etable-table-model) row col value)
  (error "Not implemented yet"))


;;; abstract table-model implementation
(defclass etable-abstract-table-model (etable-table-model)
  ()
  :abstract t)

(defun etable-convert-col-in-spreadsheet-header (col)
  "Convert the column index into spreadsheet column header.

These are: A, B, ..., Z, AA, ..., AZ, BA, ..."
  (let* ((base 26)
         (re (char-to-string (+ (mod col base) 65))))
    (setq col (1- (/ col base)))
    (while (>= col 0)
      (setq re (concat (char-to-string (+ (mod col base) 65)) re))
      (setq col (1- (/ col base))))
    re))

(defmethod etable-get-column-name ((this etable-abstract-table-model) col)
  (etable-convert-col-in-spreadsheet-header col))

(defmethod etable-is-cell-editable ((this etable-abstract-table-model) row col)
  nil)

(defmethod etable-set-value-at ((this etable-abstract-table-model) row col value)
  ())


;;; default table-model implementation using vectors
;; TODO: add methods to add/remove rows or columns
(defclass etable-default-table-model (etable-abstract-table-model)
  ((table-data :initarg :table-data
               :initform [[]]
               :type vector
               :protection :private
               :documentation "Two-dimensional vector holding the table model data")
   (column-ids :initarg :column-ids
               :initform []
               :type vector
               :protection :private
               :documentation "Vector of column identifiers.  These are used as default header names.")))


(defmethod etable-get-column-count ((this etable-default-table-model))
  (length (elt (etable-this table-data) 0)))

(defmethod etable-get-row-count ((this etable-default-table-model))
  (length (etable-this table-data)))

(defmethod etable-get-value-at ((this etable-default-table-model) row col)
  (etable-aref (etable-this table-data) row col))

(defmethod etable-get-column-name ((this etable-default-table-model) col)
  (--if-let (> (length (etable-this column-ids)) 0)
      (elt it col)
    (call-next-method)))

(defmethod etable-is-cell-editable ((this etable-default-table-model) row col)
  t)

(defmethod etable-set-value-at ((this etable-default-table-model) row col value)
  (setf (elt (elt (etable-this table-data) row) col) value))

(defmethod etable-add-row ((this etable-default-table-model) row-data)
  (cond
   ((listp row-data)
    (etable-mutate table-data (vconcat this-slot (vector (vconcat row-data nil)))))
   ((vectorp row-data)
    (etable-mutate table-data (vconcat this-slot (vector row-data))))))


(provide 'etable-table-model)
;;; etable-table-model.el ends here
