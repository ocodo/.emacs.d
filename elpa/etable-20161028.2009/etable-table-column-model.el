;;; etable-table-column-model.el --- Model representing columns in ETable.

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

;; This file contains definitions of table column model.  This holds a
;; list of columns the table displays, provides methods to add, remove
;; and reorder columns and stores rendering and navigation information
;; such as goal columns or margins between columns.

;;; Code:


;;; table-column-model interface
(defclass etable-table-column-model ()
  ()
  :abstract t)

(defmethod etable-add-column ((this etable-table-column-model) column)
  (error "Not implemented yet"))

(defmethod etable-move-column ((this etable-table-column-model) index-from index-to)
  (error "Not implemented yet"))

(defmethod etable-remove-column ((this etable-table-column-model) index)
  (error "Not implemented yet"))

(defmethod etable-get-column ((this etable-table-column-model) index)
  (error "Not implemented yet"))

(defmethod etable-get-column-width ((this etable-table-column-model) index)
  (error "Not implemented yet"))

(defmethod etable-get-columns ((this etable-table-column-model))
  (error "Not implemented yet"))

(defmethod etable-get-column-count ((this etable-table-column-model))
  (error "Not implemented yet"))

(defmethod etable-get-column-margin ((this etable-table-column-model))
  (error "Not implemented yet"))

(defmethod etable-set-column-margin ((this etable-table-column-model) margin)
  (error "Not implemented yet"))

(defmethod etable-get-total-column-width ((this etable-table-column-model))
  (error "Not implemented yet"))

(defmethod etable-get-goal-column ((this etable-table-column-model))
  (error "Not implemented yet"))

(defmethod etable-set-goal-column ((this etable-table-column-model) goal-col)
  (error "Not implemented yet"))


;;; default table-column-model implementation
(defclass etable-default-table-column-model (etable-table-column-model)
  ((column-list :initarg :column-list
                :initform []
                :type vector
                :protection :private
                :documentation "Array of `etable-table-column' objects in this model.")
   ;; (selection-model)
   (column-margin :initarg :column-margin
                  :initform 1
                  :type integer
                  :protection :private
                  :documentation "Margin between each column.")
   (goal-column :initarg :goal-column
                :initform 0
                :documentation "Default goal column used when navigating rows up and down.")))

(defmethod etable-add-column ((this etable-default-table-column-model) column)
  (etable-mutate column-list (vconcat this-slot (vector column))))

(defmethod etable-move-column ((this etable-default-table-column-model) index-from index-to)
  (etable-mutate column-list
    (let ((from (elt this-slot index-from))
          (to (elt this-slot index-to)))
      (setf (elt this-slot index-to) from)
      (setf (elt this-slot index-from) to)
      this-slot)))

(defmethod etable-remove-column ((this etable-default-table-column-model) index)
  (let ((modified (vconcat (-remove-at index (append (etable-this column-list) nil)) nil)))
    (etable-this column-list modified)))

(defmethod etable-get-column ((this etable-default-table-column-model) index)
  (elt (etable-this column-list) index))

(defmethod etable-get-column-width ((this etable-table-column-model) index)
  (etable-get-width (elt (etable-this column-list) index)))

(defmethod etable-get-columns ((this etable-default-table-column-model))
  (etable-this column-list))

(defmethod etable-get-column-count ((this etable-default-table-column-model))
  (length (etable-this column-list)))

(defmethod etable-get-column-margin ((this etable-default-table-column-model))
  (etable-this column-margin))

(defmethod etable-set-column-margin ((this etable-default-table-column-model) margin)
  (etable-this column-margin margin))

(defmethod etable-get-total-column-width ((this etable-default-table-column-model))
  (let* ((col-num (etable-get-column-count this))
         (margins (* (1- col-num) (etable-get-column-margin this))))
    (apply '+ margins (mapcar (lambda (x) (etable-get-width x)) (etable-this column-list)))))

(defmethod etable-get-goal-column ((this etable-default-table-column-model))
  (etable-this goal-column))

(defmethod etable-set-goal-column ((this etable-default-table-column-model) goal-col)
  (etable-this goal-column goal-col))


(provide 'etable-table-column-model)
;;; etable-table-column-model.el ends here
