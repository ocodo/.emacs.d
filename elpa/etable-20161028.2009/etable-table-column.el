;;; etable-table-column.el --- Class representing one column in the ETable.

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

;; This file contains definition of class representing one column in a
;; table.  It holds information such as renderer for the class, its
;; presentation properties (width, align, margins), relation to data
;; model and cell and header renderers.

;;; Code:


;;; table-column implementation
(defclass etable-table-column ()
  ((width :initarg :width
          :initform 10
          :type integer
          :protection :private
          :documentation "Width of this column.")
   (align :initarg :align
          :initform :right
          :protection :private
          :documentation "Type of vertical alignment.")
   (model-index :initarg :model-index
                :protection :private
                :documentation "Index of model column this column represents in table view.")
   (renderer :initarg :renderer
             :initform (etable-string-cell-renderer "String cell renderer"))))

(defmethod etable-get-width ((this etable-table-column))
  (etable-this width))

(defmethod etable-set-width ((this etable-table-column) w)
  (etable-this width w))

(defmethod etable-get-align ((this etable-table-column))
  (etable-this align))

(defmethod etable-get-model-index ((this etable-table-column))
  (etable-this model-index))

(defmethod etable-set-model-index ((this etable-table-column) index)
  (etable-this model-index index))

(defmethod etable-get-renderer ((this etable-table-column))
  (etable-this renderer))

(defmethod etable-set-renderer ((this etable-table-column) new-renderer)
  (etable-this renderer new-renderer))


(provide 'etable-table-column)
;;; etable-table-column.el ends here
