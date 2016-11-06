;;; etable-selection-model.el --- Class representing row selection in ETable.

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 22 Dec 2013
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

;; This file contains definition of class representing row selection
;; in ETable.  The row selection is represented in terms of visible
;; rows of the table, not the model.  Therefore, to get the rows of
;; model represented by this selection, a conversion is necessary.
;; The order of rows in view depends on factors such as filters and
;; ordering.

;;; Code:

(require 'interval-list)


;;; selection-model interface
(defclass etable-selection-model ()
  ()
  :abstract t)

(defmethod etable-add-selection-interval ((this etable-selection-model) start end)
  (error "Not implemented yet"))

(defmethod etable-clear-selection ((this etable-selection-model))
  (error "Not implemented yet"))

(defmethod etable-get-min-selection-index ((this etable-selection-model))
  (error "Not implemented yet"))

(defmethod etable-get-max-selection-index ((this etable-selection-model))
  (error "Not implemented yet"))

(defmethod etable-selected-index-p ((this etable-selection-model) index)
  (error "Not implemented yet"))

(defmethod etable-selection-empty-p ((this etable-selection-model))
  (error "Not implemented yet"))

(defmethod etable-remove-selection-interval ((this etable-selection-model) start end)
  (error "Not implemented yet"))

(defmethod etable-set-selection-interval ((this etable-selection-model) start end)
  (error "Not implemented yet"))


;;; default implementation using interval list
(defclass etable-default-selection-model (etable-selection-model)
  ((selection :initarg :selection
              :initform nil
              :protection :private
              :documentation "Interval list representing the table selection.")))

(defmethod etable-add-selection-interval ((this etable-default-selection-model) start end)
  (etable-mutate selection (intlist-add-interval this-slot start end)))

(defmethod etable-clear-selection ((this etable-default-selection-model))
  (etable-this selection nil))

(defmethod etable-get-min-selection-index ((this etable-default-selection-model))
  (caar (etable-this selection)))

(defmethod etable-get-max-selection-index ((this etable-default-selection-model))
  (cdr (-last-item (etable-this selection))))

(defmethod etable-selected-index-p ((this etable-default-selection-model) index)
  (let (selected)
    (intlist-loop (i (etable-this selection))
      (unless selected
        (setq selected (= i index))))
    selected))

(defmethod etable-selection-empty-p ((this etable-default-selection-model))
  (null (etable-this selection)))

(defmethod etable-remove-selection-interval ((this etable-default-selection-model) start end)
  (etable-mutate selection (intlist-remove-interval this-slot start end)))

(defmethod etable-set-selection-interval ((this etable-default-selection-model) start end)
  (etable-this selection (list (cons start end))))


(provide 'etable-selection-model)
;;; etable-selection-model.el ends here
