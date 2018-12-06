;;; etable.el --- Implementation of javax.swing.JTable for Emacs.

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


;; For a basic overview, see github readme at
;; https://github.com/Fuco1/ETable

;; For the complete documentation visit the documentation wiki located
;; at https://github.com/Fuco1/ETable/wiki

;; If you like this project, you can donate here:
;; https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=CEYP5YVHDRX8C

;;; Code:

(require 'eieio)
(require 'eieio-base)

(require 'dash)

(defface etable-row-unmarked-face
  '((t (:inherit highlight)))
  "The unmarked rows face."
  :group 'etable)

(defface etable-row-marked-face
  '((t (:inherit warning)))
  "The marked rows face."
  :group 'etable)


;;; helper macros
(defmacro etable-this (slot &optional value)
  "Get the value of SLOT in `this' instance.

If VALUE is non-nil, set the value of SLOT instead.  To set the
SLOT to nil, specify :nil as the VALUE."
  (if value
      (if (eq value :nil)
          `(oset this ,slot nil)
        `(oset this ,slot ,value))
    `(oref this ,slot)))

(defmacro etable-mutate (slot form &rest forms)
  "Mutate the SLOT in this object using FORM.

The SLOTs value is captured with variable `this-slot'."
  (declare (indent 1))
  `(let ((this-slot (etable-this ,slot)))
     (oset this ,slot (progn
                        ,form
                        ,@forms))))

(defun etable-aref (data idx &rest indices)
  (let ((e (aref data idx)))
    (cl-loop for i in indices do (setq e (aref e i)))
    e))

(defun etable-list-to-vector (list)
  "Transform tabular (2D) data in LIST into nested vectors"
  (vconcat (--map (vconcat it nil) list) nil))

(defmacro etable-save-table-excursion (table &rest forms)
  (declare (indent 1)
           (debug (symbolp body)))
  (let ((tbl (make-symbol "table")))
    `(let ((,tbl ,table))
       (if (etable-has-focus ,tbl)
           (let ((tpos (etable-get-selected-cell-position ,tbl)))
             (unwind-protect
                 (progn
                   ,@forms)
               (etable-goto-cell-position ,tbl tpos)))
         (save-excursion
           ,@forms)))))

;;; etable view implementation
(defclass etable ()
  ((table-model :initarg :table-model
                :type etable-table-model
                :protection :private
                :accessor etable-get-table-model
                :writer etable-set-table-model
                :documentation "Table model for this table.")
   (column-model :initarg :column-model
                 :type etable-table-column-model
                 :protection :private
                 :accessor etable-get-column-model
                 :writer etable-set-column-model
                 :documentation "Column model for this table.")
   (selection-model :initarg :selection-model
                    :type etable-selection-model
                    :protection :private
                    :accessor etable-get-selection-model
                    :documentation "Selection model for this table.")
   (overlay :initform nil
            :protection :private
            :accessor etable-get-overlay
            :writer etable-set-overlay
            :documentation "Overlay keeping track of bounds of this table.")))

(defvar etable-table-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'etable-next-row)
    (define-key map (kbd "p") 'etable-previous-row)
    (define-key map (kbd "m") 'etable-mark-row)
    (define-key map (kbd "u") 'etable-unmark-row)
    (define-key map (kbd "U") 'etable-unmark-all)
    (define-key map (kbd "t") 'etable-toggle-marks)
    (define-key map (kbd "g") 'etable-revert)
    map)
  "Keymap used inside a table.")

(defun etable-at-point (property &optional point)
  "Get PROPERTY at POINT."
  (overlay-get (car (overlays-at (or point (point)))) property))

(defun etable-next-row (&optional arg)
  (interactive "p")
  (let* ((table (etable-at-point 'etable))
         (cur-cell (etable-get-selected-cell-position table))
         (goal-col (or (etable-get-goal-column (etable-get-column-model table)) (plist-get cur-cell :col)))
         (goal-col-align (etable-get-align (etable-get-column (etable-get-column-model table) goal-col)))
         (new-cell (list :row (+ (plist-get cur-cell :row) (or arg 1))
                         :col goal-col
                         :offset (cond
                                  ((eq goal-col-align :left)
                                   999999)
                                  ((eq goal-col-align :right)
                                   0)
                                  (t (plist-get cur-cell :offset))))))
    (etable-goto-cell-position table new-cell)))

(defun etable-previous-row (&optional arg)
  (interactive "p")
  (etable-next-row (- (or arg 1))))

(defun etable-mark-row (&optional arg)
  (interactive "p")
  (let* ((table (etable-at-point 'etable))
         (cur-cel (etable-get-selected-cell-position table))
         (selection (etable-get-selection-model table)))
    (etable-add-selection-interval selection (plist-get cur-cel :row) (plist-get cur-cel :row))
    (etable-next-row)
    (etable-update table)))

(defun etable-unmark-row (&optional arg)
  (interactive "p")
  (let* ((table (etable-at-point 'etable))
         (cur-cel (etable-get-selected-cell-position table))
         (selection (etable-get-selection-model table)))
    (etable-remove-selection-interval selection (plist-get cur-cel :row) (plist-get cur-cel :row))
    (etable-next-row)
    (etable-update table)))

(defun etable-unmark-all (&optional arg)
  (interactive "p")
  (let* ((table (etable-at-point 'etable))
         (selection (etable-get-selection-model table)))
    (etable-remove-selection-interval selection (etable-get-min-selection-index selection) (etable-get-max-selection-index selection))
    (etable-update table)))

(defun etable-toggle-marks (&optional arg)
  (interactive "p")
  (let* ((table (etable-at-point 'etable))
         (selection (etable-get-selection-model table)))
    (dotimes (i (etable-get-row-count (etable-get-table-model table)))
      (if (etable-selected-index-p selection i)
          (etable-remove-selection-interval selection i i)
        (etable-add-selection-interval selection i i)))
    (etable-update table)))

(defun etable-revert ()
  (interactive)
  (let* ((table (etable-at-point 'etable)))
    (etable-update table)))

(defun etable-create-table (tbl-model &optional clmn-model)
  (setq tbl-model
        (cond
         ((and (object-p tbl-model)
               (object-of-class-p tbl-model 'etable-table-model))
          etable-table-model)
         ((listp tbl-model)
          (etable-default-table-model
           "TableModel"
           :table-data (etable-list-to-vector tbl-model)))
         ((vectorp tbl-model)
          (etable-default-table-model
           "TableModel"
           :table-data tbl-model))))
  (setq
   clmn-model
   (or clmn-model
       (let ((width (etable-get-column-count tbl-model)))
         (etable-default-table-column-model
          "TableColumnModel"
          :column-list (vconcat (cl-loop for i from 1 to width collect
                                         (etable-table-column "TableColumn"
                                                              :model-index (1- i))) nil)))))
  (etable "Table"
          :table-model tbl-model
          :column-model clmn-model
          :selection-model (etable-default-selection-model "SelectionModel")))

(defmethod etable-narrow-to-table ((this etable))
  (let ((ov (etable-this overlay)))
    (narrow-to-region (overlay-start ov) (overlay-end ov))))

(defmethod etable-get-column-positions ((this etable))
  (let* ((col-model (etable-this column-model))
         (col-list (etable-get-columns col-model))
         (col-margin (etable-get-column-margin col-model)))
    (cl-loop for col in (append col-list nil)
             for s = (etable-get-width col)
             then (+ s (etable-get-width col) col-margin)
             collect s)))

(defmethod etable-get-selected-cell-position ((this etable))
  (when (etable-has-focus this)
    (save-excursion
      (save-restriction
        (widen)
        (etable-narrow-to-table this)
        (let* ((line (line-number-at-pos))
               (col-positions (etable-get-column-positions this))
               (col (current-column))
               (col-and-offset (cl-loop for c in col-positions for i = 0 then (incf i) until (< col c)
                                        finally return (cons i (- c col)))))
          (list :row (1- line)
                :col (car col-and-offset)
                :offset (cdr col-and-offset)))))))

(defmethod etable-goto-cell-position ((this etable) tpos)
  (save-restriction
    (widen)
    (etable-narrow-to-table this)
    (goto-char (point-min))
    (forward-line (plist-get tpos :row))
    (beginning-of-line)
    (let ((col (plist-get tpos :col)))
      (forward-char (- (nth col (etable-get-column-positions this))
                       (min (etable-get-column-width (etable-this column-model) col) (plist-get tpos :offset)))))))

(defmethod etable-has-focus ((this etable))
  (let ((pos (point))
        (start (overlay-start (etable-this overlay)))
        (end (overlay-end (etable-this overlay))))
    (and (>= pos start)
         (<= pos end))))

(defmethod etable-draw ((this etable) point)
  (goto-char point)
  (-when-let* ((ov (etable-this overlay))
               (start (overlay-start ov))
               (end (overlay-end ov)))
    (let ((inhibit-read-only t))
      (remove-text-properties start end '(read-only)))
    (delete-region start end)
    (delete-overlay ov)
    (etable-this overlay :nil))
  (let ((ov (make-overlay (point) (point) nil nil t)))
    (overlay-put ov 'etable this)
    (overlay-put ov 'face 'etable-row-unmarked-face)
    (overlay-put ov 'local-map etable-table-keymap)
    (overlay-put ov 'priority 1)
    (etable-this overlay ov))
  (etable-update this))

(defmethod etable-update ((this etable))
  (let* ((ov (etable-this overlay))
         (model (etable-this table-model))
         (col-model (etable-this column-model))
         (selection (etable-this selection-model))
         (col-separator (make-string (etable-get-column-margin col-model) ? ))
         (inhibit-read-only t))
    (remove-text-properties (overlay-start ov) (overlay-end ov) '(read-only))
    (etable-save-table-excursion this
      (delete-region (overlay-start ov) (overlay-end ov))
      (goto-char (overlay-start ov))
      (cl-loop for i from 0 to (1- (etable-get-row-count model)) do
               (cl-loop for j from 0 to (1- (etable-get-column-count col-model)) do
                        (let* ((col (etable-get-column col-model j))
                               (width (etable-get-width col))
                               (align (etable-get-align col))
                               (string (etable-draw-cell
                                        (etable-get-renderer col)
                                        this
                                        (etable-get-value-at model i (etable-get-model-index col))
                                        nil nil i j)))
                          (when (> (length string) width)
                            (setq string (concat (substring string 0 (- width 3)) "...")))
                          (let ((extra (- width (length string))))
                            (cond
                             ((eq align :left)
                              (setq string (concat string (make-string extra ? ))))
                             ((eq align :right)
                              (setq string (concat (make-string extra ? ) string)))
                             ((eq align :center)
                              (setq string (concat (make-string (/ (1+ extra) 2) ? ) string (make-string (/ extra 2) ? ))))))
                          (insert string))
                        (insert col-separator))
               (when (etable-selected-index-p selection i)
                 (add-text-properties (line-beginning-position)
                                      (line-end-position)
                                      '(face etable-row-marked-face)))
               (insert "\n"))

      (delete-char -1))
    ;; TODO: this removes a possibility of having a table next to some
    ;; other text.
    (add-text-properties (overlay-start ov) (overlay-end ov)
                         '(read-only "You can't edit a table directly"
                                     front-sticky (read-only)))))

(defmethod etable-remove ((this etable))
  (let ((ov (etable-this overlay))
        (inhibit-read-only t))
    (remove-text-properties (overlay-start ov) (overlay-end ov) '(read-only))
    (delete-region (overlay-start ov) (overlay-end ov))
    (delete-overlay ov)
    (etable-this overlay :nil)))

(require 'etable-table-model)
(require 'etable-table-column)
(require 'etable-table-column-model)
(require 'etable-cell-renderer)
(require 'etable-selection-model)


(provide 'etable)
;;; etable.el ends here
