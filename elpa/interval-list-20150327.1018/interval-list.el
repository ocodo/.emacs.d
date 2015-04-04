;;; interval-list.el --- Interval list data structure for 1D selections

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 7 Dec 2013
;; Version: 0.1
;; Package-Version: 20150327.1018
;; Package-Requires: ((dash "2.4.0") (cl-lib "0.5") (emacs "24.4"))
;; Keywords: extensions, data structure
;; URL: https://github.com/Fuco1/interval-list

;; This file is not part of GNU Emacs.

;;; License:

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

;; This package implements a 1D selection model.  It provides methods
;; to add or remove intervals and merge or split the overlapping
;; portions automatically.  The intervals are modeled as a list of
;; cons (beg . end) ordered in ascending order.  An example interval
;; list is:
;;
;; ((1 . 4) (7 . 12) (15 . 17))
;;
;; Each interval represents all the integers inside it, including the
;; boundaries.  Therefore, a list like ((1 . 2) (3 . 4)) is invalid
;; and should be represented as ((1 . 4)) instead.  To model a single
;; point selection, you can use interval of zero length, for example
;; (5 . 5).  Two helper functions are provided to add or remove points
;; instead of intervals.
;;
;; This data structure is not meant to be used for querying, it is only
;; useful to represent the selection as a whole.  A helper macro is
;; provided to iterate over the selected indices.  Lookup is of course
;; possible, but inefficient.  For data structure designed for
;; interval and point lookup see package `interval-tree' instead.

;; See github readme at https://github.com/Fuco1/interval-list

;;; Code:

(require 'dash)
(require 'cl-lib)

(defun intlist-add-interval (intlist begin end)
  "Add interval between BEGIN and END inclusive into INTLIST.

The overlapping intervals are merged if needed.  This operation
runs in O(n) with respect to number of intervals in the list."
  (let* ((split (--split-with (<= (car it) begin) intlist))
         (splice (-concat (car split) (list (cons begin end)) (cadr split)))
         (backprop (-reduce-r-from (lambda (it acc)
                                     (if (>= (1+ (cdr it)) (caar acc))
                                         (cons (cons (car it) (max (cdar acc)
                                                                   (cdr it))) (cdr acc))
                                       (cons it acc)))
                                   (list (-last-item splice))
                                   (butlast splice)))
         (forwardprop (-reduce-from (lambda (acc it)
                                      (if (<= (1- (car it)) (cdar acc))
                                          (cons (cons (min (caar acc)
                                                           (car it)) (cdar acc)) (cdr acc))
                                        (cons it acc)))
                                    (list (car backprop))
                                    (cdr backprop))))
    (nreverse forwardprop)))

(defun intlist-add-point (intlist point)
  "Add point POINT into INTLIST.

See `intlist-add-interval'."
  (intlist-add-interval intlist point point))

(defun intlist-remove-interval (intlist begin end)
  "Remove interval between BEGIN and END inclusive from INTLIST.

This operation runs in O(n) time with respect to number of
intervals in the list."
  (let* ((split-ends (-mapcat (lambda (it)
                                (cond
                                 ((and (= (car it) (cdr it))
                                       (= (car it) begin))
                                  nil)
                                 ((and (>= begin (car it))
                                       (<= begin (cdr it))
                                       (>= end (car it))
                                       (<= end (cdr it)))
                                  (list (cons (car it) (1- begin))
                                        (cons begin end)
                                        (cons (1+ end) (cdr it))))
                                 ((and (>= begin (car it))
                                       (<= begin (cdr it)))
                                  (list (cons (car it) (1- begin))
                                        (cons begin (cdr it))))
                                 ((and (>= end (car it))
                                       (<= end (cdr it)))
                                  (list (cons (car it) end)
                                        (cons (1+ end) (cdr it))))
                                 (t (list it))))
                              intlist)))

    (-remove (lambda (it)
               (and (>= (car it) begin)
                    (<= (cdr it) end))) split-ends)))

(defun intlist-remove-point (intlist point)
  "Remove POINT from INTLIST.

See `intlist-remove-interval'."
  (intlist-remove-interval intlist point point))

(defmacro intlist-loop (spec &rest body)
  "Evaluate BODY with VAR bound to each point from INTLIST, in turn.

\(fn (VAR INTLIST) BODY...)"
  (declare (debug ((symbolp form) body))
           (indent 1))
  (let ((list (make-symbol "list")))
    `(let ((,list ,(cadr spec)))
       (mapc (lambda (int)
               (loop for ,(car spec) from (car int) to (cdr int)
                     do (progn ,@body))) ,list))))

(cl-eval-when (eval)
  (ert-deftest intlist-add-to-empty ()
    (should (equal (intlist-add-interval nil 1 2) '((1 . 2)))))

  (ert-deftest intlist-add-to-nonempty ()
    (let ((cases '(((((1 . 2)) 1 4) . ((1 . 4)))
                   ((((1 . 2)) 2 4) . ((1 . 4)))
                   ((((1 . 2)) 3 4) . ((1 . 4)))
                   ((((3 . 4)) 1 2) . ((1 . 4)))
                   ((((3 . 4)) 1 3) . ((1 . 4)))
                   ((((3 . 4)) 1 4) . ((1 . 4)))
                   ((((1 . 4)) 2 3) . ((1 . 4)))
                   ((((1 . 4)) 3 5) . ((1 . 5)))
                   ((((3 . 6)) 1 4) . ((1 . 6)))
                   ((((3 . 6)) 2 7) . ((2 . 7)))
                   ((((1 . 2)) 4 5) . ((1 . 2) (4 . 5)))
                   ((((1 . 2) (4 . 5)) 7 8) . ((1 . 2) (4 . 5) (7 . 8)))
                   ((((1 . 2) (4 . 5)) 4 10) . ((1 . 2) (4 . 10)))
                   ((((1 . 2) (4 . 5)) 5 10) . ((1 . 2) (4 . 10)))
                   ((((1 . 2) (4 . 5)) 1 10) . ((1 . 10)))
                   ((((1 . 2) (4 . 5)) 2 10) . ((1 . 10)))
                   ((((1 . 2) (4 . 5)) 3 10) . ((1 . 10)))
                   ((((1 . 4) (8 . 10)) 6 9) . ((1 . 4) (6 . 10)))
                   ((((1 . 4) (8 . 10)) 6 11) . ((1 . 4) (6 . 11)))
                   ((((3 . 6) (10 . 12)) 7 8) . ((3 . 8) (10 . 12)))
                   ((((3 . 5) (10 . 12)) 7 8) . ((3 . 5) (7 . 8) (10 . 12)))
                   ((((3 . 6) (8 . 10)) 5 9) . ((3 . 10)))
                   ((((3 . 6) (8 . 10) (12 . 14)) 5 15) . ((3 . 15)))
                   ((((3 . 6) (8 . 10) (12 . 14)) 5 9) . ((3 . 10) (12 . 14)))
                   ((((1 . 3) (5 . 8)) 4 4) . ((1 . 8)))
                   )))
      (cl-dolist (case cases)
        (should (equal (intlist-add-interval (caar case) (cadar case) (caddar case)) (cdr case))))))

  (ert-deftest intlist-remove-empty ()
    (should (equal (intlist-remove-interval nil 1 4) nil)))

  (ert-deftest intlist-remove-nonempty ()
    (let ((cases '(((((1 . 2)) 1 2) . nil)
                   ((((3 . 4)) 1 4) . nil)
                   ((((3 . 4)) 3 7) . nil)
                   ((((3 . 4)) 1 7) . nil)
                   ((((1 . 4)) 1 3) . ((4 . 4)))
                   ((((1 . 4)) 2 3) . ((1 . 1) (4 . 4)))
                   ((((1 . 4)) 3 3) . ((1 . 2) (4 . 4)))
                   ((((1 . 4)) 4 4) . ((1 . 3)))
                   ((((1 . 4)) 3 5) . ((1 . 2)))
                   ((((1 . 4)) 2 2) . ((1 . 1) (3 . 4)))
                   ((((1 . 4) (6 . 8) (10 . 10)) 6 6) . ((1 . 4) (7 . 8) (10 . 10)))
                   ((((1 . 4) (6 . 8) (10 . 10)) 10 10) . ((1 . 4) (6 . 8)))
                   ((((1 . 4) (6 . 9)) 3 6) . ((1 . 2) (7 . 9)))
                   ((((1 . 4) (6 . 9)) 4 10) . ((1 . 3)))
                   ((((1 . 4) (6 . 9) (12 . 15)) 4 13) . ((1 . 3) (14 . 15))))))
      (cl-dolist (case cases)
        (should (equal (intlist-remove-interval (caar case) (cadar case) (caddar case)) (cdr case))))))

  (ert-deftest intlist-loop ()
    (should (equal (let ((sum 0))
                     (intlist-loop (it '((1 . 4) (6 . 8)))
                       (setq sum (+ sum it)))
                     sum)
                   31))
    (should (equal (let ((str ""))
                     (intlist-loop (it '((1 . 4) (6 . 8)))
                       (setq str (concat str (int-to-string it))))
                     str)
                   "1234678"))))

(provide 'interval-list)
;;; interval-list.el ends here
