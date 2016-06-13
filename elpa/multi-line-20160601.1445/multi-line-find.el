;;; multi-line-find.el --- multi-line statements -*- lexical-binding: t; -*-

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

;; multi-line-find defines various approaches for finding the points
;; at which a statement can be split into multiple lines.

;;; Code:

(require 'eieio)
(require 'multi-line-shared)

(defclass multi-line-forward-sexp-find-strategy ()
  ((split-regex :initarg :split-regex :initform "[[:space:]]*,")
   (done-regex :initarg :done-regex :initform "[[:space:]]*[})]")
   (split-advance-fn :initarg :split-advance-fn :initform
                     'multi-line-comma-advance)))

(defmethod multi-line-should-stop ((strategy multi-line-forward-sexp-find-strategy))
  (cond
   ((looking-at (oref strategy :done-regex)) :done)
   ((looking-at (oref strategy :split-regex)) :candidate)
   (t nil)))

(defmethod multi-line-find-next ((strategy multi-line-forward-sexp-find-strategy))
  (let (last last-point this-point)
    (setq this-point (point))
    (condition-case nil
        (while (and (not (equal this-point last-point))
                    (not (setq last (multi-line-should-stop strategy))))
                 (forward-sexp)
                 (setq last-point this-point)
                 (setq this-point (point)))
      ('error (setq last :done))
      nil)
    (when (equal last :candidate) (funcall (oref strategy :split-advance-fn)))
    last))

(provide 'multi-line-find)
;;; multi-line-find.el ends here
