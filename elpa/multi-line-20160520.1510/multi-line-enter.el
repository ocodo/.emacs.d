;;; multi-line-enter.el --- multi-line statements -*- lexical-binding: t; -*-

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

;; multi-line-enter defines various approaches for going to the
;; beginning of a statement that can be multi-lined.

;;; Code:

(require 'eieio)
(require 'multi-line-shared)

(defclass multi-line-up-list-enter-strategy () nil)

(defmethod multi-line-enter ((enter multi-line-up-list-enter-strategy))
  (multi-line-up-list-back)
  (forward-char))

(defclass multi-line-forward-sexp-enter-strategy ()
  ((done-regex :initarg :done-regex :initform "[[:space:]]*[[({]")
   (advance-fn :initarg :advance-fn :initform 'multi-line-lparenthesis-advance)
   (inside-fn :initarg :inside-fn :initform 'multi-line-up-list-back)))

(defclass multi-line-backward-sexp-strategy ()
  ((continue-regex :initarg :continue-regex )))

(defmethod multi-line-enter ((enter multi-line-forward-sexp-enter-strategy))
  (condition-case nil
      (let (last-point)
        (while (not (or (looking-at (oref enter :done-regex))
                        (equal last-point (point))))
          (setq last-point (point))
          (forward-sexp)))
    ('scan-error
     (funcall (oref enter :inside-fn))))
  (funcall (oref enter :advance-fn)))

(provide 'multi-line-enter)
;;; multi-line-enter.el ends here
