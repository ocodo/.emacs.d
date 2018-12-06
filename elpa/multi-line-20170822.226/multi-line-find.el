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

(require 'multi-line-candidate)
(require 'multi-line-enter)
(require 'multi-line-shared)

(defclass multi-line-forward-sexp-find-strategy ()
  ((split-regex :initarg :split-regex :initform "[[:space:]]*,")
   (done-regex :initarg :done-regex :initform "[[:space:]]*[})\]]")
   (split-advance-fn :initarg :split-advance-fn :initform
                     'multi-line-comma-advance)))

(defmethod multi-line-at-end-of-candidates
  ((strategy multi-line-forward-sexp-find-strategy))
  (or (looking-at (oref strategy done-regex))
      (save-excursion
        ;; Check to see if we get a scan error when trying to move forward
        (condition-case _ignored (forward-sexp) ('scan-error t)))))

(defmethod multi-line-advance
  ((strategy multi-line-forward-sexp-find-strategy) &optional _context)
  (cl-loop
   for failed = (condition-case _ignored (forward-sexp)
                  ('error t))
   when (or failed
            (when (looking-at (oref strategy split-regex))
              (funcall (oref strategy split-advance-fn))
              t))
   return nil))

(defmethod multi-line-find ((strategy multi-line-forward-sexp-find-strategy)
                            &optional context)
  (nconc (list (make-instance 'multi-line-candidate))
         (progn
           ;; XXX: This is a hack to make hash literals work in ruby. For some
           ;; reason if you execute forward sexp at a '{' but there is a newline
           ;; immediately following that character it passes over the entire
           ;; hash body.
           (re-search-forward "[^[:space:]\n]") (backward-char)
           (cl-loop
            do (multi-line-advance strategy context)
            ;; Make sure we're not adding the same point twice
            until (and last-point (equal (point) last-point))
            for last-point = (point)
            collect (make-instance 'multi-line-candidate)
            until (multi-line-at-end-of-candidates strategy)))))

;; A finder decorator that removes candidates that follow "keyword" arguments,
;; so that things like:
;; :akey "avalue"
;; are always paired
(defclass multi-line-keyword-pairing-finder ()
  ((child :initarg :child)
   (keyword-string :initarg :keyword-string :initform ":")))

(defmethod multi-line-find ((strategy multi-line-keyword-pairing-finder)
                            &optional context)
  (let ((candidates (multi-line-find (oref strategy child) context))
        last-was-included last-candidate)
    (cl-loop for candidate in candidates
             for include-this =
             (or
              (not last-was-included)
              (progn
                (goto-char (multi-line-candidate-position last-candidate))
                (re-search-forward "[^[:space:]]")
                (backward-char)
                (not (looking-at (oref strategy keyword-string)))))
             do (progn
                  (setq last-was-included include-this)
                  (setq last-candidate candidate))
             when include-this
             collect candidate)))

(provide 'multi-line-find)
;;; multi-line-find.el ends here
