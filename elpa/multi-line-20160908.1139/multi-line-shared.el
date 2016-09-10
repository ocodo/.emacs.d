;;; multi-line-shared.el --- multi-line statements -*- lexical-binding: t; -*-

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

;; multi-line-shared defines functions that are generally useful in
;; building multi-line strategies.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)

(defun multi-line-clear-whitespace-at-point ()
  "Clear the whitespace at point."
  (interactive)
  (cl-destructuring-bind (start . end)
      (multi-line-space-markers)
    (delete-region (marker-position start) (marker-position end))))

(cl-defun multi-line-space-markers
    (&optional (space-matches-string "[:space:]\n"))
  "Get markers delimiting whitespace at point.

SPACE-MATCHES-STRING is as a string containing concatenated
character classes that will be used to find whitespace."
  (let ((space-excludes-string (format "[^%s]" space-matches-string)))
    (re-search-backward space-excludes-string)
    (forward-char)
    (let* ((start (point-marker))
           (end (progn
                  (re-search-forward space-excludes-string)
                  (backward-char)
                  (point-marker))))
      (cons start end))))

(defun multi-line-add-remove-or-leave-final-comma ()
  (save-excursion
    (if (looking-at ",?[[:space:]]*\n")
        (when (not (or (looking-at ",")
                       (save-excursion
                         (re-search-backward "[^[:space:]\n]")
                         (looking-at ","))))
          (insert ","))
      (when (or (looking-at ",")
                (progn (re-search-backward "[^[:space:]\n]")
                       (looking-at ",")))
        (delete-char 1)))))

(defun multi-line-lparenthesis-advance ()
  "Advance to the beginning of a statement that can be multi-lined."
  (re-search-forward "[[{(]"))

(defun multi-line-up-list-back ()
  "Go to the beginning of a statement from inside the statement."
  (interactive)
  ;; TODO: This could really used some explanation. I have no idea what is going
  ;; on here now.
  (let ((string-start (nth 8 (syntax-ppss))))
    (when string-start
      (goto-char string-start)))
  (up-list) (backward-sexp))

(defun multi-line-comma-advance ()
  "Pass over a comma when it is present."
  (when (looking-at "[[:space:]\n]*,")
   (re-search-forward ",")))

(defun multi-line-is-newline-between-markers (first second)
  (s-contains? "\n"
               (buffer-substring (marker-position first)
                                 (marker-position second))))

(defmacro multi-line-predicate-or (&rest predicates)
  `(lambda (&rest args)
     (or ,@(cl-loop for predicate in predicates
                    collect `(apply ,predicate args)))))

(defmacro multi-line-predicate-and (&rest predicates)
  `(lambda (&rest args)
     (and ,@(cl-loop for predicate in predicates
                     collect `(apply ,predicate args)))))

(defun multi-line-last-predicate (index candidates)
  (equal index (- (length candidates) 1)))

(defun multi-line-first-predicate (index _candidates)
  (equal index 0))

(defalias 'multi-line-first-or-last-predicate
  (multi-line-predicate-or 'multi-line-first-predicate
                           'multi-line-last-predicate))

(defun multi-line-remove-at-indices (skip-indices list)
  (-remove-at-indices (multi-line-actual-indices skip-indices list) list))

(defun multi-line-actual-indices (skip-indices list)
  (let ((list-length (length list)))
    (cl-loop for index in skip-indices
             collect (mod index list-length))))

(defun multi-line-interpret-prefix-as-number (prefix)
  (cond
   ((numberp prefix) prefix)
   ((and (-non-nil prefix) (listp prefix))
    (truncate (log (car prefix) 4)))
   (0)))

(provide 'multi-line-shared)
;;; multi-line-shared.el ends here
