;;; multi-line.el --- multi-line statements -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: multi line length whitespace programming
;; URL: https://github.com/IvanMalison/multi-line
;; Version: 0.0.0
;; Package-Requires: ((emacs "24") (s "1.9.0"))

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

;; multi-line aims to provide a flexible framework for automatically
;; multi-lining and single-lining function invocations and
;; definitions, array and map literals and more.  It relies on
;; functions that are defined on a per major mode basis wherever it
;; can so that it functions correctly across many different
;; programming languages.

;;; Code:

(require 'eieio)
(require 'multi-line-decorator)
(require 'multi-line-enter)
(require 'multi-line-find)
(require 'multi-line-respace)
(require 'multi-line-shared)

(defun multi-line-get-markers (enter-strategy find-strategy)
  "Get the markers for multi-line candidates for the statement at point.

ENTER-STRATEGY is a class with the method multi-line-enter, and
FIND-STRATEGY is a class with the method multi-line-find-next."
  (multi-line-enter enter-strategy)
  (let ((markers (list (point-marker))))
    (nconc markers
          (cl-loop until (equal (multi-line-find-next find-strategy) :done)
                   collect (point-marker)))
    (nconc markers (list (point-marker)))))

(defclass multi-line-strategy ()
  ((enter :initarg :enter :initform
          (make-instance multi-line-up-list-enter-strategy))
   (find :initarg :find :initform
         (make-instance multi-line-forward-sexp-find-strategy))
   (respace :initarg :respace :initform
            (multi-line-clearing-reindenting-respacer
             (make-instance multi-line-always-newline)))
   (sl-respace :initarg :sl-respace :initform
               (multi-line-clearing-reindenting-respacer
                (make-instance multi-line-never-newline)))))

(defmethod multi-line-markers ((strategy multi-line-strategy))
  (multi-line-get-markers (oref strategy :enter) (oref strategy :find)))

(defmethod multi-line-execute ((strategy multi-line-strategy)
                               for-single-line)
  (save-excursion
    (let ((markers (multi-line-markers strategy))
          (respacer (if for-single-line (oref strategy :sl-respace)
                      (oref strategy :respace))))
      (multi-line-respace respacer markers))))

(defmethod multi-line-execute-one ((strategy multi-line-strategy)
                                   marker i markers respacer)
  (goto-char (marker-position marker))
  (multi-line-respace-one respacer i markers))

(defclass multi-line-major-mode-strategy-selector ()
  ((default-strategy :initarg :default-strategy :initform
     (make-instance multi-line-strategy))
   (strategy-map :initarg :strategy-map :initform (make-hash-table))))

(defmethod multi-line-execute ((selector multi-line-major-mode-strategy-selector)
                               for-single-line)
  (let ((strategy (or (gethash major-mode (oref selector :strategy-map))
                      (oref selector :default-strategy))))
    (multi-line-execute strategy for-single-line)))

(defmethod multi-line-set-strategy
  ((selector multi-line-major-mode-strategy-selector)
   for-mode strategy)

  (puthash for-mode strategy (oref selector :strategy-map)))

(defmethod multi-line-set-default-strategy
  ((selector multi-line-major-mode-strategy-selector) strategy)

  (oset selector :default-strategy strategy))

(defvar multi-line-master-strategy
  (make-instance multi-line-major-mode-strategy-selector))

(defun multi-line-lisp-advance-fn ()
  "Advance to the start of the next multi-line split for Lisp."
  (re-search-forward "[^[:space:]\n]")
  (backward-char))

(defvar multi-line-skip-first-and-last-respacer
  (make-instance multi-line-always-newline
                 :skip-first t :skip-last t))

(defvar multi-line-skip-fill-respacer
  (make-instance multi-line-fixed-fill-respacer
                 :newline-respacer multi-line-skip-first-and-last-respacer))

(defvar multi-line-skip-fill-stragety
  (make-instance multi-line-strategy
                 :respace (multi-line-clearing-reindenting-respacer
                           multi-line-skip-fill-respacer)))

(defvar multi-line-fill-stragety
  (make-instance multi-line-strategy
                 :respace (multi-line-clearing-reindenting-respacer
                           (make-instance multi-line-fixed-fill-respacer))))

(defvar multi-line-fill-column-strategy
  (make-instance multi-line-strategy
                 :respace (multi-line-clearing-reindenting-respacer
                           (make-instance multi-line-fill-column-respacer))))

(defun multi-line-set-per-major-mode-strategies ()
  "Set language specific strategies."
  (interactive)

  (multi-line-set-strategy
   multi-line-master-strategy 'emacs-lisp-mode
   (make-instance multi-line-strategy
                  :find
                  (make-instance
                   multi-line-forward-sexp-find-strategy
                   :split-regex "[[:space:]\n]+"
                   :done-regex "[[:space:]]*)"
                   :split-advance-fn 'multi-line-lisp-advance-fn)
                  :enter
                  (make-instance
                   multi-line-up-list-enter-strategy)
                  :respace multi-line-skip-fill-respacer))

  (multi-line-set-strategy
   multi-line-master-strategy 'clojure-mode
   (make-instance multi-line-strategy
                  :find
                  (make-instance
                   multi-line-forward-sexp-find-strategy
                   :split-regex "[[:space:]\n]+"
                   :done-regex "[[:space:]]*)}]"
                   :split-advance-fn 'multi-line-lisp-advance-fn)
                  :enter
                  (make-instance
                   multi-line-up-list-enter-strategy)
                  :respace multi-line-skip-fill-respacer))

  (multi-line-set-strategy
   multi-line-master-strategy 'go-mode
   (make-instance multi-line-strategy
                  :respace
                  (multi-line-trailing-comma-respacer
                   (make-instance multi-line-fixed-fill-respacer)))))

(multi-line-set-per-major-mode-strategies)

;;;###autoload
(defun multi-line (arg)
  "Multi-line the statement at point.

When ARG is provided single-line the statement at point instead."
  (interactive "P")
  (let ((for-single-line (if arg t nil))) ; TODO(imalison): better cast to bool
    (multi-line-execute multi-line-master-strategy for-single-line)))

;;;###autoload
(defun multi-line-single-line ()
  "Single-line the statement at point."
  (interactive)
  (multi-line-execute multi-line-master-strategy t))

(provide 'multi-line)
;;; multi-line.el ends here
