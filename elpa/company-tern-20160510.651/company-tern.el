;;; company-tern.el --- Tern backend for company-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2013-2016 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/company-tern
;; Package-Version: 20160510.651
;; Version: 0.2.0
;; Package-Requires: ((company "0.8.0") (tern "0.0.1") (dash "2.8.0") (dash-functional "2.8.0") (s "1.9.0") (cl-lib "0.5.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the README for more details.

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'tern)
(require 'dash)
(require 'dash-functional)
(require 's)

(defvar company-tern-property-marker " ○"
  "String to indicate object own properties.")

(defvar company-tern-meta-as-single-line nil
  "Trim candidate type information to length of frame width.")

(defun company-tern-prefix ()
  "Grab prefix for tern."
  (and tern-mode
       (not (company-in-string-or-comment))
       (or (company-grab-symbol-cons "\\." 1)
           'stop)))

(defun company-tern-candidates-query (prefix callback)
  "Retrieve PREFIX completion candidates from tern.
Use CALLBACK function to display candidates."
  (tern-run-query
   (lambda (data)
     (funcall callback
              (company-tern-sort-by-depth
               (company-tern-format-candidates data))))
   '((type . "completions")
     (includeKeywords . t)
     (depths . t)
     (types . t)
     (docs . t))
   (point)))

(defun company-tern-format-candidates (data)
  "Grab candidates with properties from tern DATA."
  (let ((completions (cdr (assq 'completions data)))
        (property-p (assq 'isProperty data)))
    (mapcar
     (lambda (completion)
       (let ((candidate (cdr (assq 'name completion))))
         (dolist (prop (push property-p completion))
           (put-text-property 0 1 (car prop) (cdr prop) candidate))
         candidate))
     completions)))

(defun company-tern-sort-by-depth (candidates)
  "Sort CANDIDATES list by completion depth."
  (-sort (-on '< 'company-tern-depth) candidates))

(defun company-tern-depth (candidate)
  "Return depth attribute for CANDIDATE."
  (get-text-property 0 'depth candidate))

(defun company-tern-property-p (candidate)
  "Return t if CANDIDATE is object own property."
  (and (null (eq json-false (get-text-property 0 'isProperty candidate)))
       (eq 0 (company-tern-depth candidate))))

(defun company-tern-keyword-p (candidate)
  "Return t if CANDIDATE is a keyword."
  (get-text-property 0 'isKeyword candidate))

(defun company-tern-function-p (candidate)
  "Return t if CANDIDATE is a function."
  (--when-let (get-text-property 0 'type candidate)
    (s-starts-with? "fn(" it)))

(defun company-tern-doc (candidate)
  "Return documentation buffer for CANDIDATE."
  (--when-let (get-text-property 0 'doc candidate)
    (company-doc-buffer it)))

(defun company-tern-meta (candidate)
  "Return short documentation string for chosen CANDIDATE."
  (--when-let (get-text-property 0 'type candidate)
    (if company-tern-meta-as-single-line
        (s-left (frame-width) it)
      it)))

(defun company-tern-annotation (candidate)
  "Return type annotation for chosen CANDIDATE."
  (--when-let (company-tern-get-type candidate)
    (concat it (and (company-tern-property-p candidate)
                    company-tern-property-marker))))

(defun company-tern-get-type (candidate)
  "Analyze CANDIDATE type."
  (unless (company-tern-keyword-p candidate)
    (if (company-tern-function-p candidate)
        (company-tern-function-type candidate)
      (company-tern-variable-type candidate))))

(defun company-tern-function-type (candidate)
  "Get CANDIDATE type as a function."
  (-when-let* ((type (get-text-property 0 'type candidate))
               (annot (if company-tooltip-align-annotations "fn(%s)" "(%s)")))
    (->> (list (cons 'type type))
      (tern-parse-function-type)
      (cadr)
      (--map (car it))
      (-interpose ", ")
      (apply 'concat)
      (format annot))))

(defun company-tern-variable-type (candidate)
  "Get CANDIDATE type as a variable."
  (-when-let* ((type (get-text-property 0 'type candidate))
               (annot (if company-tooltip-align-annotations "%s" " -> %s")))
    (format annot type)))

;;;###autoload
(defun company-tern (command &optional arg &rest _args)
  "Tern backend for company-mode.
See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tern))
    (prefix (company-tern-prefix))
    (annotation (company-tern-annotation arg))
    (meta (company-tern-meta arg))
    (doc-buffer (company-tern-doc arg))
    (ignore-case t)
    (sorted t)
    (candidates (cons :async
                      (lambda (callback)
                        (company-tern-candidates-query arg callback))))))

(provide 'company-tern)

;;; company-tern.el ends here
