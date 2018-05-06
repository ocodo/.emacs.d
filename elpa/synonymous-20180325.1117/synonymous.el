;;; synonymous.el --- A thesaurus at your fingertips -*- lexical-binding: t -*-

;; Copyright (C) 2015 Katherine Whitlock
;;
;; Author: Katherine Whitlock <toroidalcode@gmail.com>
;;         Snippets adapted from FlySpell, authored by Manuel Serrano <Manuel.Serrano@inria.fr>
;; URL: http://github.com/toroidal-code/synonymous.el
;; Package-Version: 20180325.1117
;; Version: 1.0
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (request "0.2.0"))
;; Keywords: Utility

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Synonymous is a thesaurus client that allows you to look up words and
;; replace them with a chosen synonym or antonym.

;; To use, simply place the cursor over a word, and use either
;; M-x synonymous-synonyms or M-x synonymous-antonyms

;;; Installation

;; Simply (require 'synonymous)

;;; License:

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Code:
(require 'url)
(require 'url-http)
(require 'request)
(require 'json)
(require 'thingatpt)
(require 'cl-lib)


(defmacro synonymous-get-word (word callback)
  `(request
   (format "http://synonymous-synonymous.a3c1.starter-us-west-1.openshiftapps.com/%s" ,word)
   :parser 'json-read
   :success ,callback
   :error (cl-function (lambda (&key error-thrown &allow-other-keys)
                         (message "Got error: %S" error-thrown)))))

(defmacro synonymous-synonym-filter (data filterfunc)
  "Returns a vector of words (matching data) with their synonyms filtered according to FILTERFUNC."
  `(cl-map 'vector
	   #'(lambda (word-instance)
	       (setcdr (assoc 'synonyms word-instance)
		       (cl-remove-if #'(lambda (w)
					 (not (funcall ,filterfunc w)))
				     (assoc-default 'synonyms word-instance)))
	       word-instance)
	  ,data))

(defmacro synonymous-get-synonyms (word callback)
  `(synonymous-get-word ,word (cl-function (lambda (&key data &allow-other-keys)
				  (setq data (synonymous-synonym-filter data #'(lambda (w) (< 0 (assoc-default 'relevance w)))))
				  (funcall ,callback :data data)))))

(defmacro synonymous-get-antonyms (word callback)
  `(synonymous-get-word ,word (cl-function (lambda (&key data &allow-other-keys)
				  (setq data (synonymous-synonym-filter data #'(lambda (w) (> 0 (assoc-default 'relevance w)))))
				  (funcall ,callback :data data)))))

(defun synonymous-replace-word (&optional antonym event opoint)
  "Prepare to replace a word with a synonym or antonym"
  (unless (mouse-position)
    (error "Pop-up menus do not work on this terminal"))
  (or opoint (setq opoint (point)))
  (let*
      ((cursor-location (point))
       (word (thing-at-point 'word))
       (bounds (bounds-of-thing-at-point 'word))
       (start (car bounds))
       (end (cdr bounds))
       (event event)
       (opoint opoint)
       (callback (cl-function (lambda (&key data &allow-other-keys)
				(let ((replace (synonymous-emacs-popup event data word)))
				  (synonymous-do-replace replace word cursor-location start end opoint))))))
    (if (not antonym)
	(synonymous-get-synonyms word callback)
      (synonymous-get-antonyms word callback))))


(defun synonymous-do-replace (replace word cursor-location start end save)
  "The popup menu callback."
  (cond ((eq replace 'ignore)
         (goto-char save)
         nil)
        (replace
         (let ((old-max (point-max))
               (new-word (if (atom replace) replace (car replace)))
               (cursor-location (+ (- (length word) (- end start)) cursor-location)))
	   (delete-region start end)
	   (goto-char start)
	   (insert new-word)
	   (synonymous-adjust-cursor-point save cursor-location old-max)))
        (t
         (goto-char save)
         nil)))

(defun synonymous-adjust-cursor-point (save cursor-location old-max)
  (if (>= save cursor-location)
      (let ((new-pos (+ save (- (point-max) old-max))))
        (goto-char (pcase new-pos
		     ((pred (> (point-min))) (point-min))
		     ((pred (< (point-max))) (point-max))
		     (_ new-pos))))
    (goto-char save)))

(defun synonymous-extract-synonym-strings (word-instance)
  "Given a WORD-INSTANCE, collect all of the word's synonyms into a list of strings."
  (mapcar #'(lambda (syn) (assoc-default 'word syn))
	  (assoc-default 'synonyms word-instance)))

(defun synonymous-emacs-popup (event data word)
  "The Emacs popup menu."
  (unless window-system
    (error "This command requires pop-up dialogs"))
  (if (not event)
      (let* ((mouse-pos  (mouse-position))
             (mouse-pos  (if (nth 1 mouse-pos)
                             mouse-pos
                           (set-mouse-position (car mouse-pos)
                                               (/ (frame-width) 2) 2)
                           (mouse-position))))
        (setq event (list (list (car (cdr mouse-pos))
                                (1+ (cdr (cdr mouse-pos))))
                          (car mouse-pos)))))
  (cl-flet ((build-menu (word-instance)
			(let ((part-of-speech (assoc-default 'part_of_speech word-instance))
			      (similar (mapconcat #'identity (assoc-default 'similar word-instance) ", "))
			      (cor-menu (mapcar #'(lambda (syn) (list syn syn))
					     (synonymous-extract-synonym-strings word-instance))))
			  (cons (format "%s (%s) %s" word part-of-speech similar) cor-menu))))
    (car (x-popup-menu event (pcase (length data)
			       (`1 (let ((menu (list (build-menu (elt data 0)))))
				     (cons (caar menu) menu)))
			       (_ (cons word (mapcar #'build-menu data))))))))


;;;###autoload
(defun synonymous-synonyms (&optional event opoint)
  "Lookup synonyms for a word."
  (interactive)
  (synonymous-replace-word nil event opoint))

;;;###autoload
(defun synonymous-antonyms (&optional event opoint)
  "Lookup antonyms for a word."
  (interactive)
  (synonymous-replace-word t event opoint))


(provide 'synonymous)
;;; synonymous.el ends here
