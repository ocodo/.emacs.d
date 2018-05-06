;;; iterators.el --- Functions for working with iterators  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Free Software Foundation, Inc

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: Mar 18 2015
;; Keywords: extensions, elisp
;; Compatibility: GNU Emacs >=25
;; Version: 0.1.1
;; Package-Requires: ((emacs "25"))


;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This package extends "generator.el" with higher-level functions.
;;
;;
;; TODO:
;;
;; - hook ilists into seq.el via `cl-defgeneric'


;;; Code:


(eval-when-compile (require 'cl-lib))
(require 'generator)


;;;; Basic stuff

(defmacro iterator-make (&rest body)
  "Create an anonymous iterator.
This is equivalent to (funcall (iter-lambda () BODY...))"
  `(funcall (iter-lambda () ,@body)))


;;;; Special simple iterators

(defun iterator-from-elts (&rest elements)
  "Return an iterator generating the ELEMENTS."
  (iterator-make (while elements (iter-yield (pop elements)))))

(defun iterator-cycle-elts (&rest elements)
  "Return an iterator cycling through the ELEMENTS.
Unlike `iterator-from-elts', after the last of the ELEMENTS has been
generated, the resulting iterator will generate all ELEMENTS
again ad finitum."
  (if (null elements)
      (iterator-from-elts)
    (setcdr (last elements) elements)
    (iterator-make (while t (iter-yield (pop elements))))))

(defun iterator--cons (val iterator)
  (iterator-make
   (iter-yield val)
   (iter-yield-from iterator)))

(defun iterator-iterate-function (function value)
  "Return an iterator of repeated applications of FUNCTION to VALUE.
The sequence of returned elements is starting with VALUE.  Any
successive element will be found by calling FUNCTION on the
preceding element."
  (iterator--cons
   value
   (iterator-make
    (while t (iter-yield (setq value (funcall function value)))))))

(defun iterator-number-range (&optional start end inc)
  "Return an iterator of a number range.
START denotes the first number and defaults to 0.  The second,
optional argument END specifies the upper limit (exclusively).
If nil, the returned iterator is infinite.  INC is the increment
used between the numbers and defaults to 1."
  (let* ((inc   (or inc +1))
         (start (or start 0))
         (i start))
    (if end
        (let ((comp (if (> inc 0) #'< #'>)))
          (iterator-make
           (while (funcall comp i end)
             (iter-yield (prog1 i (cl-incf i inc))))))
      (iterator-make (while t (iter-yield (prog1 i (cl-incf i))))))))

(iter-defun iterator-of-directory-files-1 (directory &optional match nosort recurse follow-links)
  "Helper for `iterator-of-directory-files'."
  (when (file-accessible-directory-p directory)
    (let ((files (directory-files directory t match nosort)) dirs non-dirs)
      (dolist (file files)
        (if (file-directory-p file)
            (push file dirs)
          (push file non-dirs)))
      (dolist (file non-dirs)
        (iter-yield file))
      (dolist (dir dirs)
        (unless (member (file-name-nondirectory (directory-file-name dir)) '("." ".."))
          (iter-yield dir)
          (when (and (or follow-links (not (file-symlink-p dir)))
                     (if (functionp recurse) (funcall recurse dir) recurse))
            (iter-yield-from (iterator-of-directory-files-1
                              dir match nosort recurse follow-links))))))))

(defun iterator-of-directory-files (directory &optional full match nosort recurse follow-links)
  "Return an iterator of names of files in DIRECTORY.
Don't include files named \".\" or \"..\".  The arguments FULL,
MATCH and NOSORT are like in `directory-files'.

Optional argument RECURSE non-nil means recurse on
subdirectories.  If RECURSE is a function, it should be a
predicate accepting one argument, an absolute file name of a
directory, and return non-nil when the returned iterator should
recurse into that directory.  Any other non-nil value means
recurse into every readable subdirectory.

Even with RECURSE non-nil, don't descent into directories by
following symlinks unless FOLLOW-LINKS is non-nil."
  (iterator-map
   (lambda (file) (if full file (file-relative-name file directory)))
   (iterator-of-directory-files-1 directory match nosort recurse follow-links)))


;;;; Operations on iterators, transducers

(defun iterator-filter (predicate iterator)
  "Return an iterator filtering ITERATOR with PREDICATE.
This new iterator will return elements in the same order as
ITERATOR, but only those that fulfill PREDICATE, a function that
accepts one argument."
  (iterator-make
   (while t
     (let ((el (iter-next iterator)))
       (while (not (funcall predicate el))
         (setq el (iter-next iterator)))
       (iter-yield el)))))

(defun iterator-delq (elt iterator)
  "Return an iterator of the elements of ITERATOR not `eq' to ELT."
  (iterator-filter (lambda (el) (not (eq el elt)))  iterator))

(defun iterator-concatenate (&rest iterators)
  "Concatenate the ITERATORS.
Return a new iterator that returns the elements generated by
each iterator in ITERATORS, in order.  The ITERATORS are each
invoked to completion, in order."
  (iterator-make
   (let (current)
     (while (setq current (pop iterators))
       (iter-yield-from current)))))

(defun iterator-map (function &rest iterators)
  "Return an iterator mapping FUNCTION across ITERATORS.
If there are several ITERATORS, FUNCTION is called with that
many arguments.  The resulting iterator will produce elements as
long as the shortest iterator does."
  (iterator-make
   (while t (iter-yield (apply function (mapcar #'iter-next iterators))))))

(defun iterator-take-while (predicate iterator)
  "Return an iterator representing a \"do-while\" loop.
It will invoke ITERATOR to produce elements as long they fulfill
PREDICATE and stop then."
  (iterator-make
   (let (el)
     (while (funcall predicate (setq el (iter-next iterator)))
       (iter-yield el)))))

(defun iterator-take-until (predicate iterator)
  "Return an iterator representing an \"until-do\" loop.
It will invoke ITERATOR to produce elements until one fulfills
PREDICATE.  It will stop after returning this element."
  (iterator-make
   (let (el)
     (while (not (funcall predicate (setq el (iter-next iterator))))
       (iter-yield el))
     (iter-yield el))))

(defun iterator-take (n iterator)
  "Return an iterator of the first N elements of ITERATOR.
This iterator generates at most the first N elements generated
by ITERATOR, in order."
  (iterator-make (while (>= (cl-decf n) 0)
                   (iter-yield (iter-next iterator)))))

(defun iterator-scan (function init iterator)
  "Return an iterator of successive reduced values.
If the elements generated by iterator i are i_1, i_2, ..., the
elements s_1, s_2, ... of the iterator returned by
\(iterator-scan f init i\) are defined recursively by

  s_1     = init
  s_(n+1) = (funcall f s_n  i_n)

as long as i_n exists.

Example: (iterator-scan #\\='* 1 (iterator-number-range 1))
returns an iterator of the factorials."
  (let ((res init))
    (iterator--cons
     res
     (iterator-map (lambda (el) (setq res (funcall function res el)))
                   iterator))))


;;;; Iteration

(defun iterator-flush (iterator)
  "Request all elements from ITERATOR, for side effects only."
  (condition-case nil
      (while t (iter-next iterator))
    (iter-end-of-sequence nil)))


;;;; Processing elements

(defun iterator-reduce (function init iterator)
  "Reduce two-argument FUNCTION across ITERATOR starting with INIT.
This is the same value as the expression

  (iter-last (iterator-scan function init iterator))

would return."
  (let ((res init))
    (iterator-flush (iterator-map (lambda (el) (setq res (funcall function res el))) iterator))
    res))

(defun iterator-to-list (iterator)
  "Convert ITERATOR into a list.
Run ITERATOR until it runs out of elements and return a list of
the generated elements."
  (nreverse (iterator-reduce (lambda (x y) (cons y x)) () iterator)))

(defun iterator-last (iterator)
  "Request all elements from ITERATOR and return the last one."
  (let ((el (iter-next iterator)))
    (condition-case nil
        (while t (setq el (iter-next iterator)))
      (iter-end-of-sequence el))))

(defun iterator-count (iterator)
  "Request all elements from ITERATOR and return their count."
  (iterator-reduce (lambda (s _el) (1+ s)) 0 iterator))

(defun iterator-some (predicate &rest iterators)
  "Return non-nil if PREDICATE is true for any element of ITER or ITERs.
If so, return the true (non-nil) value returned by PREDICATE.
\n(fn PREDICATE ITER...)"
  (catch 'success
    (iterator-flush
     (apply #'iterator-map
            (lambda (&rest elts) (let (res) (when (setq res (apply predicate elts))
                                         (throw 'success res))))
            iterators))
    nil))

(defun iterator-every (predicate &rest iterators)
  "Return non-nil if PREDICATE is true for every element of ITER or ITERs.
\n(fn PREDICATE ITER...)"
  (not (apply #'iterator-some (lambda (&rest args) (not (apply predicate args))) iterators)))

(defun iterator-max (iterator &optional function)
  "Return an element of finite ITERATOR maximizing FUNCTION.
Request all elements from ITERATOR and pass them to FUNCTION, a
one-argument function that must return a number.  Return an
element for which FUNCTION was maximal.  Raise an error if
ITERATOR produced no elements.  FUNCTION defaults to `identity'.

Example: if ITERATOR is an iterator of lists, this would return
a longest generated list: (iterator-max iterator #'length)."
  (let ((first (iter-next iterator))
        (function (or function #'identity)))
    (iterator-reduce
     (lambda (x y) (if (< (funcall function x) (funcall function y))  y  x))
     first iterator)))

(defun iterator-min (iterator &optional function)
  "Return an element of ITERATOR that minimizes FUNCTION.
Request all elements from ITERATOR and pass them to FUNCTION, a
one-argument function that must return a number.  Return an
element for which FUNCTION was minimal.  Raise an error if
ITERATOR produced no elements.  FUNCTION defaults to `identity'."
  (let ((function (or function #'identity)))
    (iterator-max iterator (lambda (x) (- (funcall function x))))))

(defun iterator-mapconcat (function iterator separator)
  "Apply FUNCTION to each element of ITERATOR, and concat the results as strings.
In between of each pair of results, stick in SEPARATOR.  This is
like `mapconcat', but for iterators."
  (let ((first (iter-next iterator)))
    (iterator-reduce (lambda (x y) (concat x separator y))
                     (funcall function first)
                     (iterator-map function iterator))))


;;;; ILists - "Delayed" lists via iterators

(defconst ilist--last-link-tag 'ilist--last-link-tag)

(defun iterator-to-ilist (iterator)
  "Return an ilist using ITERATOR to produce elements."
  (cons ilist--last-link-tag iterator))

(defmacro ilist-make (expr)
  "Return an ilist calling an iterator using EXPR to produce elements."
  `(iterator-to-ilist (iterator-make ,expr)))

(defconst ilist-null
  (cons ilist--last-link-tag nil)
  "A distinguished empty ilist.")

(defun ilistp (object)
  "Return t if OBJECT is an ilist, that is, a cons cell or nil.
Otherwise, return nil."
  (listp object))

(defun ilist-car (ilist)
  "Return the first element of ILIST.
Error if arg is not nil and not a cons cell."
  (if (eq (car ilist) ilist--last-link-tag)
      (let ((iterator (cdr ilist)) new-el)
        (if (null iterator) nil
          (condition-case nil
              (prog1 (setq new-el (iter-next iterator))
                (setcar ilist new-el)
                (setcdr ilist (cons ilist--last-link-tag iterator)))
            (iter-end-of-sequence (setcdr ilist nil)
                                  nil))))
    (car ilist)))

(defun ilist-empty-p (ilist)
  "Return t if ILIST is empty."
  (ignore (ilist-car ilist))
  (null (cdr ilist)))

(defun ilist-cdr (ilist)
  "Return the `ilist-cdr' of ILIST.
Error if arg is not nil and not a cons cell."
  (if (ilist-empty-p ilist) ilist (cdr ilist)))

(defun ilist-cons (el ilist)
  "Return a new ilist with EL as `ilist-car' ILIST as `ilist-cdr'."
  (cons el ilist))

(defun ilist-nthcdr (n ilist)
  "Take `ilist-cdr' N times on ILIST, return the result."
  (cl-dotimes (_ n) (cl-callf ilist-cdr ilist))
  ilist)

(defun ilist-nth (n ilist)
  "Return the Nth element of ILIST.
N counts from zero.  If ILIST is not that long, nil is returned."
  (ilist-car (ilist-nthcdr n ilist)))

(defun ilist-to-iterator (ilist)
  "Return an iterator generating the elements of ILIST.
The structure of ILIST is updated as side effect if new elements
are generated by the returned iterator which were not yet
created in ILIST."
  (iterator-make
   (while (not (ilist-empty-p ilist))
     (prog1 (iter-yield (ilist-car ilist))
       (cl-callf ilist-cdr ilist)))))

(defun ilist-mapcar (function ilist)
  "Apply FUNCTION to each element of ILIST, and make an ilist of the results.
The result is an ilist just as long as ILIST."
  (iterator-to-ilist
   (iterator-map function (ilist-to-iterator ilist))))

(defun ilist (&rest objects)
  "Return a newly created ilist with specified arguments as elements."
  (nconc objects ilist-null))

(defun list-to-ilist (list)
  "Convert LIST into an ilist.
The result is an ilist containing the same elements as LIST in
the same order.  This is a destructive operation modifying LIST."
  (nconc list ilist-null))

(defun ilist-to-list (ilist)
  "Return a list of all elements of ILIST.
All elements of ILIST are generated as side effect."
  (let ((elts '()))
    (while (not (ilist-empty-p ilist))
      (push (ilist-car ilist) elts)
      (cl-callf ilist-cdr ilist))
    (nreverse elts)))

(defun ilist-concatenate (&rest ilists)
  "Concatenate the ILISTS into a new ilist and return the result.
New elements in the argument ilists are generated when being
referenced in the concatenated ilist.  Apart from that, the
argument ilists are not modified."
  (iterator-to-ilist
   (apply #'iterator-concatenate
          (mapcar #'ilist-to-iterator ilists))))

(define-error 'empty-ilist "Empty ilist")

(defun ilist-setcar (ilist object)
  "Set the first element of ILIST to OBJECT.
Error if ILIST is empty.  Return OBJECT."
  (if (ilist-empty-p ilist)
      (signal 'empty-ilist nil)
    (setcar ilist object)))

(defun ilist-setcdr (ilist newcdr)
  "Set the `ilist-cdr' of ILIST to NEWCDR.
Error if ILIST is empty.  Return NEWCDR."
  (if (ilist-empty-p ilist)
      (signal 'empty-ilist nil)
    (setcdr ilist newcdr)))

;;;; ChangeLog:

;; 2018-03-16  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	Optimize order of iterator-of-directory-files yield elements
;; 
;; 	* packages/iterators/iterators.el (iterator-of-directory-files-1): Yield
;; 	all non-directory files before descending into subdirectories.
;; 
;; 2018-01-05  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	Update copyrights of some packages
;; 
;; 	Update copyrights of el-search, iterators, on-screen and smart-yank.
;; 
;; 2017-01-26  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	Update some copyrights
;; 
;; 2016-07-11  Paul Eggert	 <eggert@cs.ucla.edu>
;; 
;; 	Fix some quoting problems in doc strings
;; 
;; 	Most of these are minor issues involving, e.g., quoting `like this' 
;; 	instead of 'like this'.	 A few involve escaping ` and ' with a preceding
;; 	\= when the characters should not be turned into curved single quotes.
;; 
;; 2015-12-12  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	add TODO section
;; 
;; 2015-11-16  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	New function iterator-of-directory-files
;; 
;; 2015-08-21  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	iterators.el: fix comments
;; 
;; 2015-08-04  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	iterators.el: fix two typos
;; 
;; 2015-04-19  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* iterators/iterators.el: Don't need cl-lib at run-time.
;; 
;; 2015-03-27  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	iterators.el: copyright to FSF
;; 
;; 2015-03-27  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	new package: iterators/iterators.el
;; 



(provide 'iterators)

;;; iterators.el ends here
