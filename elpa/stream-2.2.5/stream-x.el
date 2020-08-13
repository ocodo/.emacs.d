;;; stream-x.el --- Additional functions for working with streams  -*- lexical-binding: t -*-

;; Copyright (C) 2017 - 2020 Free Software Foundation, Inc

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: 2017_03_22
;; Keywords: stream, laziness, sequences


;; This file is not part of GNU Emacs.

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

;; This file contains additional functions for working with streams.


;;; Code:

(require 'stream)


(defun stream-substream-before (stream rest)
  "Return a stream of the elements of STREAM before REST.

REST is a rest of STREAM: it must either be `eq' to STREAM or to
one of the subsequent calls of `stream-rest' on STREAM.  The
return value is a newly created stream containing the first
elements of STREAM with REST cut off.

When REST appears multiple times as a rest of STREAM, a stream
with the minimal number of elements is returned."
  (stream-make
   (if (eq stream rest)
       nil
     (cons (stream-first stream)
           (stream-substream-before (stream-rest stream) rest)))))

(defun stream-divide-with-get-rest-fun (stream get-rest-fun)
  "Divide STREAM into two parts according to GET-REST-FUN.

The return value is a list (S R) where R is the result of
(funcall get-rest-fun STREAM) and S a stream of minimal length so
that (stream-append S R) is equivalent to STREAM.

Calling GET-REST-FUN on STREAM must be `eq' to one of
STREAM, (stream-rest STREAM), (stream-rest (stream-rest STREAM)),
..."
  (let ((rest (funcall get-rest-fun stream)))
    (list (stream-substream-before stream rest) rest)))

(defun stream-divide (stream predicate)
  "Divide STREAM between the first pair of elements for that PREDICATE fails.

When STREAM generates the elements S_1, S_2, ..., call
(PREDICATE S_i, S_i+1) for i=1,2,... until the first index i=k is
found so that (funcall PREDICATE S_k S_k+1) returns nil.

The return value is a list of two streams (HEAD REST) where
HEAD generates the elements S_1, ... S_k and REST is the rest of STREAM
generating the remaining elements S_k+1, ...

Example:

  (mapcar #'seq-into-sequence
          (stream-divide
           (stream (list 1 2 3 5 6 7 9 10 11 23))
           (lambda (this next) (< (- next this) 2))))
==> ((1 2 3)
     (5 6 7 9 10 11 23))


If STREAM is finite and no index k with (funcall PREDICATE S_k S_k+1) ==>
nil is found, return (STREAM E) where E is an empty stream.  When
STREAM is infinite and no such index is found, this function will not
terminate.

See `stream-divide-with-get-rest-fun' for a generalization of this function."
  (stream-divide-with-get-rest-fun stream (stream-divide--get-rest-fun predicate)))

(defun stream-divide--get-rest-fun (pred)
  (lambda (s)
    (unless (stream-empty-p s)
      (while (let ((this (stream-pop s)))
               (unless (stream-empty-p s)
                 (funcall pred this (stream-first s))))))
    s))

(defun stream-partition (stream predicate)
  "Partition STREAM into bunches where PREDICATE returns non-nil for subsequent elements.

The return value is a stream S: S_1, S_2, ... of streams S_i of
maximal length so that (stream-concatenate S) is equivalent to STREAM
and for any pair of subsequent elements E, F in any S_i
(PREDICATE E F) evals to a non-nil value.

Often, but not necessarily, PREDICATE is an equivalence predicate.

Example:

  (seq-into-sequence
   (seq-map #'seq-into-sequence
            (stream-partition
             (stream (list 1 2 3 5 6 7 9 10 15 23))
                (lambda (x y) (< (- y x) 2)))))
   ==> ((1 2 3)
        (5 6 7)
        (9 10)
        (15)
        (23))

See `stream-partition-with-get-rest-fun' for a generalization of this function."
  (stream-partition-with-get-rest-fun stream (stream-divide--get-rest-fun predicate)))

(defun stream-partition-with-get-rest-fun (stream get-rest-fun)
  "Call `stream-divide-with-get-rest-fun' on stream ad finitum.
The return value is a (not necessarily finite) stream S of
streams S_i where (stream-concatenate S) is equivalent to STREAM,

  (S_1 R_1)      := (stream-divide-with-get-rest-fun STREAM get-rest-fun)

and

  (S_i+1  R_i+1) := (stream-divide-with-get-rest-fun R_i    get-rest-fun)

as long as R_i is not empty."
  (stream-make
   (if (stream-empty-p stream) nil
     (let ((divided (stream-divide-with-get-rest-fun stream get-rest-fun)))
       (cons (car divided)
             (stream-partition-with-get-rest-fun (cadr divided) get-rest-fun))))))


(provide 'stream-x)

;;; stream-x.el ends here
