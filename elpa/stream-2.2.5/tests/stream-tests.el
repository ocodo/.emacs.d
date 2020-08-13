;;; stream-tests.el --- Unit tests for stream.el  -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2017-2020 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>

;; Maintainer: emacs-devel@gnu.org

;; This file is part of GNU Emacs.

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
;;; Code:

(require 'ert)
(require 'stream)
(require 'generator)
(require 'cl-lib)

(ert-deftest stream-empty-test ()
  (should (streamp (stream-empty)))
  (should (stream-empty-p (stream-empty))))

(ert-deftest stream-seq-empty-test ()
  (should (seq-empty-p (stream-empty)))
  (should-not (seq-empty-p (stream-range))))

(ert-deftest stream-make-test ()
  (should (streamp (stream-range)))
  (should (not (stream-empty-p (stream-range))))) ;; Should use stream-list or something

(ert-deftest stream-first-test ()
  (should (= 3 (stream-first (stream-range 3))))
  (should (null (stream-first (stream-empty)))))

(ert-deftest stream-rest-test ()
  (should (= 4 (stream-first (stream-rest (stream-range 3)))))
  (should (= 5 (stream-first (stream-rest (stream-rest (stream-range 3)))))))

(ert-deftest stream-from-iterator-test ()
  (skip-unless (require 'generator nil t))
  (should (equal '(1 2)
                 (seq-into-sequence
                  (stream-from-iterator
                   (funcall (iter-lambda ()
                              (iter-yield 1)
                              (iter-yield 2))))))))

(ert-deftest stream-append-test ()
  (should (stream-empty-p (stream-append)))
  (should (let ((list '(1 2)))
            (equal list (seq-into-sequence (stream-append (stream list))))))
  (should (= (seq-elt (stream-append
                       (stream (list 0 1))
                       (stream-range 2))
                      4)
             4))
  (should (let ((stream (stream (list 0))))
            (and (= (seq-elt (stream-append stream (stream-range 1)) 10)
                    10)
                 (stream-empty-p (stream-rest stream)))))
  (should (equal (seq-into-sequence
                  (stream-append
                   (stream '(1))
                   (stream '())
                   (stream '(2 3))))
                 '(1 2 3))))

(ert-deftest stream-seqp-test ()
  (should (seqp (stream-range))))

(ert-deftest stream-seq-elt-test ()
  (should (null (seq-elt (stream-empty) 0)))
  (should (= 0 (seq-elt (stream-range) 0)))
  (should (= 1 (seq-elt (stream-range) 1)))
  (should (= 10 (seq-elt (stream-range) 10))))

(ert-deftest stream-seq-length-test ()
  (should (zerop (seq-length (stream-empty))))
  (should (= 10 (seq-length (stream-range 0 10)))))

(ert-deftest stream-seq-doseq-test ()
  (let ((stream (stream '(a b c d)))
        (lst '()))
    (seq-doseq (elt stream)
      (push elt lst))
    (should (equal '(d c b a) lst))))

(ert-deftest stream-seq-let-test ()
  (seq-let (first _ third &rest rest) (stream-range 2 7)
    (should (= first 2))
    (should (= third 4))
    ;; The rest of the stream shouldn't be consumed
    (should (streamp rest))
    (should (= 5 (stream-first rest)))
    (should (= 6 (stream-first (stream-rest rest))))
    (should (stream-empty-p (stream-rest (stream-rest rest))))))

(ert-deftest stream-seq-subseq-test ()
  (should (equal (seq-into (seq-subseq (stream (list 0 1 2 3 4)) 1 3) 'list)
                           (seq-subseq         (list 0 1 2 3 4)  1 3)))
  (should (= (stream-first (seq-subseq (stream-range 0) 5))
             5))
  (should (= (stream-first (seq-subseq (seq-subseq (stream-range 0) 5) 5))
             10))

  (should-error (seq-subseq (stream-range 0) -1)))

(ert-deftest stream-seq-into-test ()
  (should (streamp (seq-into (stream-empty) 'stream)))
  (should (streamp (seq-into '(2 4 5) 'stream)))
  (should (= 2  (stream-first (seq-into '(2 4 5) 'stream))))
  (should (null (seq-into (stream-empty) 'list)))
  (should (equal '(0 1 2 3 4 5 6 7 8 9) (seq-into (stream-range 0 10) 'list))))

(ert-deftest stream-seq-take-test ()
  (should (streamp (seq-take (stream-range) 2)))
  (should (= 0 (stream-first (seq-take (stream-range) 2))))
  (should (= 1 (stream-first (stream-rest (seq-take (stream-range) 2)))))
  (should (null (stream-first (stream-rest (stream-rest (seq-take (stream-range) 2))))))
  (should (stream-empty-p (stream-rest (stream-rest (seq-take (stream-range) 2))))))

(ert-deftest stream-seq-drop-test ()
  (should (streamp (seq-drop (stream-range) 2)))
  (should (= 2 (stream-first (seq-drop (stream-range) 2))))
  (should (= 3 (stream-first (stream-rest (seq-drop (stream-range) 2)))))
  (should (stream-empty-p (seq-drop (stream-empty) 2))))

(ert-deftest stream-seq-take-while-test ()
  (let ((stream (stream '(1 3 2 5))))
    (should (stream-empty-p (seq-take-while #'identity (stream-empty))))
    (should (streamp (seq-take-while #'cl-oddp stream)))
    (should (= 1 (stream-first (seq-take-while #'cl-oddp stream))))
    (should (= 3 (stream-first (stream-rest (seq-take-while #'cl-oddp stream)))))
    (should (stream-empty-p (stream-rest (stream-rest (seq-take-while #'cl-oddp stream)))))))

(ert-deftest stream-seq-drop-while-test ()
  (let ((stream (stream '(1 3 2 5))))
    (should (streamp (seq-drop-while #'cl-evenp stream)))
    (should (stream-empty-p (seq-drop-while #'identity (stream-empty))))
    (should (= 2 (stream-first (seq-drop-while #'cl-evenp stream))))
    (should (= 5 (stream-first (stream-rest (seq-drop-while #'cl-evenp stream)))))
    (should (stream-empty-p (stream-rest (stream-rest (seq-drop-while #'cl-evenp stream)))))))

(ert-deftest stream-seq-map-test ()
  (should (stream-empty-p (seq-map #'- (stream-empty))))
  (should (= -1 (stream-first (seq-map #'- (stream-range 1)))))
  (should (= -2 (stream-first (stream-rest (seq-map #'- (stream-range 1)))))))

(ert-deftest stream-seq-mapn-test ()
  (should (streamp (seq-mapn #'+ (stream (list 1 2 3)) (stream (list 4 5 6)))))
  (should (not (streamp (seq-mapn #'+ (stream (list 1 2 3)) (stream (list 4 5 6)) (list 7 8 9)))))
  (should (= 2 (seq-length (seq-mapn #'+ (stream (list 1 2 3)) (stream (list 4 5))))))
  (should (equal (letrec ((fibs (stream-cons
                                 1
                                 (stream-cons
                                  1
                                  (seq-mapn #'+ fibs (stream-rest fibs))))))
                   (seq-into (seq-take fibs 10) 'list))
                 '(1 1 2 3 5 8 13 21 34 55))))

(ert-deftest stream-seq-do-test ()
  (let ((result '()))
    (seq-do
     (lambda (elt)
       (push elt result))
     (stream-range 0 5))
    (should (equal result '(4 3 2 1 0)))))

(ert-deftest stream-seq-filter-test ()
  (should (stream-empty-p (seq-filter #'cl-oddp (stream-empty))))
  (should (stream-empty-p (seq-filter #'cl-oddp (stream-range 0 4 2))))
  (should (= 1 (stream-first (seq-filter #'cl-oddp (stream-range 0 4)))))
  (should (= 3 (stream-first (stream-rest (seq-filter #'cl-oddp (stream-range 0 4))))))
  (should (stream-empty-p (stream-rest (stream-rest (seq-filter #'cl-oddp (stream-range 0 4)))))))

(ert-deftest stream-delay-test ()
  (should (streamp (stream-delay (stream-range))))
  (should (= 0 (stream-first (stream-delay (stream-range)))))
  (should (= 1 (stream-first (stream-rest (stream-delay (stream-range))))))
  (should (let ((stream (stream-range 3 7)))
            (equal (seq-into (stream-delay stream) 'list)
                   (seq-into               stream  'list))))
  (should (null (seq-into (stream-delay (stream-empty)) 'list)))
  (should (let* ((evaluated nil)
                 (one-plus (lambda (el)
                             (setq evaluated t)
                             (1+ el)))
                 (stream (seq-map one-plus (stream '(1)))))
            (equal '(nil 2 t)
                   (list evaluated (stream-first stream) evaluated))))
  (should (let* ((a 0)
                 (set-a (lambda (x) (setq a x)))
                 (s (stream-delay (stream (list a))))
                 res1 res2)
            (funcall set-a 5)
            (setq res1 (stream-first s))
            (funcall set-a 11)
            (setq res2 (stream-first s))
            (and (equal res1 5)
                 (equal res2 5)))))

(ert-deftest stream-seq-copy-test ()
  (should (streamp (seq-copy (stream-range))))
  (should (= 0 (stream-first (seq-copy (stream-range)))))
  (should (= 1 (stream-first (stream-rest (seq-copy (stream-range))))))
  (should (let ((stream (stream-range 3 7)))
            (equal (seq-into (seq-copy stream) 'list)
                   (seq-into stream 'list))))
  (should (null (seq-into (seq-copy (stream-empty)) 'list))))

(ert-deftest stream-range-test ()
  (should (stream-empty-p (stream-range 0 0)))
  (should (stream-empty-p (stream-range 3 3)))
  (should (= 0 (stream-first (stream-range 0 6 2))))
  (should (= 2 (stream-first (stream-rest (stream-range 0 6 2)))))
  (should (= 4 (stream-first (stream-rest (stream-rest (stream-range 0 6 2))))))
  (should (stream-empty-p (stream-rest (stream-rest (stream-rest (stream-range 0 6 2))))))
  (should (= -4 (stream-first (stream-rest (stream-rest (stream-range 0 nil -2)))))))

(ert-deftest stream-list-test ()
  (dolist (list '(nil '(1 2 3) '(a . b)))
    (should (equal list (seq-into (stream list) 'list)))))

(ert-deftest stream-seq-subseq-test ()
  (should (stream-empty-p (seq-subseq (stream-range 2 10) 0 0)))
  (should (= (stream-first (seq-subseq (stream-range 2 10) 0 3)) 2))
  (should (= (seq-length (seq-subseq (stream-range 2 10) 0 3)) 3))
  (should (= (seq-elt (seq-subseq (stream-range 2 10) 0 3) 2) 4))
  (should (= (stream-first (seq-subseq (stream-range 2 10) 1 3)) 3))
  (should (= (seq-length (seq-subseq (stream-range 2 10) 1 3)) 2))
  (should (= (seq-elt (seq-subseq (stream-range 2 10) 1 3) 1) 4)))

(ert-deftest stream-pop-test ()
  (let* ((str (stream '(1 2 3)))
         (first (stream-pop str))
         (stream-empty (stream-empty)))
    (should (= 1 first))
    (should (= 2 (stream-first str)))
    (should (null (stream-pop stream-empty)))))

(ert-deftest stream-scan-test ()
  (should (eq (seq-elt (stream-scan #'* 1 (stream-range 1)) 4) 24)))

(ert-deftest stream-flush-test ()
  (should (let* ((times 0)
                 (count (lambda () (cl-incf times))))
            (letrec ((make-test-stream (lambda () (stream-cons (progn (funcall count) nil)
                                                          (funcall make-test-stream)))))
              (stream-flush (seq-take (funcall make-test-stream) 5))
              (eq times 5)))))

(ert-deftest stream-iterate-function-test ()
  (should (equal (list 0 1 2) (seq-into-sequence (seq-take (stream-iterate-function #'1+ 0) 3)))))

(ert-deftest stream-concatenate-test ()
  (should (equal (seq-into-sequence
                  (stream-concatenate
                   (stream (list (stream (list 1 2 3))
                                 (stream (list))
                                 (stream (list 4))
                                 (stream (list 5 6 7 8 9))))))
                 (list 1 2 3 4 5 6 7 8 9))))

;; Tests whether calling stream processing functions ("transducers")
;; doesn't generate elements from argument streams

(defvar this-delayed-stream-function nil)

(defun make-delayed-test-stream ()
  (stream-make
   (cons (prog1 1 (error "`%s' not completely delayed" this-delayed-stream-function))
         (make-delayed-test-stream))))

(defmacro deftest-for-delayed-evaluation (call)
  (let ((function (car call)))
    `(ert-deftest ,(intern (concat (symbol-name function) "-delayed-test")) ()
       (let ((this-delayed-stream-function ',function))
         (should (prog1 t ,call))))))

(deftest-for-delayed-evaluation (streamp        (make-delayed-test-stream)))
(deftest-for-delayed-evaluation (seqp           (make-delayed-test-stream)))
(deftest-for-delayed-evaluation (stream-append  (make-delayed-test-stream) (make-delayed-test-stream)))
(deftest-for-delayed-evaluation (seq-take (make-delayed-test-stream) 2))
(deftest-for-delayed-evaluation (seq-drop (make-delayed-test-stream) 2))
(deftest-for-delayed-evaluation (seq-take-while #'numberp (make-delayed-test-stream)))
(deftest-for-delayed-evaluation (seq-map #'identity (make-delayed-test-stream)))
(deftest-for-delayed-evaluation (seq-mapn #'cons
                                          (make-delayed-test-stream)
                                          (make-delayed-test-stream)))
(deftest-for-delayed-evaluation (seq-filter #'cl-evenp (make-delayed-test-stream)))
(deftest-for-delayed-evaluation (stream-delay (make-delayed-test-stream)))
(deftest-for-delayed-evaluation (seq-copy (make-delayed-test-stream)))
(deftest-for-delayed-evaluation (seq-subseq (make-delayed-test-stream) 2))
(deftest-for-delayed-evaluation (stream-scan #'* 1 (make-delayed-test-stream)))
(deftest-for-delayed-evaluation (stream-concatenate (stream (list (make-delayed-test-stream)
                                                                  (make-delayed-test-stream)))))

(provide 'stream-tests)
;;; stream-tests.el ends here
