;;; basic-c-compile-ert.el --- basic-c-compile: ERT extensions

;; The MIT License (MIT)

;; Copyright (c) 2016 nick96

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Author: Nick Spain <nicholas.spain96@gmail.com>
;; Version: 1.1.1
;; Keywords: C, Makefile, compilation
;; URL: https://github.com/nick96/basic-c-compile

;;; Commentary:
;; Unit testing for basic-c-compile

;;; Code:
(require 'basic-c-compile)
(require 'ert)

;; TODO create temp file (makes more general so can run on any machine)
(ert-deftest files-to-compile-all-test ()
  (let* ((file-path "~/Google Drive/Programming/C/kernighan_ritchie/word_count/word_count.c")
        (dir-c-files (mapconcat 'identity
                                (cl-remove-if-not #'basic-c-compile--c-file-extension-p
                                                            (directory-files (file-name-directory file-path)))
                                " ")))
    ;; Test that all '.c' files are return when 'all' argument is used.
    (should (equal (basic-c-compile--files-to-compile "all" file-path)
                   dir-c-files))))

;; TODO create temp file (makes more general so can run on any machine)
;; Test when 'selection' argument is used
(ert-deftest files-to-compile-selection-test ()
  (let ((str-files-to-comp "test1.c test2.c test3.c test4.c")
        (file-path "~/Google Drive/Programming/C/kernighan_ritchie/word_count/word_count.c"))
    (should (equal (basic-c-compile--files-to-compile "selection"
                                                      file-path
                                                      str-files-to-comp)
                   str-files-to-comp))))

;; TODO create temp file (makes more general so can run on any machine)
;; Test that only the input file is return when argument is not 'all'
;; or 'selection'.
(ert-deftest files-to-compile-nil-test ()
  (let ((file-path  "~/Google Drive/Programming/C/kernighan_ritchie/word_count/word_count.c"))
    (should (equal (basic-c-compile--files-to-compile nil file-path)
                   file-path))))


;; Helper function for create-makefile-single-file-test
(defun read-file (file-name)
  "Read FILE-NAME into a string."
  (with-temp-buffer (insert-file-contents file-name)
                    (buffer-string)))

(defun appropriate-makefile-contents (compiler
                                      files-to-compile
                                      file)
  "Return a string of what the Makefile should contain.
Using compiler COMPILER for FILES-TO-COMPILE and naming out-file FILE.o."
  (let ((makefile-contents
         (format (concat "CC = %s\n"
                         "INFILE = %s\n"
                         "OUTFILE = %s.o\n\n"
                         "build: $(INFILE)\n\t"
                         "$(CC) -Wall $(INFILE)  -o $(OUTFILE)\n\n"
                         "clean:\n\t rm -f *.o \n\n"
                         "rebuild: clean build")
                 compiler
                 files-to-compile
                 (shell-quote-argument (file-name-nondirectory (file-name-sans-extension file))))))
  makefile-contents))

;; Test basic-c-compile--create-makefile but remove the file once done
;; Using gcc but it doesn't really matter because file is not evaluated
(ert-deftest create-makefile-single-file-test ()
  (let ((compiler "gcc")
        (makefile "Makefile")
        (file "test.c"))
    (unwind-protect
        (progn (with-temp-file file (basic-c-compile--create-makefile compiler file file makefile))
               (should (file-exists-p file))
               (should (file-exist-p makefile))
               (should (equal (read-file makefile)
                              (apprpriate-makefile-contents compiler
                                                            file
                                                            file))))
      (progn (delete-file "test.c")
             (delete-file "Makefile")))))

(provide 'basic-c-compile-ert)
;;; basic-c-compile-ert.el ends here
