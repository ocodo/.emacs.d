;;; basic-c-compile.el --- Quickly create a basic Makefile, compile and run a C program.

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
;; Version: 1.1.2
;; Keywords: C, Makefile, compilation
;; URL: https://github.com/nick96/basic-c-compile
;; Package-Requires: ((cl-lib "0.5"))

;;; Commentary:

;; basic-c-compile.el is a basic script for C programming.  It can create a basic
;; Makefile, compile the C program (with or without the Makefile) and run the
;; file.

;;; Code:

(require 'cl-lib)

;; Global variables
;; These can be changed by the user in their init file

;; Change to whatever compiler you prefer
(defcustom basic-c-compile-compiler "gcc"
  "Compiler used to by basic-c-compile to compile file(s)."
  :group 'basic-c-compile)

;; basic-c-compile-all-files can be "all", "selection"
;; Any other value will mean that only the current file is compiled
(defcustom basic-c-compile-all-files "all"
  "Changes the files compiled by basic-c-compile.
'all' will compile all files in directory.  'selection' will give you a prompt
to list the file.  Any other setting will only compile the current file."
  :group 'basic-c-compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interactive function

;; TODO TEST basic-c-compile-makefile (interactive)
;;;###autoload
(defun basic-c-compile-makefile ()
  "Create a Makefile of the form shown in README."
  (interactive)
  (basic-c-compile--create-makefile basic-c-compile-compiler
                                    (basic-c-compile--files-to-compile basic-c-compile-all-files
                                                                       (buffer-file-name))
                                    (buffer-file-name)
                                    "Makefile"))

;; TODO TEST basic-c-compile-file (interactive)
;;;###autoload
(defun basic-c-compile-file ()
  "Compile file with or without a Makefile."
  (interactive)
  ;; Define local scope variables
  (let* ((path (file-name-directory (buffer-file-name)))
         (infile (buffer-file-name))
         (outfile (concat (file-name-sans-extension infile) ".o")))
    ;; Makefile control flow
    (if (y-or-n-p "Compile with Makefile? ")
        ;; Check for presence of Makefile to stop creating duplicates
        (if (member "Makefile" (directory-files path))
              (if (member outfile (directory-files path))
                  (basic-c-compile--with-makefile "rebuild")
                (basic-c-compile--with-makefile "build"))
          (progn (basic-c-compile--create-makefile basic-c-compile-compiler
                                            (basic-c-compile--files-to-compile basic-c-compile-all-files
                                                                               (buffer-file-name))
                                            infile
                                            "Makefile")
          (basic-c-compile--with-makefile "build")))
      (basic-c-compile--sans-makefile basic-c-compile-compiler
                                      (basic-c-compile--files-to-compile basic-c-compile-all-files
                                                                         (buffer-file-name))
                                      infile))))

;; TODO TEST basic-c-compile-run-c (interactive)
;;;###autoload
(defun basic-c-compile-run-c ()
  "Run the program."
  (interactive)
  (basic-c-compile--run-c-file (file-name-nondirectory (buffer-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Non-interactive functions

;; Function called when user wants to specify a subset of the files to compile
(defun basic-c-compile--choose-files ()
  "Return string of SELECTED-FILES which can be entered from the mini-buffer."
  (let ((selected-files (read-string "Enter file names: ")))
    selected-files))

;; Helper predicate for basic-c-compile--files-to-compile
(defun basic-c-compile--c-file-extension-p (file-name)
  "Return t if FILE-NAME has extension '.c', otherwise nil."
  (equal (last (split-string file-name "\\."))
         '("c")))

(defun basic-c-compile--files-to-compile (var-files-to-compile
                                          file
                                          &optional str-files-to-compile)
  "Return a list of files to compile.
Contents of list depends VAR-FILES-TO-COMPILE.  If 'all' then all '.c' files in
FILE directory will be compiled.  For selected; if STR-FILES-TO-COMPILE is
specified, then files in that list will be compiled (this is mostly for testing
purposes)."
  (cond (;; Make list of all .c files in directory
         (equal var-files-to-compile "all")
         (mapconcat 'identity
                    (mapcar #'shell-quote-argument
                            (cl-remove-if-not #'basic-c-compile--c-file-extension-p
                                              (directory-files (file-name-directory file))))
                    " "))
         (;; Call function that allows input of files to be compiled
          (equal var-files-to-compile "selection")
          (if str-files-to-compile
              str-files-to-compile
            (basic-c-compile--choose-files)))
         (;; Default to only compiling the current file
          t file)))

(defun basic-c-compile--sans-makefile (compiler
                                       files-to-compile
                                       file)
  "Use COMPILER without a Makefile to compile FILES-TO-COMPILE as the name of current FILE."
        (compile (format "%s -Wall %s -o %s.o"
                         compiler
                         files-to-compile
                         (shell-quote-argument (file-name-sans-extension file)))))

(defun basic-c-compile--with-makefile (arg)
  "Compile file using the Makefile with specified ARG (build, clean or rebuild)."
  (compile (format "make %s"
                   arg)))

(defun basic-c-compile--create-makefile (compiler
                                         files-to-compile
                                         file
                                         makefile)
  "Create makefile of rules for compiler COMPILER on FILES-TO-COMPILE.
Out-file will have name FILE.o and makefile will be written to MAKEFILE."
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
    (write-region makefile-contents
                  nil
                  makefile)))


;; Testing this is similar to compiling with Makefile
;; Run file
(defun basic-c-compile--run-c-file (file)
  "Run FILE with the output printing in a temporary buffer."
  (compile (format "./%s.o"
                   (shell-quote-argument (file-name-sans-extension file)))))
(provide 'basic-c-compile)
;;; basic-c-compile.el ends here
