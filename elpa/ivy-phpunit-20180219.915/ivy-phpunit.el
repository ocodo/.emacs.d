;;; ivy-phpunit.el --- Ivy integration for phpunit.el

;; Copyright (C) 2018, 12pt

;; Author: 12pt
;; URL: https://github.com/12pt/ivy-phpunit
;; Package-Version: 20180219.915
;; Package-Commit: ffedb0138d36564e8e36a28fd9bc71ea8944681f
;; Version: 0.0.1
;; Keywords: convenience tools ivy phpunit php
;; Package-Requires: ((ivy "0.10.0") (phpunit "0.7.0") (emacs "25"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;;
;; You should always be able to find the latest version here at <URL:https://github.com/12pt/ivy-phpunit.el/>
;; This project is inspired by helm-phpunit but also provides additional utility such as selecting classes to test,
;; and allowing for fast swapping between test classes with ivy-phpunit-list-test-classes.
;;
;; Require the package and then call either `ivy-phpunit-test-function', `ivy-phpunit-list-test-classes', or `ivy-phpunit-test-class'.

;;; Code:
(require 'phpunit)
(require 'ivy)

(defgroup ivy-phpunit nil
  "Quick running of PHPUnit tests."
  :group 'convenience)

(defcustom ivy-phpunit-ignorelist '("setUp" "tearDown")
  "Functions to ignore when listing test candidates."
  :type '(repeat string)
  :options '("setUpBeforeClass" "tearDownAfterClass")
  :group 'ivy-phpunit)

(defconst ivy-phpunit-list-tests
  "^ - \\(?1:[[:word:]]+\\)::\\(?2:[[:word:]]+\\)$"
  "Regular expression for PHPUnit's response to --list-tests.".)

(defun ivy-phpunit--find-funcs ()
  "Find all the PHP function names in the current buffer and insert them into a list."
  (let (funcs '())
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp php-beginning-of-defun-regexp nil t)
        (unless (member (match-string-no-properties 1) ivy-phpunit-ignorelist)
          (add-to-list 'funcs (match-string-no-properties 1)))))
    funcs))

(defun ivy-phpunit--test-func (func-name &optional filename)
  "Run a test given its name via FUNC-NAME.
If non-nil, use FILENAME as the name of the file the test class/FUNC-NAME exists in."
  (let* ((filename (or filename buffer-file-name))
         (args (s-concat
                (shell-quote-argument filename)
                " --filter '" (phpunit-get-current-class) "::" func-name "'"))) ; select the test
    (phpunit-run args)))

(defun ivy-phpunit--parse-tests (output)
  "Split the string OUTPUT into a list of (classname . testname).
Expecting OUTPUT to be the result of running phpunit --list-tests."
  (let ((pos 0)
        matches)
    (while (string-match ivy-phpunit-list-tests output pos)
      (push
       (list (match-string-no-properties 1 output) (match-string-no-properties 2 output))
       matches)
      (setq pos (match-end 0)))
    matches))

(defun ivy-phpunit--filter-classes (test-names)
  "Convert the TEST-NAMES list of (class . function) to a flat list of just the classes."
  (delete-dups (mapcar 'car test-names)))

(defun ivy-phpunit--filter-functions (test-names)
  "Convert the TEST-NAMES list of (class . function) to a flat list of just the functions."
  (delete-dups (mapcar 'cdr test-names)))

(defun ivy-phpunit--class-to-file-path (classname)
  "Attempt to get the source file for the given test.
We do this by recursively searching from the project root for files matching the CLASSNAME, and picking the first one."
  (let ((project-root (phpunit-get-root-directory)))
    (car (directory-files-recursively project-root (s-concat classname ".php")))))

;;-----------------------------------------------------------------------------------------------

(defun ivy-phpunit-list-test-classes ()
  "Find all the test classes in this project.
If called interactively, allow the user to quick-switch via ivy to the class.
If not, just return a list of classes."
  (interactive)
  (let* ((output (phpunit--execute "--list-tests"))
        (tests (ivy-phpunit--parse-tests output)))
    (if (called-interactively-p 'any)
        (ivy-read "Edit a test: " (ivy-phpunit--filter-classes tests)
                  :sort t
                  :caller 'ivy-phpunit-list-test-classes
                  :action (lambda (classname) (find-file (ivy-phpunit--class-to-file-path classname))))
      tests)))

(defun ivy-phpunit-test-class ()
  "Find all test classes in the current project and enable the user to test it."
  (interactive)
  (ivy-read "Class to test: " (ivy-phpunit--filter-classes (ivy-phpunit-list-test-classes))
            :sort t
            :caller 'ivy-phpunit-test-class
            :action (lambda (classname) (phpunit-run (s-concat "--filter '" classname "'")))))

(defun ivy-phpunit-test-function ()
  "Find all the test functions in the current buffer and allow user to select one to test."
  (interactive)
  (ivy-read "Function to test: " (ivy-phpunit--find-funcs)
            :sort t
            :caller 'ivy-phpunit-select-test
            :action (lambda (x) (ivy-phpunit--test-func x))))

(provide 'ivy-phpunit)

;;; ivy-phpunit.el ends here
