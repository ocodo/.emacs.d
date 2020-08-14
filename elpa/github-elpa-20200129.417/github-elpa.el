;;; github-elpa.el --- Build and publish ELPA repositories with GitHub Pages

;; Copyright (C) 2016 10sr

;; Author: 10sr<8slashes+el@gmail.com>
;; URL: https://github.com/10sr/github-elpa
;; Version: 0.0.1
;; Package-Requires: ((package-build "1.0") (commander "0.7.0") (git "0.1.1"))

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:

;; github-elpa is an Emacs command-line utility to build your own
;; package.el-compatible package repository in your git repository.
;; By default this repository will be built into docs/elpa directory,
;; so by just pushing it to GitHub you can publish the repository with
;; GitHub Pages.

;; This package is mainly intended to be used from Cask.
;; See https://github.com/10sr/github-elpa for usage guide.

;;; Code:

(require 'package-build)
(require 'git)

(defvar github-elpa-working-dir
  "./.github-elpa-working")

(defvar github-elpa-archive-dir
  "./docs/elpa")

(defvar github-elpa-recipes-dir
  "./recipes")

(defvar github-elpa-tar-executable
  nil)

(defun github-elpa--git-check-repo ()
  "Check if current directory is git toplevel directory.
If not throw error."
  (or (git-repo? default-directory)
      (error "Current directory is not a git toplevel directory")))

(defun github-elpa--git-check-workdir-clean ()
  "Check if currnet working tree is clean.
If not throw error."
  (let ((git-repo default-directory))
    (condition-case err
        (git-run "diff"
                 "--quiet")
      (git-error
       (error "Current working tree is not clean")))))

(defun github-elpa--git-commit-archives ()
  "Commit elpa archives to git repository."
  (let ((git-repo default-directory))
    (git-add github-elpa-archive-dir)
    (git-commit "[github-elpa] Update archive"
                github-elpa-archive-dir)))

;;;###autoload
(defun github-elpa-build ()
  "Github elpa build."
  (let ((package-build-working-dir
         (expand-file-name github-elpa-working-dir))
        (package-build-archive-dir
         (expand-file-name github-elpa-archive-dir))
        (package-build-recipes-dir
         (expand-file-name github-elpa-recipes-dir)))
    ;;(github-elpa--git-check-repo)
    ;;(github-elpa--git-check-workdir-clean)
    (make-directory package-build-archive-dir t)
    ;; Currently no way to detect build failure...
    (dolist (recipe (directory-files package-build-recipes-dir nil "^[^.]"))
      (message "")
      (message "")
      (message ":: github-elpa: packaging recipe %s" recipe)
      (let ((package-build-tar-executable (or github-elpa-tar-executable
                                              package-build-tar-executable)))
        (package-build-archive recipe)))
    (package-build-cleanup)))

;;;###autoload
(defun github-elpa-commit ()
  "Github elpa commit."
  (let ((package-build-working-dir
         (expand-file-name github-elpa-working-dir))
        (package-build-archive-dir
         (expand-file-name github-elpa-archive-dir))
        (package-build-recipes-dir
         (expand-file-name github-elpa-recipes-dir)))
    (message ":: github-elpa: Commit packages in %s"
              package-build-archive-dir)
    (github-elpa--git-check-repo)
    (github-elpa--git-commit-archives)))

(provide 'github-elpa)

;;; github-elpa.el ends here
