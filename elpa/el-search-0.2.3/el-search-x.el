;;; el-search-x.el --- Additional pattern definitions for el-search    -*- lexical-binding: t -*-

;; Copyright (C) 2016 Free Software Foundation, Inc

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: 2016_08_03
;; Keywords: lisp


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

;; This file contains additional definitions of el-search patterns.
;; You can just `require' this file, but doing so is not mandatory for
;; using el-search.


;;; Code:

(require 'el-search)


;;;; `change', `changed'

(defvar diff-hl-reference-revision)
(declare-function diff-hl-changes "diff-hl")
(defvar-local el-search--cached-changes nil)

(defun el-search--changes-from-diff-hl (revision)
  "Return a list of changed regions (as conses of positions) since REVISION.
Use variable `el-search--cached-changes' for caching."
  (if (and (consp el-search--cached-changes)
           (equal (car el-search--cached-changes)
                  (list revision (visited-file-modtime))))
      (cdr el-search--cached-changes)
    (when (buffer-modified-p)
      (error "Buffer is modified - please save"))
    (require 'diff-hl)
    ;; `diff-hl-changes' returns line numbers.  We must convert them into positions.
    (save-restriction
      (widen)
      (save-excursion
        (let ((diff-hl-reference-revision revision)
              (current-line-nbr 1) change-beg)
          (goto-char 1)
          (cdr (setq el-search--cached-changes
                     (cons (list revision (visited-file-modtime))
                           (delq nil (mapcar (pcase-lambda (`(,start-line ,nbr-lines ,kind))
                                               (if (eq kind 'delete) nil
                                                 (forward-line (- start-line current-line-nbr))
                                                 (setq change-beg (point))
                                                 (forward-line (1- nbr-lines))
                                                 (setq current-line-nbr (+ start-line nbr-lines -1))
                                                 (cons (copy-marker change-beg)
                                                       (copy-marker (line-end-position)))))
                                             (diff-hl-changes)))))))))))

(defun el-search--change-p (posn &optional revision)
  ;; Non-nil when sexp after POSN is part of a change
  (save-restriction
    (widen)
    (let ((changes (el-search--changes-from-diff-hl revision))
          (sexp-end (scan-sexps posn 1)))
      (while (and changes (< (cdar changes) sexp-end))
        (pop changes))
      (and changes
           (<= (caar changes) posn)))))

(defun el-search--changed-p (posn &optional revision)
  ;; Non-nil when sexp after POSN contains a change
  (when (buffer-modified-p)
    (error "Buffer is modified - please save"))
  (save-restriction
    (widen)
    (let ((changes (el-search--changes-from-diff-hl revision)))
      (while (and changes (<= (cdar changes) posn))
        (pop changes))
      (and changes
           (< (caar changes) (scan-sexps posn 1))))))

(el-search-defpattern change (&optional revision)
  "Matches the object if its text is part of a file change.

Requires library \"diff-hl\".  REVISION defaults to the file's
repository's HEAD commit."
  `(guard (el-search--change-p (point) ,revision)))

(el-search-defpattern changed (&optional revision)
  "Matches the object if its text contains a file change.

Requires library \"diff-hl\".  REVISION defaults to the file's
repository's HEAD commit."
  `(guard (el-search--changed-p (point) ,revision)))


(provide 'el-search-x)

;;; el-search-x.el ends here
