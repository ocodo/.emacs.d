;;; xcode-parser.el --- macOS Property List Parser

;; Copyright (C) 2017 Olive Toast Software Ltd

;; Author: John Buckley <john at olivetoast.com>
;; URL: https://github.com/nhojb/xcode-project
;; Version: 1.0
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; This is a library for parsing Xcode project.pbxproj files.
;; These files use the original OpenStep text file format.
;; This parser implements only enough of the Property List format
;; to read Xcode files.  Notably this means there is no support
;; for reading binary (NSData) objects or the more recent xml/binary
;; Property List file formats.

;; For more information about the OpenStep property list format see:
;; <URL:https://developer.apple.com/library/content/documentation/Cocoa/Conceptual/PropertyLists/OldStylePlists/OldStylePLists.html>

;; The user-serviceable entry points for the parser are the functions
;; `xcode-parser-read', `xcode-parser-read-from-string', `xcode-parser-read-file'.

;;; Code:

;; Parameters

(defvar xcode-parser-array-type 'vector
  "Type to convert PLIST arrays to.
Must be one of `vector' or `list'.  Consider let-binding this around
your call to `xcode-parser-read' instead of `setq'ing it.")

;;; Utilities

;; Reader utilities

(defalias 'xcode-parser-advance 'forward-char)

(defsubst xcode-parser-peek ()
  "Return the character at point."
  (or (char-after) :xcode-parser-eof))

(defsubst xcode-parser-pop ()
  "Advance past the character at point, returning it."
  (let ((char (xcode-parser-peek)))
    (if (eq char :xcode-parser-eof)
        (signal 'end-of-file nil)
      (xcode-parser-advance)
      char)))

(defsubst xcode-parser-skip-whitespace ()
  "Skip past the whitespace at point."
  (skip-chars-forward "\t\r\n\f\b "))

(defsubst xcode-parser-skip-whitespace-or-comment ()
  "Skip past any whitespace and/or comment at point."
  (xcode-parser-skip-whitespace)
  (if (xcode-parser-skip-comment)
        (xcode-parser-skip-whitespace)))

;; Comments

(defun xcode-parser-skip-comment ()
  "Skip over the PLIST comment at point."
  (if (eq (char-after) ?/)
      (cond ((looking-at "/\\*")
             ;; '.' match must be non-greedy to avoid taking in further comments.
             (if (looking-at "/\\*.*?\\(\n.*?\\)*\\*/")
                 (goto-char (match-end 0))
               (signal 'xcode-parser-comment-format (list (point)))))
            ((looking-at "//.*")
             (goto-char (match-end 0))))))

;; Error conditions

(put 'xcode-parser-error 'error-message "Unknown PLIST error")
(put 'xcode-parser-error 'error-conditions '(xcode-parser-error error))

(put 'xcode-parser-readtable-error 'error-message "PLIST readtable error")
(put 'xcode-parser-readtable-error 'error-conditions
     '(xcode-parser-readtable-error xcode-parser-error error))

(put 'xcode-parser-comment-format 'error-message "Invalid comment format")
(put 'xcode-parser-comment-format 'error-conditions
     '(xcode-parser-comment-format xcode-parser-error error))

(put 'xcode-parser-string-escape 'error-message "Bad unicode escape")
(put 'xcode-parser-string-escape 'error-conditions
     '(xcode-parser-string-escape xcode-parser-error error))

(put 'xcode-parser-string-format 'error-message "Bad string format")
(put 'xcode-parser-string-format 'error-conditions
     '(xcode-parser-string-format xcode-parser-error error))

(put 'xcode-parser-array-format 'error-message "Bad PLIST array")
(put 'xcode-parser-array-format 'error-conditions
     '(xcode-parser-array-format xcode-parser-error error))

(put 'xcode-parser-dict-format 'error-message "Bad PLIST dict")
(put 'xcode-parser-dict-format 'error-conditions
     '(xcode-parser-dict-format xcode-parser-error error))

;;; Numbers

;; Number parsing

(defun xcode-parser-read-number ()
 "Read the PLIST number following point.

N.B.: Only numbers which can fit in Emacs Lisp's native number
representation will be parsed correctly."
 ;; It is important that this regex only permit a single decimal point
 ;; in the result. This is because input with > 1 decimal point must be
 ;; treated as an unquoted string.
 (if (and (looking-at "[-+]?[0-9]*[.0-9]\\{1\\}[0-9]*\\([Ee][+-]?[0-9]+\\)?\\b")
          ;; a second decimal point is not permitted - should be an unquoted string.
          (not (eq (char-after (match-end 0)) ?.)))
     (progn
       (goto-char (match-end 0))
       (string-to-number (match-string 0)))
   ;; else treat as an unquoted string
   (xcode-parser-read-unquoted-string)))

;;; Strings

(defvar xcode-parser-escape-chars
  '((?\" . ?\")
    (?\\ . ?\\)
    (?/ . ?/)
    (?b . ?\b)
    (?f . ?\f)
    (?n . ?\n)
    (?r . ?\r)
    (?t . ?\t))
  "Characters which may be escaped, with their elisp counterparts.")

;; String parsing

(defun xcode-parser-read-escaped-char ()
  "Read the PLIST string escaped character at point."
  ;; Skip over the '\'
  (xcode-parser-advance)
  (let* ((char (xcode-parser-pop))
         (special (assq char xcode-parser-escape-chars)))
    (cond
     (special (cdr special))
     ((not (eq char ?u)) char)
     ((looking-at "[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]")
      (let ((hex (match-string 0)))
        (xcode-parser-advance 4)
        (decode-char 'ucs (string-to-number hex 16))))
     (t
      (signal 'xcode-parser-string-escape (list (point)))))))

;; String parsing

(defun xcode-parser-read-quoted-string ()
  "Read the PLIST string at point."
  (let ((parts '())
        (more t)
        ;; strings may be quoted using single or double quotes
        (qchar (char-after))
        (chars-to-skip (if (eq (char-after) ?')
                           "^\\\\'"
                         "^\\\\\"")))
    ;; Skip over the '"'
    (xcode-parser-advance)
    (while more
      (let ((start (point)))
        (when (> (skip-chars-forward chars-to-skip) 0)
          (push (buffer-substring-no-properties start (point)) parts)))
      (let ((char (char-after)))
        (cond ((eq char qchar) (xcode-parser-advance) (setq more nil))
              ((eq char ?\\) (push (string (xcode-parser-read-escaped-char)) parts))
              (t (error "Unterminated string!")))))
    (if parts
        (if (cdr parts)
            (apply 'concat (nreverse parts))
          (car parts))
      "")))

(defvar xcode-parser-special-chars
  '(?\"
    ?'
    ?\\
    ?{
    ?}
    ?\(
    ?\)
    ?\,
    ?=
    ?\;)
  "Characters are special and not permitted in unquoted strings.")

(defun xcode-parser-read-unquoted-string ()
  "Read the PLIST string at point."
  (let (result
        (start (point)))
    ;; Read up to white space or any non-permitted char: (){},=;"'\
	(if (> (skip-chars-forward "^ ;,=\n\t\"'{}()\\\\") 0)
        (setq result (buffer-substring-no-properties start (point))))
    (unless result
      (error "Unquoted string has no value!"))
    result))

;;; PLIST Dictionaries

(defun xcode-parser-add-to-dict (key value object)
  "Add a new KEY -> VALUE association to OBJECT.
Returns the updated object, which you should save, e.g.:
    (setq obj (xcode-parser-add-to-dict \"foo\" \"bar\" obj))"
  (cons (cons (intern key) value) object))

;; PLIST dict parsing

(defun xcode-parser-read-dict ()
  "Read the PLIST dict at point."
  ;; Skip over the "{"
  (xcode-parser-advance)
  (xcode-parser-skip-whitespace-or-comment)
  ;; read key/value pairs until "}"
  (let ((elements '())
        key value (more t))
    (unless (eq (char-after) ?})
      (while more
        (setq key (xcode-parser-read))
        (unless key
          (signal 'xcode-parser-dict-format (list (point))))
        (xcode-parser-skip-whitespace-or-comment)
        (if (eq (char-after) ?=)
            (xcode-parser-advance)
          (signal 'xcode-parser-dict-format (list "=" (xcode-parser-peek))))
        (setq value (xcode-parser-read))
        (setq elements (xcode-parser-add-to-dict key value elements))
        (xcode-parser-skip-whitespace-or-comment)
        (if (eq (char-after) ?\;)
            (xcode-parser-advance)
          (signal 'xcode-parser-dict-format (list ";" (xcode-parser-peek))))
        (xcode-parser-skip-whitespace-or-comment)
        (if (eq (char-after) ?})
            (setq more nil))))
    ;; Skip over the "}"
    (xcode-parser-advance)
    elements))

;;; Arrays

;; Array parsing

(defun xcode-parser-read-array ()
  "Read the PLIST array at point."
  ;; Skip over the "("
  (xcode-parser-advance)
  (xcode-parser-skip-whitespace-or-comment)
  ;; read values until ")"
  (let (elements (more t))
    (unless (eq (char-after) ?\))
      (while more
        (push (xcode-parser-read) elements)
        ;; trailing quotes are permitted even at the end of the array.
        ;; so we test for both symbols each time.
        (let (term)
          (xcode-parser-skip-whitespace-or-comment)
          (if (eq (char-after) ?,)
              (progn (xcode-parser-advance)
                     (setq term t)))
          (xcode-parser-skip-whitespace-or-comment)
          (if (eq (char-after) ?\))
              (progn (setq more nil)
                     (setq term t)))
          (unless term
            (signal 'xcode-parser-array-format (list (point)))))))
    ;; Skip over the ")"
    (xcode-parser-advance)
    (if (eq xcode-parser-array-type 'list)
        (nreverse elements)
      (apply xcode-parser-array-type (nreverse elements)))
    ))

;;; PLIST reader.

(defvar xcode-parser-readtable
  (let ((table (make-char-table nil)))
    (aset table ?{ '(xcode-parser-read-dict))
    (aset table ?\( '(xcode-parser-read-array))
    (aset table ?\" '(xcode-parser-read-quoted-string))
    (aset table ?\' '(xcode-parser-read-quoted-string))
    (mapc (lambda (char)
	    (aset table char '(xcode-parser-read-number)))
          '(?- ?+ ?. ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    table)
  "Readtable for PLIST reader.")

(defun xcode-parser-read ()
  "Parse and return the property list object following point."
  (xcode-parser-skip-whitespace-or-comment)
  (if (char-after)
      (let ((record (aref xcode-parser-readtable (char-after))))
        (unless record
          (setq record '(xcode-parser-read-unquoted-string)))
        (if record
            (apply (car record) (cdr record))
          (signal 'xcode-parser-readtable-error record)))
    (signal 'end-of-file nil)))

;; Syntactic sugar for the reader

(defun xcode-parser-read-from-string (string)
  "Read the property list contained in STRING and return it."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (xcode-parser-read)))

(defun xcode-parser-read-file (file)
  "Read the property list contained in FILE and return it."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (xcode-parser-read)))

(provide 'xcode-parser)

;;; xcode-parser.el ends here
