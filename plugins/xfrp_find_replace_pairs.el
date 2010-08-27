;-*- coding: utf-8 -*-
;; xfrp_find_replace_pairs.el -- elisp utility for string replacement

;; Copyright © 2010 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Keywords: emacs lisp, string, find replace

;; You can redistribute this program and/or modify it under the terms
;; of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later
;; version.

;;; DESCRIPTION

;; this package is a emacs lisp utility.
;; It provides the following functions:

;; replace-pairs-in-string
;; replace-regexp-pairs-in-string
;; replace-pairs-region
;; replace-regexp-pairs-region
;; replace-pairs-in-string-recursive

;; These are convenient wrapper functions to emacs's replace-regexp-in-string

;; For some explanation of the need for these functions, see:
;;  http://xahlee.org/emacs/elisp_replace_string_region.html

;;; INSTALL

;; Place the file in your emacs load path. Then
;; (require 'xfrp_find_replace_pairs)

;;; HISTORY

;; version 1.0, 2010-08-17. First version.


;;; Code:

(defun replace-pairs-in-string (str pairs)
  "Replace string STR by find/replace PAIRS sequence.

Example:
 (replace-pairs-in-string \"abcdef\"
  '([\"a\" \"1\"] [\"b\" \"2\"] [\"c\" \"3\"]))  ⇒ “\"123def\"”.

The search strings are not case sensitive.
The replacement are literal and case sensitive.

If you want search strings to be case sensitive, set
case-fold-search to nil. Like this:

 (let ((case-fold-search nil)) 
   (replace-regexp-in-string-pairs ...)

Once a subsring in the input string is replaced, that part is not changed again.
For example, if the input string is “abcd”, and the pairs are
a → c and c → d, then, result is “cbdd”, not “dbdd”.
See also `replace-pairs-in-string-recursive'.

This function calls `replace-regexp-in-string' to do its work.

See also `replace-regexp-pairs-in-string'."
  (let (ii (mystr str) (randomStrList '()))
    (random t) ; set a seed

    ;; generate a random string list for intermediate replacement
    (setq ii 0)
    (while (< ii (length pairs))
      (setq randomStrList (cons
                    (concat "ㄓ" (number-to-string (random)) "ㄘ")
 ; use rarely used unicode char to prevent match in input string
                    randomStrList ))
      (setq ii (1+ ii))
      )

    ;; replace each find string by corresponding item in random string list
    (setq ii 0)
    (while (< ii (length pairs))
      (setq mystr (replace-regexp-in-string
                   (regexp-quote (elt (elt pairs ii) 0))
                   (elt randomStrList ii)
                   mystr t t))
      (setq ii (1+ ii))
      )

    ;; replace each random string by corresponding replacement string
    (setq ii 0)
    (while (< ii (length pairs))
      (setq mystr (replace-regexp-in-string
                   (elt randomStrList ii)
                   (elt (elt pairs ii) 1)
                   mystr t t))
      (setq ii (1+ ii))
      )
    
    mystr))

(defun replace-regexp-pairs-in-string (str pairs &optional fixedcase)
  "Replace string STR recursively by regex find/replace pairs PAIRS sequence.

Form:
 (replace-regexp-in-string-pairs
 '([REGEX1 REPLACE1] [REGEX2 REPLACE2] ...)
  FIXEDCASE)

The PAIRS can be any lisp sequence data type.

The third argument FIXEDCASE, if non-nil, changes the case of the replacement in a smart way matching the letter case of the find string.

If you want the regex to be case sensitive, set the global variable case-fold-search to nil. Like this: (let ((case-fold-search nil)) (replace-regexp-in-string-pairs ...)

This function calls `replace-regexp-in-string' to do its work.

See also `replace-pairs-in-string'."
  (let ((mystr str))
    (setq mystr str)
    (mapc
     (lambda (x) (setq mystr (replace-regexp-in-string
                              (elt x 0)
                              (elt x 1) mystr fixedcase)))
     pairs)
    mystr))


(defun replace-pairs-region (start end pairs)
  "Replace regex string find/replace PAIRS in region.

For detail, see `replace-pairs-in-string'."
  (let (mystr)
    (setq mystr (buffer-substring-no-properties start end))
    (delete-region start end)
    (insert (replace-pairs-in-string mystr pairs))))

(defun replace-regexp-pairs-region (start end pairs &optional fixedcase)
  "Replace regex string find/replace PAIRS in region.

For detail, see `replace-regexp-pairs-in-string'."
  (let (mystr)
    (setq mystr (buffer-substring-no-properties start end))
    (delete-region start end)
    (insert (replace-regexp-pairs-in-string mystr pairs fixedcase))))

(defun replace-pairs-in-string-recursive (str pairs)
  "Replace string STR recursively by find/replace pairs PAIRS sequence.

This function is similar to `replace-pairs-in-string', except that
the replacement is done recursively after each find/replace pair.
Earlier replaced value may be replaced again.

For example, if the input string is “abcd”, and the pairs are
a → c and c → d, then,

replace-pairs-in-string would return
“cbdd”
but replace-pairs-in-string-recursive would return
“dbdd”.

See `replace-pairs-in-string' for full doc."
  (let (mystr)
    (setq mystr str)
    (mapc
     (lambda (x) (setq mystr (replace-regexp-in-string
                              (regexp-quote (elt x 0))
                              (elt x 1) mystr t t)))
     pairs)
    mystr))

(provide 'xfrp_find_replace_pairs)
