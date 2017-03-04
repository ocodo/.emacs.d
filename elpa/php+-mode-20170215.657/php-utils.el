;;; php-utils.el --- Miscellaneous helpful functions and keybindings

;; Version: 1.0
;; Created: 2011-07-19
;; Copyright © 2011 Michael Dwyer
;; Author(s):
;; Michael Dwyer <mdwyer@ehtech.in>

;;; *****
;;; About
;;; *****

;; php-utils.el was created as part of the php+-mode suite and contains
;; miscellaneous utility functions that do not have a home elsewhere,
;; but that are useful nonetheless and should not be cluttering up
;; users' .emacs.  We willprobably add defcustoms later so that people
;; may turn off these functionalities.

;; ********************************************************************

;; ************
;; REQUIREMENTS
;; ************

(require 'hideshow)

;; *********
;; FUNCTIONS
;; *********

(defun integer-range-intersectp (r0 r1)
  "This function determines whether two Ranges intersect.  The
ranges are of the form '(r-start r-end).  Since we are dealing
with integer ranges they are treated as inclusive.  The bounds of
the intersection will be returned or nil if none."
  (let* (r00 r01 r10 r11 i0 i1)
    (dolist (i '(0 1))
      (let* ((num (number-to-string i))
             (range (intern (concat "r" num))))
        (dolist (j '(0 1))
          (let ((sym (intern (concat (symbol-name range) 
                                     (number-to-string j)))))
            (setf (symbol-value sym) (elt (symbol-value range) j))))))
    (when (and (<= r00 r01) (<= r10 r11))
      (when (and (>= r00 r10) (<= r00 r11)) 
        (setf i0 r00)
        (setf i1 (min r01 r11)))
      (when (and (>= r01 r10) (<= r01 r11)) 
        (setf i1 r01)
        (setf i0 (max r00 r10)))
      (when (and (>= r10 r00) (<= r10 r01)) 
        (setf i0 r10)
        (setf i1 (min r11 r01)))
      (when (and (>= r11 r00) (<= r11 r01)) 
        (setf i1 r11)
        (setf i0 (max r10 r00)))
      (when (and (integerp i0) (integerp i1)) `(,i0 ,i1)))))

(defun separate-seq (seq pred)
  "Separates a copy of SEQ into two sequences, one that satisfies
PRED and one that does not.  Returns a list of '(satisfying
non-satisfying)."
  (let (sat non-sat)
    (dotimes (i (length seq) `(,(nreverse sat) ,(nreverse non-sat)))
      (let ((j (elt seq i)))
        (if (funcall pred j) (push j sat) (push j non-sat))))))

(defun inc-ints (x y)
  "Useful ``mapcar'' predicate for applying a numerical change to
applicable members of a list."
  (if (and (integerp x) (integerp y)) (+ x y) x))

(defun bind-key-if-fboundp (key-cmd func)
  (if (fboundp func)
      (global-set-key key-cmd func)))

(defun php-goto-line (line)
  "Goes to the given line, expanding the code if its hidden by hide-show."
  (interactive "nGoto Line: ")
  (goto-char (point-min))
  (forward-line (1- line))
  (when (and hs-minor-mode (get-char-property (point) 'hs))
    (hs-show-block)
    (goto-char (point-min))
    (forward-line (1- line))))

(provide 'php-utils)
