;;; helm-j-cheatsheet.el --- Quick J reference for Emacs

;; Copyright (C) 2013  Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/helm-j-cheatsheet
;; Version: 20131209.1031
;; X-Original-Version: 0.1
;; Package-Requires: ((helm "1.5.3") (j-mode "1.0.0"))

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; The J cheat sheet for Emacs:
;; * look up a command by English name
;; * look up a command at http://www.jsoftware.com/help/dictionary/

;;; Code:

(require 'helm)
(require 'helm-match-plugin)
(require 'j-mode)

(defun jc-prep (s)
  "Fontify S for `helm-j-cheatsheet'."
  (replace-regexp-in-string
   "\\[[^]]*\\]"
   (lambda(x) (propertize (substring x 1 -1) 'face 'font-lock-keyword-face))
   s))

(defun jc-action-show-doc (x)
  "Extract from X the operation name and `j-help-lookup-symbol'."
  (j-help-lookup-symbol
   (and (string-match "^\\([^ ]+\\) " x)
        (match-string 1 x))))

(defvar helm-source-j-cheatsheet
  `(((name . "Adverbs")
     (candidates
      ,@(mapcar
         #'jc-prep
         '("~  [Reflex] • [Passive] / Evoke"
           "/  [Insert] • [Table]"
           "/. [Oblique] • [Key]"
           "\  [Prefix] • [Infix]"
           "\. [Suffix] • [Outfix]"
           "}  [Item Amend] • [Amend] (m} u})"
           "b. [Boolean] / [Basic]"
           "f. [Fix]"
           "M. [Memo]"
           "t. [Taylor Coeff.] (m t. u t.)"
           "t: [Weighted Taylor]")))
     (action . jc-action-show-doc))
    ((name . "Conjunctions")
     (candidates
      ,@(mapcar
         #'jc-prep
         '("^:  [Power] (u^:n u^:v)"
           ".   [Determinant] • [Dot Product]"
           "..  [Even]"
           ".:  [Odd]"
           ":   [Explicit]/[Monad-Dyad]"
           ":.  [Obverse]"
           "::  [Adverse]"
           ";.  [Cut]"
           "!.  [Fit]"
           "!:  [Foreign]"
           "\"   [Rank] (m\"n u\"n m\"v u\"v)"
           "`   [Tie] (Gerund)"
           "`:  [Evoke Gerund]"
           "@   [Atop]"
           "@.  [Agenda]"
           "@:  [At]"
           "&   [Bond] / [Compose]"
           "&.  [Under] (Dual)"
           "&.: [Under] (Dual)"
           "&:  [Appose]"
           "d.  [Derivative]"
           "D.  [Derivative]"
           "D:  [Secant Slope]"
           "H.  [Hypergeometric]"
           "L:  [Level At]"
           "S:  [Spread]"
           "T.  [Taylor Approximation]")))
     (action . jc-action-show-doc))
    ((name . "Verbs")
     (candidates
      ,@(mapcar
         #'jc-prep
         '("=   Self-Classify • Equal"
           "<   Box • Less Than"
           "<.  Floor • Lesser Of (Min)"
           "<:  Decrement • Less Or Equal"
           ">   Open • Larger Than"
           ">.  Ceiling • Larger of (Max)"
           ">:  Increment • Larger Or Equal"
           "_:  Infinity"
           "+   Conjugate • Plus"
           "+.  Real / Imaginary • GCD (Or)"
           "+:  Double • Not-Or"
           "*   Signum • Times"
           "*.  Length/Angle • LCM (And)"
           "*:  Square • Not-And"
           "-   Negate • Minus"
           "-.  Not • Less"
           "-:  Halve • Match"
           "%   Reciprocal • Divide"
           "%.  Matrix Inverse • Matrix Divide"
           "%:  Square Root • Root"
           "^   Exponential • Power"
           "^.  Natural Log • Logarithm"
           "$   Shape Of • Shape"
           "$.  Sparse"
           "$:  Self-Reference"
           "~.  Nub •"
           "~:  Nub Sieve • Not-Equal"
           "|   Magnitude • Residue"
           "|.  Reverse • Rotate (Shift)"
           "|:  Transpose"
           ",   Ravel • Append"
           ",.  Ravel Items • Stitch"
           ",:  Itemize • Laminate"
           ";   Raze • Link"
           ";:  Words • Sequential Machine"
           "#   Tally • Copy"
           "#.  Base 2 • Base"
           "#:  Antibase 2 • Antibase"
           "!   Factorial • Out Of"
           "/:  Grade Up • Sort"
           "\:  Grade Down • Sort"
           "[   Same • Left"
           "[:  Cap"
           "]   Same • Right"
           "{   Catalogue • From"
           "{.  Head • Take"
           "{:  Tail •"
           "{:: Map • Fetch"
           "}.  Behead • Drop"
           "}:  Curtail •"
           "\".  Do • Numbers"
           "\":  Default Format • Format"
           "?   Roll • Deal"
           "?.  Roll • Deal (fixed seed)"
           "A.  Anagram Index • Anagram"
           "C.  Cycle-Direct • Permute"
           "e.  Raze In • Member (In)"
           "E.  Member of Interval"
           "i.  Integers • Index Of"
           "i:  Steps • Index Of Last"
           "I.  Indices • Interval Index"
           "j.  Imaginary • Complex"
           "L.  Level Of"
           "o.  Pi Times • Circle Function"
           "p.  Roots • Polynomial"
           "p.. Poly. Deriv. • Poly. Integral"
           "p:  Primes"
           "q:  Prime Factors • Prime Exponents  "
           "r.  Angle • Polar"
           "s:  Symbol"
           "u:  Unicode"
           "x:  Extended Precision")))
     (action . jc-action-show-doc))))

;;;###autoload
(defun helm-j-cheatsheet ()
  "Use helm to show a J cheat sheet."
  (interactive)
  (helm :sources helm-source-j-cheatsheet))

(provide 'helm-j-cheatsheet)

;;; helm-j-cheatsheet.el ends here
