;;; helm-j-cheatsheet.el --- Quick J reference for Emacs

;; Copyright (C) 2013  Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/helm-j-cheatsheet
;; Version: 20131228.441
;; X-Original-Version: 0.2
;; Package-Requires: ((helm "1.5.3"))

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
;; * look up a command by English name (use C-g when done)
;; * look up a command at http://www.jsoftware.com/help/dictionary/ (use C-m)
;; * insert a command by English name (use C-e)

;;; Code:

(require 'helm)
(require 'helm-help)
(eval-when-compile
  (require 'cl))

(defgroup helm-j-cheatsheet nil
  "Quick J reference."
  :group 'helm
  :prefix "jc-")

(defcustom jc-make-insert-primary nil
  "When nil, \"show doc\" action is primary (C-m),
while \"insert\" action is secondary (C-e).
When t, the opposite."
  :group 'helm-j-cheatsheet
  :type 'boolean)

(defface jc-verb-face
    '((t (:foreground "#110099")))
  "Face for verbs."
  :group 'helm-j-cheatsheet)

(defface jc-conjunction-face
    '((t (:foreground "#7F0055" :weight bold)))
  "Face for conjunctions."
  :group 'helm-j-cheatsheet)

(defface jc-adverb-face
    '((t (:foreground "#BE314C")))
  "Face for adverbs."
  :group 'helm-j-cheatsheet)

(defconst jc-verbs
  '(("=" "Self-Classify" "Equal" "d000")
    ("<" "Box" "Less Than" "d010")
    ("<." "Floor" "Lesser Of (Min)" "d011")
    ("<:" "Decrement" "Less Or Equal" "d012")
    (">" "Open" "Larger Than" "d020")
    (">." "Ceiling" "Larger of (Max)" "d021")
    (">:" "Increment" "Larger Or Equal" "d022")
    ("_:" "Infinity" "\"" "d032")
    ("+" "Conjugate" "Plus" "d100")
    ("+." "Real / Imaginary" "GCD (Or)" "d101")
    ("+:" "Double" "Not-Or" "d102")
    ("*" "Signum" "Times" "d110")
    ("*." "Length/Angle" "LCM (And)" "d111")
    ("*:" "Square" "Not-And" "d112")
    ("-" "Negate" "Minus" "d120")
    ("-." "Not" "Less" "d121")
    ("-:" "Halve" "Match" "d122")
    ("%" "Reciprocal" "Divide" "d130")
    ("%." "Matrix Inverse" "Matrix Divide" "d131")
    ("%:" "Square Root" "Root" "d132")
    ("^" "Exponential" "Power" "d200")
    ("^." "Natural Log" "Logarithm" "d201")
    ("$" "Shape Of" "Shape" "d210")
    ("$." "Sparse" "\"" "d211")
    ("$:" "Self-Reference" "" "d212")
    ("~." "Nub" "" "d221")
    ("~:" "Nub Sieve" "Not-Equal" "d222")
    ("|" "Magnitude" "Residue" "d230")
    ("|." "Reverse" "Rotate (Shift)" "d231")
    ("|:" "Transpose" "\"" "d232")
    ("," "Ravel" "Append" "d320")
    (",." "Ravel Items" "Stitch" "d321")
    (",:" "Itemize" "Laminate" "d322")
    (";" "Raze" "Link" "d330")
    (";:" "Words" "Sequential Machine" "d332")
    ("#" "Tally" "Copy" "d400")
    ("#." "Base 2" "Base" "d401")
    ("#:" "Antibase 2" "Antibase" "d402")
    ("!" "Factorial" "Out Of" "d410")
    ("/:" "Grade Up" "Sort Up" "d422")
    ("\\:" "Grade Down" "Sort Down" "d432")
    ("[" "Same" "Left" "d500")
    ("[:" "Cap" "\"" "d502")
    ("]" "Same" "Right" "d500")
    ("{" "Catalogue" "From" "d520")
    ("{." "Head" "Take" "d521")
    ("{:" "Tail" "" "d522")
    ("{::" "Map" "Fetch" "d523")
    ("}." "Behead" "Drop" "d531")
    ("}:" "Curtail" "" "d532")
    ("\"." "Do" "Numbers" "d601")
    ("\":" "Default Format" "Format" "d602")
    ("?" "Roll" "Deal" "d640")
    ("?." "Roll" "Deal (fixed seed)" "d641")
    ("A." "Anagram Index" "Anagram" "dacapdot")
    ("C." "Cycle-Direct" "Permute" "dccapdot")
    ("e." "Raze In" "Member (In)" "dedot")
    ("E." "" "Member of Interval" "decapdot")
    ("i." "Integers" "Index Of" "didot")
    ("i:" "Steps" "Index Of Last" "dico")
    ("I." "Indices" "Interval Index" "dicapdot")
    ("j." "Imaginary" "Complex" "djdot")
    ("L." "Level Of" "" "dlcapdot")
    ("o." "Pi Times" "Circle Function" "dodot")
    ("p." "Roots" "Polynomial" "dpdot")
    ("p.." "Poly. Deriv." "Poly. Integral" "dpdotdot")
    ("p:" "Primes" "\"" "dpco")
    ("q:" "Prime Factors" "Prime Exponents" "dqco")
    ("r." "Angle" "Polar" "drdot")
    ("s:" "Symbol" "\"" "dsco")
    ("u:" "Unicode" "\"" "duco")
    ("x:" "Extended Precision" "Num/Denom" "dxco")))

(defconst jc-adverbs
  '(("~" "Reflex" "Passive" "d220v" "Evoke" "d220n")
    ("/" "Insert" "Table" "d420")
    ("/." "Oblique" "Key" "d421")
    ("f." "Fix" "" "dfdot")
    ("\\" "Prefix" "Infix" "d430")
    ("\\." "Suffix" "Outfix" "d431")
    ("}" "Item Amend" "Amend" "d530n" "m} / u}" "d530v")
    ("b." "Boolean" "\"" "dbdotn")
    ("b." "Basic" "" "dbdotu")
    ("M." "Memo" "" "dmcapdot")
    ("t." "Taylor Coeff." "" "dtdotu" "u t. / m t." "dtdotm")
    ("t:" "Weighted Taylor" "" "dtco")))

(defconst jc-conjunctions
  '(("^:" "Power" "" "d202n" "u^:n / u^:v" "d202v")
    (".;" "Determinant" "Dot Product" "d300")
    (".." "Even" "" "d301")
    (".:" "Odd" "" "d301")
    (":" "Explicit" "" "d310n" "Monad-Dyad" "d310v")
    (":." "Obverse" "" "d311")
    ("::" "Adverse" "" "d312")
    (";." "Cut" "\"" "d331")
    ("!." "Fit (Customize)" "" "d411")
    ("!:" "Foreign" "" "d412")
    ("\"" "Rank" "" "d600n" "m\"n / u\"n" "d600v" "m\"v u\"v" "d600xv")
    ("`" "Tie" "" "d610")
    ("`:" "Evoke Gerund" "" "d612")
    ("@:" "At" "" "d622")
    ("@" "Atop" "" "d620")
    ("@." "Agenda" "" "d621")
    ("&" "Bond" "" "d630n" "Compose" "d630v")
    ("&." "Under" "" "d631")
    ("&.:" "Under" "" "d631c")
    ("&:" "Appose" "" "d632")
    ("d." "Derivative" "" "dddot")
    ("D." "Derivative" "" "ddcapdot")
    ("D:" "Secant Slope" "" "ddcapco")
    ("H." "Hypergeometric" "" "dhcapdot")
    ("L:" "Level At" "" "dlcapco")
    ("S:" "Spread" "" "dscapco")
    ("T." "Taylor Approximation" "" "dtcapdot")))

(defconst jc-others
  '(("=." "Is (Local)" "" "d001")
    ("=:" "Is (Global)" "" "d001")
    ("_" "Negative Sign / Infinity" "" "d030")
    ("_." "Indeterminate" "" "d031")
    ("a." "Alphabet" "" "dadot")
    ("a:" "Ace" "" "dadot")
    ("NB." "Comment" "" "dnb")))

(defun jc-action-show-doc (x)
  "Look up X doc on the internet."
  (let ((url (format "http://www.jsoftware.com/help/dictionary/%s.htm" (caadr x))))
    (message "Loading %s ..." url)
    (browse-url url)))

(defun jc-action-show-2nd-doc (x)
  "Look up 2nd doc for X on the internet."
  (let ((bit (caddr (cadr x)))
        (n "2nd")
        url)
    (unless bit
      (setq bit (caadr x)
            n "1st"))
    (message
     "Loading %s doc %s ..." n
     (setq url (format "http://www.jsoftware.com/help/dictionary/%s.htm" bit)))
    (browse-url url)))

(defun jc-action-insert (x)
  "Insert X."
  (insert (car x)))

(defun jc-action-transformer (actions candidate)
  "Action transformer for `helm-source-j-cheatsheet'."
  (if jc-make-insert-primary
      (mapcar `(lambda (x) (nth x ',actions)) '(1 0 2))
    actions))

(defun jc-candidates (name lst face)
  "Generate a section for `helm-source-j-cheatsheet'.
NAME is the section name, LST holds the candidate columns
and FACE propertizes them."
  `((name . ,name)
    (candidates
     ,@(mapcar
        (lambda(x)
          (list
           (format
            "% -3s % -18s % -18s %s"
            (car x)
            (propertize (cadr x) 'face face)
            (propertize (caddr x) 'face face)
            (if (nth 4 x) (propertize (nth 4 x) 'face face) ""))
           (nth 0 x)
           (cl-subseq x 3)))
        lst))
    (action . (("Show 1st doc" . jc-action-show-doc)
               ("Insert" . jc-action-insert)
               ("Show 2nd doc" . jc-action-show-2nd-doc)))
    (action-transformer . jc-action-transformer)
    (pattern-transformer . regexp-quote)))

(defvar helm-source-j-cheatsheet
  (list
   (jc-candidates "Adverbs" jc-adverbs 'jc-adverb-face)
   (jc-candidates "Conjunctions" jc-conjunctions 'jc-conjunction-face)
   (jc-candidates "Verbs" jc-verbs 'jc-verb-face)
   (jc-candidates "Others" jc-others nil)))

;;;###autoload
(defun helm-j-cheatsheet ()
  "Use helm to show a J cheat sheet."
  (interactive)
  (let (helm-update-blacklist-regexps)
    (helm :sources helm-source-j-cheatsheet)))

(provide 'helm-j-cheatsheet)

;;; helm-j-cheatsheet.el ends here
