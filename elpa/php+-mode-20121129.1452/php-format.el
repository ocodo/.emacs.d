;;; php-format.el --- Functions that deal with PHP formatting

;; Version: 2.0
;; Created: 07-29-2011
;; Copyright © 2011 Michael Dwyer
;; Author(s): 
;; Michael Dwyer <mdwyer@ehtech.in>

;;; *****
;;; About
;;; *****

;; php-format.el is a part of the php+-mode suite and contains
;; convenience functions for PHP formatting, such as re-ordering the
;; constants, properties and methods withing a class and line
;; breaking.

;; ********************************************************************

;; ************
;; REQUIREMENTS
;; ************
(require 'php-parse)
(require 'php-structure)

;; *********
;; CUSTOMIZE
;; *********

(defcustom php-blank-line-at-start-of-class nil
  "Whether you like to have a blank line between the opening
brace of a class and the next line of code.  Also applies to
interfaces."
  :group 'php-format
  :type 'boolean)

(defcustom php-blank-line-at-end-of-class nil
  "Whether you like to have a blank line between the closing
brace of a class and the previous line of code.  Also applies to
interfaces."
  :group 'php-format
  :type 'boolean)

(defcustom php-auto-fill nil
  "Whether to make use of the php-auto-fill functions."
  :group 'php-format
  :type 'boolean)

(defcustom php-format-align-array-double-arrows nil
  "Whether to align arrays at the double-arrows."
  :group 'php-format
  :type 'boolean)

(defcustom php-format-break-anonymous-function-always t
  "Whether you like to have statements in anonymous functions
broken even if the entire definition and implementation fit on
the current line."
  :group 'php-format
  :type 'boolean)

(defcustom php-format-break-all-method-chain-links nil
  "Whether you like to have method chains broken at each link (t) or only to
fit 80 character line length (nil)."
  :group 'php-format
  :type 'boolean)

(defcustom php-format-break-string-before-white-space nil
  "Whether to break a string before or after white-space."
  :group 'php-format
  :type 'boolean)

(defcustom php-format-break-all-method-call-arguments nil
  "Whether to break all method call arguments even if they fit on
one line."
  :group 'php-format
  :type 'boolean)

(defcustom php-format-break-function-call-like-definition t
  "Whether to break function definition parameters one per line
just like function calls.  This was added to abide by PSR-2."
  :group 'php-format
  :type 'boolean)

(defcustom php-format-break-all-for-semicolons nil
  "Whether to break on subsequent semicolons of a for statement
always after breaking on the first semicolon, even if the rest of
the expression fits on the same line."
  :group 'php-format
  :type 'boolean)  

(defcustom php-format-break-all-ternary-parts nil
  "Whether to break on the colon of a ternary always after
breaking on the question mark, even if the rest of the expression
fits on the same line."
  :group 'php-format
  :type 'boolean)  

(defcustom php-format-break-all-operators nil
  "Whether to break at all operators (boolean, logical and
arithmetic) even if they fit on one line."
  :group 'php-format
  :type 'boolean)

(defcustom php-format-break-all-assignment-operators nil
  "Whether to break at all assignment operators even if they fit
on one line."
  :group 'php-format
  :type 'boolean)

(defcustom php-format-break-start-line-with-concat t
  "Whether to start a new line with the concatenate operator when
breaking strings."
  :group 'php-format
  :type 'boolean)

(defcustom php-force-single-quoted-strings nil
  "Whether to auto-convert strings to single-quoted when running
script cleanup functions."
  :group 'php-format
  :type 'boolean)

;; *********
;; FUNCTIONS
;; *********
; declarations for compiler
(declare-function php-jump-to-first-statement "php-funcs")
(declare-function php-jump-to-first-class/interface "php-funcs")
(declare-function php-kill-current "php-edit")
(declare-function php-yank "php-edit")
(declare-function php-format-spacing "php-format")
(declare-function php-change-string-quotes "php-string")
(declare-function php-implode-concats-in-statement "php-string")
(declare-function php+-mode "php+-mode")
(declare-function hs-already-hidden-p "hideshow")
(declare-function hs-hide-block "hideshow")

(defun php-rearrange-current (&optional type sit-for)
  "Kills the current TYPE PHP structure and yanks it into its
proper position.  TYPE follows the semantics of
php-parse-current.  Optionally, you may tell it how long to
SIT-FOR at each statement.  If SIT-FOR is nil, will not sit at
all."
  (interactive)
  (let* ((type (or type '(constant property method)))
         (parse (php-parse-current type))
         current-doc-hs-status current-innard-hs-status)
    (when (php-parse-p parse)
      (save-excursion
        (goto-char (rest (assoc 'begin parse)))
        (when (and hs-minor-mode (hs-already-hidden-p))
          (setf current-doc-hs-status t))
        (php-jump-to-first-statement nil t)
        (setf current-innard-hs-status 
              (and hs-minor-mode (hs-already-hidden-p))))
      (php-kill-current type)
      (let* ((type (rest (assoc 'type parse)))
             (name (rest (assoc 'name parse)))
             (visibility (rest (assoc 'visibility parse)))
             (staticp (rest (assoc 'staticp parse)))
             (abstractp (rest (assoc 'abstractp parse)))
             (finalp (rest (assoc 'finalp parse)))
             (pos (php-find-struct-position type name visibility staticp 
                                            abstractp finalp)))
        (goto-char pos)
        (when (looking-at-p "}")
          (re-search-backward non-ws-re nil t)
          (forward-char)
          (delete-region (point) pos)
          (newline 3)
          (forward-line -1)
          (indent-according-to-mode))
        (php-yank)
        (when (integerp sit-for)
          (sit-for sit-for))
        (save-excursion
          (goto-char pos)
          (when (and hs-minor-mode current-doc-hs-status) (hs-hide-block))
          (php-jump-to-first-statement nil t)
          (when (and hs-minor-mode current-innard-hs-status)
            (hs-hide-block)))))))

(defun php-update-innards-bounds (innards start delta updatep decision-point)
  "Auxillary function to update innards bounds."
  (dotimes (j (- (length innards) start) innards)
    (let* ((j* (+ start j))
           (j-innard (elt innards j*))
           (j-begin (rest (assoc 'begin j-innard)))
           (j-end (rest (assoc 'end j-innard))))
      (when (funcall updatep j-innard decision-point)
        (setf (rest (assoc 'begin (elt innards j*)))
              (+ j-begin delta)
              (rest (assoc 'end (elt innards j*)))
              (+ j-end delta))))))

(defun php-innard-beforep (innard decision-point)
  "Auxillary function to decide whether an innard is before a certain point."
  (<= (rest (assoc 'end innard)) decision-point))

(defun php-innard-afterp (innard decision-point)
  "Auxillary function to decide whether an innard is after a certain point."
  (>= (rest (assoc 'begin innard)) decision-point))

(defun php-rearrange-innards (&optional no-fix-spacing sit-for)
  "Rearrange all of the constants. properties and methods in the
current class or interface.  If NO-FIX-SPACING is non-nil, don't
run ``php-format-spacing'' afterwards.  Optionally, you may tell
it how long to SIT-FOR at each change.  If SIT-FOR is nil, will
not sit at all."
  (interactive)
  (save-excursion
    (let ((top-struct (php-parse-current '(class interface))))
      (when (php-parse-p top-struct)
        (let* ((constants (rest (assoc 'constants top-struct)))
               (properties (rest (assoc 'properties top-struct)))
               (methods (rest (assoc 'methods top-struct)))
               (innards (sort (append constants properties methods)
                              'php-parse<)))
          (php-jump-to-first-statement top-struct)
          (let ((next-insert (point)))
            (dotimes (i (1- (length innards)))
              (let* ((current-innard (elt innards i))
                     (current-type (rest (assoc 'type current-innard)))
                     current-doc-hs-status current-innard-hs-status)
                (goto-char (rest (assoc 'begin current-innard)))
                (when (looking-at-p ws-re)
                  (re-search-forward non-ws-re nil t))
                (when (and hs-minor-mode (hs-already-hidden-p))
                  (setf current-doc-hs-status t))
                (save-excursion
                  (php-jump-to-first-statement nil t)
                  (setf current-innard-hs-status 
                        (and hs-minor-mode (hs-already-hidden-p))))
                (let* ((old-innard (php-kill-current current-type t))
                       (old-begin (rest (assoc 'begin old-innard)))
                       (old-end (rest (assoc 'end old-innard)))
                       (deleted-length (- old-end old-begin))
                       (innards (php-update-innards-bounds innards
                                                           (1+ i)
                                                           (- deleted-length)
                                                           'php-innard-afterp
                                                           (point))))
                  (goto-char next-insert)
                  (setf (rest (assoc 'begin (elt innards i))) (point))
                  (php-yank nil t t)
                  (when (integerp sit-for)
                    (sit-for sit-for))
                  (save-excursion
                    (let ((innard (elt innards i)))
                      (goto-char (rest (assoc 'begin innard)))
                      (when (and hs-minor-mode current-doc-hs-status)
                        (hs-hide-block))
                      (when (and hs-minor-mode current-innard-hs-status)
                        (php-jump-to-first-statement innard t)
                        (hs-hide-block))))
                  (when (not (looking-back-p (concat "^" ws-re "*")))
                    (newline)
                    (newline-and-indent))
                  (let ((inserted-length (- (point) next-insert)))
                    (setf (rest (assoc 'end (elt innards i))) (point))
                    (setq innards 
                          (php-update-innards-bounds innards 
                                                     (1+ i) 
                                                     inserted-length
                                                     'php-innard-afterp
                                                     next-insert))
                    (setq next-insert (point)))))))))))
  (unless no-fix-spacing
    (php-format-spacing)))

(defun php-format-break-statement (&optional strip-newlines-in-string 
                                             max-length begin end verbose)
  "This function breaks the current PHP statement.  Lines will be
kept below MAX-LENGTH.  Optionally, start at BEGIN and end at
END.  You may also STRIP-NEWLINES-IN-STRINGs with a prefix arg.
You may also tell it to be VERBOSE by using the parameter or
setting php-verbose to true."
  (interactive "P")
  (let ((start-pos (point)))
    (when (looking-at-p (concat ws-re "*$"))
      (re-search-backward non-ws-re nil t))
    (when (and (looking-back-p (concat "^" ws-re "*"))
               (re-search-forward non-ws-re nil t))
      (backward-char))
    (let ((in-comment (php-in-commentp)))
      (if (integerp in-comment)
          (let* ((comment-end (save-excursion (php-skip-this-text-struct)))
                 (old-end comment-end))
            (goto-char in-comment)
            (while (< (point) comment-end)
              (setf comment-end (+ comment-end 
                                   (indent-according-to-mode)))
              (forward-line))
            (goto-char start-pos)
            (- comment-end old-end))
        (let ((verbose (or (and (boundp 'php-verbose) php-verbose) verbose))
              (begin (if (integerp begin) begin (php-in-statementp))))
          (when begin
            (goto-char begin)
            (unless (looking-back-p (concat "^" ws-re "*"))
              (indent-according-to-mode))
            (let ((change
                   (if (or (looking-at-p "<\\?\\(php\\|=\\)?") 
                           (looking-at-p "\\?>"))
                       0
                     (let* ((max-length (or max-length 80))
                            (end (or end (save-excursion 
                                           (php-skip-this-statement))))
                            (old-end end)
                            (indent-gap (indent-according-to-mode))
                            (begin (+ begin indent-gap))
                            (begin-column (current-column))
                            (end (+ end indent-gap))
                            (bounds 
                             (php-format-strip-newlines 
                              `((,end 1)) strip-newlines-in-string))
                            (broke-once nil)
                            (bound (first (first bounds)))
                            (bound (- bound (php-implode-concats-in-statement)))
                            (in-control (php-in-control-statementp))
                            (in-function-def (php-function-definitionp))
                            (ternary-regexp 
                             "\\(?1:\\?\\)[^:]\\|[^?:]\\(?1::\\)[^:]")
                            (accessor-regexp (php-type-regexp 'accessor))
                            (operator-regexp (php-type-regexp 'operator))
                            (assignment-operator-regexp 
                             (php-type-regexp 'assignment-operator))
                            last-point
                            (statement-text 
                             (buffer-substring-no-properties begin bound)))
                       (php-format-break-statement-text verbose)
                       (php-delete-horizontal-space)
                       (unless (looking-at-p (concat ws-re "*$"))
                         (newline-and-indent))
                       (- (point) old-end)))))
              (goto-char start-pos)
              change)))))))

(defun php-format-break-statement-text (&optional in-buffer)
  "Function that takes ``statement-text'' and breaks it in a temp
buffer, returning the broken statement.  Can be told to perform
everything IN-BUFFER."
  (cond (in-buffer (php-format-break-statement-loops)
                   (goto-char bound))
        (t (goto-char bound)
           (insert (chomp (with-temp-buffer
                            (php+-mode)
                            (insert "<?php")
                            (newline)
                            (dotimes (i (/ begin-column php-basic-offset))
                              (indent-according-to-mode)
                              (insert "if ($test) {\n"))
                            (indent-according-to-mode)
                            (save-excursion
                              (insert statement-text))
                            (let* ((delta (- begin (point)))
                                   (begin (point))
                                   (bound (- bound delta))
                                   (end (- end delta)))
                              (php-format-break-statement-loops)
                              (buffer-substring begin (point-max))))))
           (save-excursion
             (delete-region begin bound)))))

(defun php-format-break-statement-loops ()
  "Actual breaking loops for php-format-break-statement."
  (dolist (break-regexp `((boolean+ . ,(concat "&&\\|||\\|" ws-re "+as" 
                                               ws-re "+\\|=>\\|"
                                               "[^.+-*/%&|^<>!=]\\(?1:=\\)"
                                               "[^=>]"))
                          (acc1 . ,accessor-regexp)
                          (concats . "\\(?1:\\.\\)[^=]")
                          (commas . ",")
                          (sexp1 . "[{()};]")
                          (concats+commas . "\\(?1:\\.\\)[^=]\\|,")
                          (sexp2 . "[(,);]")
                          (tern1 . ,ternary-regexp)
                          (quotes . "['\"]")
                          (ops1 . ,operator-regexp)
                          (acc2 . ,accessor-regexp)
                          (ass-ops 
                           . ,assignment-operator-regexp)
                          (tern2 . ,ternary-regexp)
                          (ops2 . ,operator-regexp)
                          (brackets . "[][]")))
    (let ((pass-name (first break-regexp))
          (break-regexp (rest break-regexp)))
      (setf last-point -1)
      (goto-char begin)
      (while (and (< (point) bound) 
                  (re-search-forward break-regexp bound t))
        (catch 'skip
          (when verbose (message "While point: %s" (point)))
          (when (<= (point) last-point)
            (when verbose
              (message (concat "Infinite loop %s: %s <= %s detected in "
                               "php-format-break-statement.  Advancing "
                               "through statement.") 
                       pass-name (point) last-point))
            (forward-char)
            (throw 'skip t))
          (setf last-point (point))
          (let* ((sexp-level (php-get-current-sexp-level))
                 (whole-match (match-string-no-properties 0))
                 (whole-match-begin (match-beginning 0))
                 (whole-match-end (match-end 0))
                 (i (if (match-string-no-properties 1) 1 0))
                 (best-match (match-string-no-properties i))
                 (best-match-begin (match-beginning i))
                 (best-match-end (match-end i))
                 (best-match-length (- best-match-end best-match-begin)))
            (when verbose
              (message "Whole-match %s - %s (%s)" whole-match-begin
                       whole-match-end whole-match)
              (message "Best-match %s - %s (%s)" best-match-begin
                       best-match-end best-match))
            (let ((string-start (php-in-text-structp))
                  (looking-at-quote (or (string= best-match "'")
                                        (string= best-match "\""))))
              (when (or (and looking-at-quote
                             (wholenump string-start)
                             (< string-start (1- (point))))
                        (and (or (not looking-at-quote) (not string-start))
                             (php-in-text-structp nil best-match-begin 
                                                  (1- best-match-end))))
                (goto-char best-match-end)
                (throw 'skip t)))
            (when (eq pass-name 'tern2)
              (when php-format-break-all-ternary-parts
                (php-format-break-statement-ternary-aux))
              (throw 'skip t))
            (when (or (and (eq pass-name 'boolean+)
                           (or in-control (not (string= best-match "=>"))))
                      (and (eq pass-name 'tern1)
                           (string-match-p ternary-regexp whole-match))
                      (and (or (and (or (eq pass-name 'ops1)
                                        (eq pass-name 'ops2))
                                    (not in-control))
                               (and (member pass-name '(concats concats+commas))
                                    (or in-control (<= sexp-level 1))))
                           (string-match-p operator-regexp whole-match))
                      (and (eq pass-name 'ass-ops)
                           (string-match-p assignment-operator-regexp 
                                           whole-match)))
              (when (string= best-match ".")
                (save-excursion
                  (goto-char best-match-begin)
                  (when (looking-back-p "^[[:space:]]*")
                    (let ((cur-line (line-number-at-pos)))
                      (end-of-line 0)
                      (when (not (= cur-line (line-number-at-pos)))
                        (let ((gap (php-format-slurp-operands
                                    max-length "\\." 
                                    best-match-begin)))
                          (setf last-point (+ last-point gap)
                                best-match-begin (+ best-match-begin gap)
                                best-match-end (+ best-match-end gap)
                                bound (+ bound gap))))))))
              (php-format-break-statement-operator-aux)
              (goto-char best-match-end)
              (throw 'skip t))
            (when (or (and (eq pass-name 'acc1)
                           (zerop (php-get-current-sexp-level)))
                      (eq pass-name 'acc2))
              (php-format-break-statement-accessor-aux)
              (throw 'skip t))
            (when (looking-back-p "\\.")
              (when (or (not (eq pass-name 'concats+commas)) 
                        (<= sexp-level 1))
                (php-format-break-statement-concat-aux))
              (throw 'skip t))
            (when (or (looking-back-p "'") 
                      (looking-back-p "\""))
              (php-format-break-statement-quote-aux)
              (forward-char)
              (when (looking-at-p "[[:space:]]*$")
                (let ((gap (php-format-slurp-operands max-length "\\.")))
                  (setf bound (+ bound gap))))
              (throw 'skip t))
            (when (looking-back-p "{")
              (php-format-break-statement-opening-brace-aux)
              (throw 'skip t))
            (when (or (and (looking-back-p "(")
                           (or (eq pass-name 'sexp1) (not in-function-def)))
                      (looking-back-p "\\["))
              (php-format-break-statement-opening-parenthesis-aux)
              (throw 'skip t))
            (when (looking-back-p ";")
              (php-format-break-statement-semicolon-aux)
              (throw 'skip t))
            (when (and (looking-back-p ",")
                       (or (zerop (php-get-current-sexp-level))
                           (not (eq pass-name 'commas))))
              (php-format-break-statement-commas-aux)
              (throw 'skip t))
            (when (or (looking-back-p ")")
                      (looking-back-p "]")
                      (looking-back-p "}"))
              (php-format-break-statement-closing-parenthesis-aux)
              (throw 'skip t))))))))

(defun php-format-slurp-operands (&optional max-length operator-regexp
                                            change-point)
  "Run ``php-format-slurp-next-operand'' until it returns 0.
Return the total change in text size.  You may tell it that you
only care about the movement of CHANGE-POINT."
  (let ((total-change 0))
    (catch 'done
      (while t
        (let ((change (php-format-slurp-next-operand max-length 
                                                     operator-regexp 
                                                     change-point)))
          (when (zerop change) (throw 'done total-change))
          (setf total-change (+ total-change change)
                change-point (+ change-point change)))))
    total-change))

(defun php-format-slurp-next-operand (&optional max-length operator-regexp 
                                                change-point)
  "This function looks at the space left between the end of this
line and MAX-LENGTH, and the length of next operand on the line
below.  If it is short enough, it will slurp it up.  Returns the
change in text size.  You may tell it that you only care about
the movement of CHANGE-POINT."
  (let ((max-length (if (wholenump max-length) max-length 80))
        (operator-regexp (or operator-regexp 
                             (concat (php-type-regexp 'operator) "\\|"
                                     (php-type-regexp 'assignment-operator))))
        (cur-line (line-number-at-pos))
        (change-point (if (wholenump change-point) change-point (point)))
        (change 0))
    (save-excursion
      (end-of-line)
      (let ((gap (php-delete-horizontal-space)))
        (when (<= (point) change-point) 
          (setf change gap change-point (+ change-point gap))))
      (when (looking-at-p (concat ws-re "*" operator-regexp))
        (let ((end-col-1 (current-column)))
          (beginning-of-line-non-whitespace 2)
          (when (> (line-number-at-pos) cur-line)
            (let ((start-col (current-column)))
              (save-match-data
                (when (looking-at operator-regexp)
                  (goto-char (if (match-end 1) (match-end 1) (match-end 0)))
                  (unless (looking-at-p non-ws-re)
                    (re-search-forward non-ws-re (line-end-position) t)
                    (backward-char)))
                (let ((bounds (php-atom-bounds)))
                  (when (and bounds (not (looking-at-p "[]})]")))
                    (goto-char (second bounds))
                    (when (looking-at-p "[,;]")
                      (forward-char))
                    (let ((operand-length (- (current-column) start-col)))
                      (when (and (= (line-number-at-pos)
                                    (1+ cur-line))
                                 (< (+ end-col-1 1 operand-length) max-length))
                        (if (looking-at "\\([[:space:]]\\)*$")
                            (let ((b (match-beginning 1))
                                  (e (match-end 1)))
                              (when (and (integerp b) (integerp e))
                                (let ((gap (- b e)))
                                  (when (<= (point) change-point)
                                    (setf change (+ change gap)
                                          change-point (+ change gap))))
                                (delete-region b e)))
                          (save-excursion
                            (let ((gap (1+ (newline-and-indent))))
                              (when (<= (point) change-point)
                                (setf change (+ change gap)
                                      change-point (+ change-point gap))))))
                        (beginning-of-line-non-whitespace)
                        (let ((gap (php-delete-horizontal-space t)))
                          (when (<= (point) change-point)
                            (setf change (+ change gap)
                                  change-point (+ change-point gap))))
                        (backward-delete-char 1)
                        (insert " ")))))))))))
    change))
                    
(defun php-format-break-string (&optional strip-newlines max-length begin end)
  "This function breaks a PHP string with the proper quotes and
concatenation operator at a maximum length of
MAX-LENGTH. Optionally start at BEGIN and stop at END. Returns
number of characters added.  Optionally STRIP-NEWLINES as well."
  (interactive "P")
  (let* ((max-length (or max-length 80))
         (statement-begin-col (save-excursion 
                                (goto-char (php-in-statementp))
                                (current-column)))
         (begin (if (integerp begin) begin (or (php-in-stringp) 
                                               (line-beginning-position)))))
    (save-excursion
      (save-match-data
        (goto-char begin)
        (let* ((end (if (integerp end) 
                        end 
                      (save-excursion 
                        (forward-sexp)
                        (if (looking-at-p "[];)]")
                            (point)
                          (1- (point))))))
               (old-end end))
          (catch 'done
            (when (>= (current-column) max-length)
              (setq end (+ end (newline-and-indent))))
            (let* ((this-quote (char-after))
                   (this-string-begin begin))
              (while (< (point) (1- end))
                (forward-char)
                (while (and strip-newlines (looking-at-p "\n"))
                  (delete-char 1)
                  (setf end (1- end)))
                (when (or (= (current-column) (1- max-length))
                          (looking-at-p "\n"))
                  (if (not (looking-at-p "\n"))
                      (re-search-backward ws-re this-string-begin t)
                    (when (= this-quote ?\")
                      (insert "\\n")
                      (setf end (+ 2 end))))
                  (when (not php-format-break-string-before-white-space)
                    (re-search-forward non-ws-re nil t)
                    (backward-char))
                  (when (and (not php-format-break-start-line-with-concat)
                             (>= (current-column) (- max-length 3)))
                    (re-search-backward ws-re this-string-begin t))
                  (when (= (char-after) this-quote)
                    (throw 'done t))
                  (insert this-quote)
                  (insert " .")
                  (setf end (+ 3 end))
                  (when php-format-break-start-line-with-concat
                    (insert " ")
                    (setf end (1+ end)))
                  (when (and (looking-at-p "\n")
                             (= this-quote ?\"))
                    (delete-char 1)
                    (setf end (1- end)))
                  (insert this-quote)
                  (setf end (1+ end))
                  (when (and (looking-at-p "\n")
                             (= this-quote ?\"))
                    (insert "\\n")
                    (delete-char 1)
                    (setf end (1+ end)))
                  (backward-char (if php-format-break-start-line-with-concat
                                     3 1))
                  (when (looking-back-p ws-re)
                    (backward-char)
                    (setf end (+ end (php-delete-horizontal-space))))
                  (setq end (+ end (newline-and-indent)))
                  (re-search-forward (char-to-string this-quote))
                  (setf this-string-begin (point))))
              (forward-char 1)))
          (- end old-end))))))

(defun php-format-break-statement-ternary-aux ()
  "Auxillary function used by ``php-format-break-statement''."
  (let* ((ternary-bounds (php-get-ternary-bounds))
         (start-line (save-excursion
                       (goto-char (first ternary-bounds))
                       (line-number-at-pos)))
         (q-line (save-excursion
                   (goto-char (third ternary-bounds))
                   (line-number-at-pos)))
         (c-line (save-excursion
                   (goto-char (fourth ternary-bounds))
                   (line-number-at-pos))))
    (if (or (and (= start-line q-line) (= q-line c-line))
            (not (or (= start-line q-line) (= q-line c-line))))
        (goto-char (second ternary-bounds))
      (let ((break-point (if (= start-line q-line) 
                             (third ternary-bounds) 
                           (fourth ternary-bounds))))
        (goto-char break-point)
        (let ((gap (newline-and-indent)))
          (setf bound (+ bound gap))
          (goto-char (+ gap (second ternary-bounds))))))))

(defun php-format-break-statement-operator-aux ()
  "Auxillary function used by ``php-format-break-statement''."
  (unless (and in-function-def (not (string= best-match "=")))
    (goto-char best-match-begin)
    (when (looking-back-p "^[[:space:]]*")
      (let ((gap (indent-according-to-mode)))
        (setf best-match-begin (+ best-match-begin gap)
              best-match-end (+ best-match-end gap) 
              last-point (+ last-point gap)
              bound (+ bound gap))))
    (if (not (save-excursion
               (re-search-backward non-ws-re nil t)
               (php-atom-bounds)))
        (goto-char whole-match-end)
      (let* ((changes (clean-up-whitespace-around-chunk 
                       best-match-begin best-match-end t))
             (begin-gap (first changes))
             (gap (+ begin-gap (second changes))))
        (setf best-match-begin (+ begin-gap best-match-begin)
              best-match-end (+ begin-gap best-match-end)
              last-point (+ begin-gap last-point)
              bound (+ gap bound))
        (unless in-function-def
          (if (looking-back-p "^[[:space:]]*")
              (let ((gap (indent-according-to-mode)))
                (setf best-match-begin (+ best-match-begin gap)
                      best-match-end (+ best-match-end gap)
                      bound (+ bound gap) last-point (+ last-point gap)))
            (goto-char best-match-end)
            (let* ((operand-end 
                    (save-excursion
                      (catch 'done
                        (while t
                          (unless (looking-at-p non-ws-re)
                            (save-match-data
                              (re-search-forward non-ws-re bound t))
                            (backward-char))
                          (let ((current-atom (php-atom-bounds)))
                            (if current-atom
                                (let ((atom-end (second current-atom)))
                                  (if (<= atom-end bound)
                                      (goto-char (second current-atom))
                                    (throw 'done bound)))
                              (or (re-search-forward ws-re bound t)
                                  (throw 'done (1- bound)))))
                          (cond ((eq pass-name 'boolean+)
                                 (let ((op-end (1- (point))))
                                   (if (not (re-search-forward non-ws-re bound 
                                                               t))
                                       (throw 'done op-end)
                                     (backward-char)
                                     (when (or (looking-at-p break-regexp)
                                               (looking-at-p "[],)]"))
                                       (throw 'done op-end)))))
                                ((looking-back-p "new") (forward-char))
                                (t (if (not (looking-at-p "[],);]"))
                                       (throw 'done (1- (point)))
                                     (forward-char)
                                     (unless (member best-match 
                                                     '("extends" "implements"))
                                       (throw 'done (1- (point)))))))))))
                   (gap (php-clean-up-whitespace-around-operators
                         (point) operand-end t
                         (when php-format-align-array-double-arrows "=>"))))
              (setf operand-end (+ gap operand-end) bound (+ gap bound))
              (let* ((operand-end-stats (save-excursion
                                          (goto-char operand-end)
                                          `(,(current-column) 
                                            ,(line-number-at-pos))))
                     (operand-end-col (first operand-end-stats))
                     (operand-end-line (second operand-end-stats))
                     (num-lines (1+ (- operand-end-line (line-number-at-pos)))))
                (when (and (or (>= operand-end-col max-length)
                               (and (> num-lines 1)
                                    (save-excursion
                                      (catch 'needs-breaking
                                        (dotimes (i num-lines)
                                          (end-of-line)
                                          (when (>= (current-column) max-length)
                                            (throw 'needs-breaking t))
                                          (forward-line))))))
                           (not (and (eq pass-name 'boolean+)
                                     (string= best-match "="))))
                  (goto-char best-match-begin)
                  (let ((gap (newline-and-indent)))
                    (setf operand-end (+ gap operand-end) 
                          bound (+ gap bound))))) 
              (goto-char (1+ operand-end)))))))))

(defun php-format-break-statement-accessor-aux ()
  "Auxillary function used by ``php-format-break-statement''."
  (when (looking-back-p "->")
    (let* ((bounds (php-atom-bounds))
           (end (second bounds))
           (need-to-break (and (not (php-in-method-call))
                               (save-excursion
                                 (goto-char end)
                                 (> (current-column) max-length))))
           (gap 0))
      (when need-to-break
        (setf gap (php-format-break-at-arrows max-length (point) end))
        (setq bound (+ bound gap) end (+ end gap)))
      (goto-char end))))

(defun php-format-break-statement-concat-aux ()
  "Auxillary function used by ``php-format-break-statement''."
  (if (looking-at-p "=")
      (forward-char)
    (save-excursion
      (backward-char 2)
      (when (looking-at-p non-ws-re)
        (forward-char)
        (insert " ")))
    (let* ((begin (php-beginning-of-concat begin))
           (end (php-end-of-concat)))
      (setq bound (+ bound (php-format-break-at-concats max-length begin end)))
      (php-skip-this-concat))))

(defun php-format-break-statement-quote-aux ()
  "Auxillary function used by ``php-format-break-statement''."
  (if (not (zerop (php-get-current-sexp-level "[")))
      (php-skip-this-text-struct)
    (backward-char)
    (let ((begin (point))
          (begin-col (current-column))
          (begin-line (line-number-at-pos)))
      (forward-sexp)
      (unless (looking-at-p "[];),]")
        (backward-char))
      (unless (>= begin-col max-length)
        (when (or (>= (current-column) max-length)
                  (and strip-newlines-in-string
                       (> (line-number-at-pos) begin-line)))
          (let* ((end (point))
                 (gap (php-format-break-string strip-newlines-in-string 
                                               max-length begin end)))
            (setf bound (+ bound gap))
            (goto-char (+ end gap))))))))

(defun php-format-break-statement-opening-brace-aux ()
  "Auxillary function used by ``php-format-break-statement''."
  (save-excursion
    (backward-char)
    (if (and (php-class/interface-definitionp)
             (not (looking-back "^[[:space:]]*")))
        (let ((gap (+ (php-delete-horizontal-space)
                      (newline-and-indent))))
          (setf bound (+ bound gap) last-point (+ bound gap)))
      (when (re-search-backward ")" begin t)
        (let ((multiline-call 
               (not (= (line-number-at-pos)
                       (save-excursion
                         (forward-char)
                         (backward-sexp)
                         (line-number-at-pos))))))
          (when multiline-call
            (unless (looking-back (concat "^" ws-re "*"))
              (setf bound (1+ bound))
              (setf bound (+ bound (indent-according-to-mode)))))
          (forward-char)
          (let ((gap (php-delete-horizontal-space)))
            (insert " ")
            (setf bound (+ bound gap 1) last-point (+ last-point gap 1)))
          (if (or (and (php-anonymous-function-definitionp)
                       (or php-format-break-anonymous-function-always
                           (save-excursion
                             (forward-sexp)
                             (> (current-column) max-length))))
                  (and (php-named-function-definitionp) (not multiline-call)))
              (progn
                (unless (php-named-function-definitionp)
                  (forward-char))
                (let ((gap (1+ (newline-and-indent))))
                  (setf bound (+ bound gap) last-point (+ last-point gap)))
                (when (and (looking-at-p "{")
                           (not (looking-at-p (concat "{" ws-re "*$"))))
                  (forward-char)
                  (setf bound (+ 1 bound (newline-and-indent))))
                (backward-char))))))
    (when (looking-back-p non-ws-re)
      (insert " ")
      (setf bound (1+ bound) last-point (1+ last-point)))
    (when (looking-at-p (concat ws-re "*$"))
      (php-delete-horizontal-space))))

(defun php-format-break-statement-opening-parenthesis-aux ()
  "Auxillary function used by ``php-format-break-statement''."
  (when (<= (current-column) max-length)
    (setf bound (+ bound (php-delete-horizontal-space nil t)))
    (backward-char)
    (save-match-data
      (re-search-backward non-ws-re nil t)
      (forward-char)
      (when (looking-back-p (php-type-regexp 'identifier))
        (let ((gap (php-delete-horizontal-space)))
          (setf bound (+ bound gap) last-point (+ last-point gap)))))
    (when (or (looking-back-p "return")
              (looking-back-p "function")
              (and in-control 
                   (looking-at-p "(")
                   (= sexp-level 1)
                   (looking-back-p "[^([:space:]\n]")))
      (insert " ")
      (setf bound (1+ bound) last-point (1+ last-point)))
    (let* ((skipped 0)
           (sexp-ends (save-excursion
                        (forward-sexp)
                        (while (looking-at-p "[])]")
                          (forward-char)
                          (incf skipped))
                        (dotimes (i (min skipped (or broke-once 0)))
                          (backward-char))
                        (when (looking-at-p ";")
                          (forward-char))
                        (save-match-data
                          (when (looking-at (concat "[[:space:]]*{"))
                            (goto-char (match-end 0))))
                        `(,(1- (point)) ,(1- (current-column)) 
                          ,(line-number-at-pos))))
           (sexp-end (first sexp-ends))
           (sexp-end-col (second sexp-ends))
           (sexp-end-line (third sexp-ends))
           (in-control (php-in-control-statementp nil t)))
      (if (and (= sexp-end-col (+ 2 (current-column)))
               (= sexp-end-line (line-number-at-pos)))
          (forward-char 2)
        (if (or (>= sexp-end-col max-length)
                (and (> sexp-end-line (line-number-at-pos))
                     (not (looking-at-p (concat ws-re "*$"))))
                (and (wholenump broke-once)
                     (= (1+ broke-once) (php-get-current-sexp-level))
                     (not (looking-at-p "\\["))
                     php-format-break-all-method-call-arguments)
                (and php-format-break-anonymous-function-always
                     (save-excursion 
                       (re-search-forward 
                        (concat "function" ws-re "*(") sexp-end t))))
            (if (or (or php-format-break-function-call-like-definition 
                        (not (php-function-definitionp)))
                    (and (> (php-get-current-sexp-level) 0)
                         (looking-back-p "array")))
                (let* ((function-call 
                        (unless (or in-control
                                    (and (looking-at-p "\\[")
                                         (php-in-control-statementp)))
                          (looking-back-p (php-type-regexp 'identifier)))))
                  (forward-char)
                  (if in-control 
                      (throw 'skip t)
                    (when function-call
                      (setq broke-once (php-get-current-sexp-level))
                      (setf bound (+ 1 bound (newline-and-indent))))))
              (setf bound 
                    (+ bound 
                       (php-format-break-at-commas max-length (1+ (point))
                                                   sexp-end)))
              (forward-char))
          (forward-sexp))))))

(defun php-format-break-statement-commas-aux ()
  "Auxillary function used by ``php-format-break-statement''."
  (save-excursion
    (backward-char)
    (let ((gap (php-delete-horizontal-space)))
      (setf bound (+ gap bound) last-point (+ gap last-point))))
  (when (looking-at-p ws-re)
    (setf bound (+ bound (php-delete-horizontal-space))))
  (unless (looking-at-p "$")
    (let* ((current-line (line-number-at-pos))
           (atom-end-col (save-excursion (php-skip-this-atom) (current-column)))
           (sexp-begin (php-find-current-sexp-begin))
           (sexp-begin-line (save-excursion
                              (cond ((integerp sexp-begin)
                                     (goto-char sexp-begin)
                                     (line-number-at-pos))
                                    (t (beginning-of-line-non-whitespace)
                                       (line-number-at-pos)))))
           (multilinep (or (>= (current-column) max-length)
                           (>= atom-end-col max-length)
                           (not (= current-line sexp-begin-line)))))
      (if (and multilinep (or php-format-break-function-call-like-definition
                              (not in-function-def)))
          (setf bound (+ 1 bound (newline-and-indent)))
        (insert " ")
        (setf bound (1+ bound))))))

(defun php-format-break-statement-semicolon-aux ()
  "Auxillary function used by ``php-format-break-statement''."
  (unless (>= (point) bound)
    (let ((sexp-begin (php-in-control-statementp "for" t)))
      (if sexp-begin
          (php-format-break-statement-for-semicolons-aux)
        (unless (or (looking-at-p (concat ws-re "*$"))
                    (= (line-number-at-pos)
                       (save-excursion
                         (when (php-find-current-sexp-begin "{" nil t)
                           (line-number-at-pos)))))
          (set 'bound (+ 1 bound (newline-and-indent))))))))

(defun php-format-break-statement-for-semicolons-aux ()
  "Auxillary function used by ``php-format-break-statement''."
  (when (looking-at-p ws-re)
    (setf bound (+ bound (php-delete-horizontal-space nil t))))
  (let* ((current-line (line-number-at-pos))
         (sexp-begin-line (when (integerp sexp-begin)
                            (save-excursion
                              (goto-char sexp-begin)
                              (line-number-at-pos))))
         (sexp-end (when (integerp sexp-begin)
                     (save-excursion
                       (goto-char sexp-begin)
                       (save-match-data
                         (re-search-forward "(" nil t)
                         (backward-char)
                         (forward-sexp)
                         (point)))))
         (sexp-end-col (when (integerp sexp-end)
                         (save-excursion
                           (goto-char sexp-end)
                           (current-column))))
         (multilinep (not (= current-line 
                             sexp-begin-line))))
    (if (or (and php-format-break-all-for-semicolons 
                 (> current-line sexp-begin-line))
            (>= sexp-end-col (1- max-length)))
        (setf bound (+ 1 bound (newline-and-indent)))
      (insert " ")
      (setf bound (1+ bound)))))

(defun php-format-break-statement-closing-parenthesis-aux ()
  "Auxillary function used by ``php-format-break-statement''."
  (let* ((sexp-begin-stats (save-excursion
                             (backward-sexp)
                             (forward-char)
                             `(,(line-number-at-pos)
                               ,(or (php-in-control-statementp nil t)
                                    in-function-def
                                    (looking-at-p (concat ws-re "*$")))
                               ,(looking-back-p "array("))))
         (sexp-begin-line (first sexp-begin-stats))
         (multilinep (< sexp-begin-line 
                        (save-excursion
                          (save-match-data
                            (when (and in-control
                                       (looking-at (concat ws-re "*{")))
                              (goto-char (1- (match-end 0)))
                              (when (>= (current-column) max-length)
                                (goto-char (1- (match-beginning 0)))
                                (let ((gap (1+ (newline-and-indent))))
                                  (setf bound (+ bound gap)
                                        last-point (+ last-point gap)))))
                            (line-number-at-pos)))))
         (arrayp (third sexp-begin-stats)))
    (backward-char)
    (unless (looking-back-p "^[[:space:]]*")
      (when (looking-back-p ws-re)
        (save-match-data
          (re-search-backward non-ws-re nil t)
          (forward-char)
          (let ((gap (php-delete-horizontal-space)))
            (setf bound (+ bound gap) last-point (+ last-point gap)))))
      (unless (looking-back-p (concat "^" ws-re "*" (when in-control ")*")))
        (if (and (second sexp-begin-stats) multilinep)
            (let ((gap (1+ (newline-and-indent))))
              (setf bound (+ bound gap) last-point (+ last-point gap)))
          (forward-char)
          (save-excursion
            (backward-sexp)
            (forward-char)
            (let ((gap (php-delete-horizontal-space)))
              (setf bound (+ bound gap) last-point (+ last-point gap)))
            (when (and (looking-back-p "{")
                       (not (looking-at-p "[[:space:]]*$")))
              (insert " ")
              (setf bound (1+ bound) last-point (1+ last-point))))
          (backward-char)
          (when (and (looking-at-p "}")
                     (not (looking-back-p "^[[:space:]]*")))
            (when (looking-back-p ws-re)
              (backward-char)
              (let ((gap (php-delete-horizontal-space nil t)))
                (setf bound (+ bound gap) last-point (+ last-point gap))))
            (let ((gap (if in-function-def 
                           (1+ (newline-and-indent))
                         (insert " ")
                         1)))
              (setf bound (+ gap bound) last-point (+ gap last-point)))))))
    (when (and arrayp php-format-align-array-double-arrows)
      (setf bound (+ bound (align-on "=>"))))
    (forward-char)))

(defun php-format-update-break-points (gap)
  "Auxiliary function used by ``php-format-simple-break''."
  (dotimes (j (- (length break-points) (1+ i)))
    (let* ((k (+ i j 1))
           (bp (elt break-points k)))
      (setf (elt break-points k) `(,(+ (first bp) gap) ,(second bp))))))

(defun php-format-strip-newlines (break-points &optional in-string)
  "Auxiliary function used by `php-format-simple-break' and
others.  Strips newlines and extraneous spaces from (point)
to (last (BREAK-POINTS)).  BREAK-POINTS are assumed to be in
order and will be updated to reflect the removed newlines.
Optionally, remove newlines that are IN-STRING."
  (save-excursion
    (let* ((accessor-re (php-type-regexp 'accessor))
           (end (first (first (last break-points))))
           insert-space)
      (while (and (re-search-forward "\n" end t)
                  (or in-string (not (php-in-text-structp))))
        (backward-char)
        (save-match-data
          (when (looking-at (concat ws-re "+"))
            (let* ((b (match-beginning 0))
                   (e (match-end 0))
                   (to-delete (- e b)))
              (delete-region b e)
              (setf insert-space
                    (or (and in-string (php-in-text-structp))
                        (not (or (looking-at-p accessor-re)
                                 (looking-at-p "[])}]")
                                 (looking-back-p "[({[]")))))
              (when insert-space 
                (insert " ")
                (setf to-delete (1- to-delete)))
              (setf end (- end to-delete)
                    break-points (mapcar (lambda (x) 
                                           (let ((y (first x)) (l (second x)))
                                             (if (< y (point)) 
                                                 `(,y ,l) 
                                               `(,(- y to-delete) ,l))))
                                         break-points))))))))
  break-points)

(defun php-format-simple-break (regexp &optional begin end max-length 
                                       break-every break-after no-strip 
                                       match-length cleanup-whitespace 
                                       skip-sexps match-start)
  "Performs simple line breaking at occurrences of REGEXP when
the statement is over MAX-LENGTH.  If BREAK-EVERY is set, break
at first necessity and then everytime after.  BEGIN may be a
regexp or a position.  If it is a regexp it will be passed to
``re-search-backward''.  END will be passed to
``php-locate-non-enclosed''.  Breaks the string before REGEXP
unless BREAK-AFTER.  Will strip embedded newlines unless NO-STRIP
is t.  Cleans up spacing after each break point as well.  Use
MATCH-START to control where in the match to make the break and
MATCH-LENGTH to control how many characters of REGEXP match to
consider as the break point.  It can be told to
CLEANUP-WHITESPACE around the match.  It can be told to
SKIP-SEXPS that it encounters.  Returns the amount of characters
added."
  (save-excursion
    (let* ((begin (goto-char (if (integerp begin)
                                 begin
                               (if (stringp begin)
                                   (or (save-excursion 
                                         (re-search-backward begin nil t))
                                       (point-min))
                                 (or (php-in-statementp) 
                                     (line-beginning-position))))))
           (end (or end (save-excursion (php-skip-this-statement) (point))))
           (max-length (or max-length 80))
           (regexp (if no-strip (concat regexp "\\|\n") regexp))
           (break-points (php-locate-non-enclosed regexp end skip-sexps))
           (match-start (if (integerp match-start) match-start 0))
           (break-points (mapcar (lambda (x) `(,(+ (first x) match-start)
                                               ,(- (second x) match-start))) 
                                 break-points))
           (break-points (add-to-list 'break-points
                                      (if (integerp end)
                                          `(,end 1)
                                        (first (php-locate-non-enclosed end 
                                                                        end))) 
                                      t))
           (original-end (first (first (last break-points)))))
      (unless no-strip
        (setq break-points (php-format-strip-newlines break-points)))
      (dotimes (i (1- (length break-points)) 
                  (- (first (first (last break-points))) original-end))
        (let* ((b1-proto (elt break-points i))
               (b1 (first b1-proto))
               (b1-length (if (integerp match-length) 
                              match-length
                            (second b1-proto)))
               (b1-line (save-excursion (goto-char b1) (line-number-at-pos)))
               (b2 (first (elt break-points (1+ i))))
               (already-broken (save-excursion
                                 (goto-char b1)
                                 (looking-back-p "^[[:space:]]*"))))
          (goto-char b2)
          (when (or (and (not already-broken) 
                         (or break-every 
                             (and (not (looking-back-p "\n"))
                                  (> (line-number-at-pos) b1-line))))
                    (>= (current-column) max-length))
            (goto-char b1)
            (when cleanup-whitespace
              (clean-up-whitespace-around-chunk (point) (+ b1-length (point)))
              (forward-char))
            (if break-after 
                (forward-char)
              (re-search-backward non-ws-re nil t)
              (forward-char))
            (php-format-update-break-points (php-delete-horizontal-space nil t))
            (php-format-update-break-points (1+ (newline-and-indent)))))))))

(defun php-format-break-at-commas (&optional max-length begin end break-every)
  "This function breaks a PHP statement at the comma operators so
that the lines are less than MAX-LENGTH.  It respects
``php-format-break-all-method-call-arguments''.  BEGIN and END
will be passed to ``php-format-simple-break''.  It works within
the current sexp level."
  (interactive)
  (php-format-simple-break "," begin end max-length 
                           (or php-format-break-all-method-call-arguments
                               break-every) t t nil nil t))

(defun php-format-break-at-concats (&optional max-length begin end)
  "This function breaks a PHP statement at the concatenate
operators so that the lines are less than MAX-LENGTH.  Respects
``php-format-break-start-line-with-concat''.  BEGIN and END will
be passed to ``php-format-simple-break''.  It works within the
current sexp level."
  (interactive)
  (php-format-simple-break "\\.[^=]" begin end max-length nil 
                           (not php-format-break-start-line-with-concat) t 1 
                           nil t))

(defun php-format-break-at-operators (&optional max-length begin end)
  "This function breaks a PHP statement at the
operators (boolean, logical and arithmetic) so that the lines are
less than MAX-LENGTH.  Respects
``php-format-break-all-operators''.  BEGIN and END will be passed
to ``php-format-simple-break''.  It works within the current sexp
level."
  (interactive)
  (php-format-simple-break (php-type-regexp 'operator) begin end max-length 
                           php-format-break-all-operators nil t nil nil t))

(defun php-format-break-at-assignment-operators (&optional max-length begin end)
  "This function breaks a PHP statement at the assigment
operators that the lines are less than MAX-LENGTH.  Respects
``php-format-break-all-assignment-operators''.  BEGIN and END
will be passed to ``php-format-simple-break''.  It works within
the current sexp level."
  (interactive)
  (php-format-simple-break (php-type-regexp 'assignment-operator) begin end 
                           max-length php-format-break-all-assignment-operators 
                           nil t nil nil t))

(defun php-format-break-at-arrows (&optional max-length begin end)
  "This function breaks a PHP statement at the object accessing
arrows so that the lines are less than MAX-LENGTH.  It respects
the defcustom ``php-format-break-all-method-chain-links''.  BEGIN
and END will be passed to ``php-format-simple-break''.  It works
within the current sexp level."
  (interactive)
  (php-format-simple-break ")->" begin end max-length 
                           php-format-break-all-method-chain-links nil t nil 
                           nil t 1))

(defun php-auto-fill (arg)
  "This function is used in place of the regular bound command
for the return key. If the given line is under 80 characters,
this command will just run the normal
self-insert-command. Otherwise, it will decide how to truncate
the line appropriately and automatically.  When ARG, don't insert
a line, just go to the next one."
  (interactive "P")
  (let ((at-eof (eq (point) (point-max))))
    (cond ((php-in-text-structp)
           (save-match-data
             (newline-and-indent)
             (when (save-excursion
                     (forward-line -1)
                     (looking-at "\\*+[[:space:]]*"))
               (insert (match-string-no-properties 0)))))
          (t (if (not (and php-auto-fill 
                           (php-in-scriptp) 
                           (equal (point) (point-at-eol))
                           (re-search-backward non-ws-re (point-at-bol) t)))
                 (newline-and-indent)
               (when (or (looking-at-p ";")
                         (and (looking-at-p "{")
                              (not (looking-back-p (concat "function" ws-re "*"
                                                           "(.*)" ws-re "*")))))
                 (let ((start-pos (point)))
                   (goto-char (+ start-pos (php-format-break-statement)))))
               (forward-char)
               (php-delete-horizontal-space)
               (when (looking-at-p "}")
                 (save-excursion
                   (newline-and-indent)))
               (when at-eof
                 (save-excursion
                   (newline)))
               (let ((cur-line (line-number-at-pos)))
                 (beginning-of-line 2)
                 (when (re-search-forward non-ws-re (line-end-position) t)
                   (backward-char)
                   (when (or (<= (line-number-at-pos) cur-line)
                             (and (not arg) (looking-at-p non-ws-re)))
                     (let ((cur-col (current-column)))
                       (newline)
                       (indent-to cur-col))
                     (when (looking-at-p non-ws-re)
                       (forward-line -1))))
                 (indent-according-to-mode)))))))

(defun php-clean-up-whitespace-around-operators (begin end &optional 
                                                       ignore-newlines
                                                       ignore-ops)
  "Cleans up the whitespace around operators between BEGIN and
END.  Can be told to IGNORE-NEWLINES and/or a regexp of
IGNORE-OPS.  Returns the change in END."
  (save-excursion
    (goto-char begin)
    (let ((change 0)
          (regexp (concat (php-type-regexp 'assignment-operator) "\\|"
                          (php-type-regexp 'operator))))
      (save-match-data
        (while (and (<= (point) end) (re-search-forward regexp end t))
          (let* ((whole-match (match-string-no-properties 0))
                 (i (if (match-string-no-properties 1) 1 0))
                 (best-match (match-string-no-properties i))
                 (best-match-begin (match-beginning i))
                 (best-match-end (match-end i))
                 (best-match-length (- best-match-end
                                       best-match-begin)))
            (unless (or (and (stringp ignore-ops)
                             (string-match-p ignore-ops best-match))
                        (php-in-text-structp nil best-match-begin 
                                             best-match-end)
                        (not (save-excursion
                               (goto-char best-match-begin)
                               (re-search-backward non-ws-re nil t)
                               (php-atom-bounds))))
              (let ((changes 
                     (clean-up-whitespace-around-chunk best-match-begin 
                                                       best-match-end
                                                       ignore-newlines)))
                (setf change (+ change (indent-according-to-mode) 
                                (+ (first changes) (second changes)))))))))
      change)))

(defun php-format-break-this-type (&optional type sit-for force-quotes)
  "Runs ``php-format-break-statement'' on all statements in the
current TYPE.  TYPE may either be a cons of (begin . end) or
follow the semantics of ``php-parse-current'' and defaults to
'(constant property method class interface script file).
Optionally, you may tell it how long to SIT-FOR at each
statement.  If SIT-FOR is nil, will not sit at all.  Can also
FORCE-QUOTES."
  (save-excursion
    (let* ((type (or type 
                     '(constant property method class interface script file)))
           (statement-re non-ws-re)
           (non-statement-re ws-re)
           (parse (if (and (consp type)
                           (integerp (first type))
                           (integerp (rest type)))
                      `((type . region)
                        (begin . ,(first type)) 
                        (end . ,(rest type))
                        (text . ,(buffer-substring-no-properties (first type)
                                                                 (rest type))))
                    (php-parse-current type))))
      (when (php-parse-p parse)
        (let ((begin (rest (assoc 'begin parse)))
              (end (rest (assoc 'end parse)))
              (last-point -1))
          (goto-char begin)
          (catch 'done
            (while (< (point) (min end (point-max)))
              (when (<= (point) last-point)
                (error (concat "Infinite loop detected in "
                               "php-format-break-this-type at %s <= %s.  "
                               "Unable to advance through statements.") (point) 
                               last-point))
              (setf last-point (point))
              (when (and (looking-at-p non-statement-re)
                         (re-search-forward statement-re end t))
                (backward-char))
              (save-match-data
                (cond ((looking-at (concat ws-re "*}"))
                       (goto-char (match-end 0)))
                      ((looking-at-p (concat "\\?>\\|" ws-re "*\\'"))
                       (throw 'done t))
                      ((php-in-statementp)
                       (when force-quotes
                         (condition-case nil
                             (setf end (+ end 
                                          (php-force-string-quotes-statement)))
                           (error 
                            (message (concat "Error running "
                                             "php-force-string-quotes-statement "
                                             "in php-format-break-this-type at "
                                             "%s.")
                                     (point)))))
                       (when (integerp sit-for)
                         (sit-for sit-for))
                       (condition-case nil
                           (setf end (+ end (php-format-break-statement)))
                         (error 
                          (message (concat "Error running "
                                           "php-format-break-statement in "
                                           "php-format-break-this-type at %s.")
                                   (point))))
                       (php-skip-this-statement))
                      (t (condition-case nil
                             (setf end 
                                   (+ end (indent-according-to-mode)))
                           (error 
                            (message (concat "Error running "
                                             "indent-according-to-mode in "
                                             "php-format-break-this-type at "
                                             "%s.")
                                     (point))))
                         (let ((bounds (php-atom-bounds)))
                           (if bounds
                               (goto-char (second bounds))
                             (unless (zerop (forward-line))
                               (goto-char (point-max)))))))))))))))

(defun php-format-break-current (&optional sit-for start end)
  "Runs ``php-format-break-statement'' on all statements in the
current constant, property or method.  Optionally, you may tell
it how long to SIT-FOR at each statement.  If SIT-FOR is nil,
will not sit at all.  You may also specify START and/or END or
use point and mark. "
  (interactive (append `(,current-prefix-arg)
                       (when (region-active-p)
                         `(,(region-beginning) ,(region-end)))))
  (let ((type (if (and (integerp start) (integerp end))
                  `(,start . ,end)
                '(constant property method))))
    (php-format-break-this-type type sit-for php-force-single-quoted-strings)))

(defun php-format-break-class/interface (&optional sit-for)
  "Runs ``php-format-break-statement'' on all statements in the
current class or interface.  Optionally, you may tell it how long
to SIT-FOR at each statement.  If SIT-FOR is nil, will not sit at
all."
  (interactive "P")
  (php-format-break-this-type '(class interface) sit-for 
                              php-force-single-quoted-strings))

(defun php-format-clean-up-script (&optional sit-for)
  "Runs ``php-format-break-class/interface'' and then
``php-rearrange-innards'' on the current class/interface.
Optionally, you may tell it how long to SIT-FOR at each
statement.  If SIT-FOR is nil, it will sit-for 0.  If sit-for is
negative, it will not sit at all."
  (interactive "P")
  (let ((sit-for (cond ((or (and (stringp sit-for)
                                 (string= "-" sit-for))
                            (and (integerp sit-for) (< sit-for 0)) nil))
                       ((integerp sit-for) sit-for)
                       (t 0))))
    (php-format-clean-up-typos)
    (save-excursion
      (if (not (php-jump-to-first-class/interface))
          (php-format-break-this-type '(script) sit-for 
                                      php-force-single-quoted-strings)
        (php-format-break-class/interface sit-for)
        (php-rearrange-innards sit-for)))))

(defun php-format-clean-up-typos ()
  "This function currently replaces tabs with spaces and
short-tags with their equivalents."
  (interactive)
  (let ((control-re (concat "\\(" php-block-stmt-1-key "\\|" 
                            php-block-stmt-2-key "\\)"))
        (statement-re (concat "\\(\\(?:include\\|require\\)\\(?:_once\\)?\\)"
                              "\\s-*(\\([^)]*?\\))\\s-*;")))
    (dolist (pair `(("\t" ,(make-string php-basic-offset ?\ ))
                    ("<\\?=" "<?php echo")
                    ("<\\?[^p]\\s-*" "<?php ")
                    ("\\([^\n[:space:];{]\\)\\s-*\\?>" "\\1; ?>")
                    ("else if\\s-*(" "elseif (")
                    (,(concat control-re "\\s-*(") "\\1 (")
                    (,(concat control-re "\\s-*(\\|else") 
                     ,(lambda ()
                        (catch 'not-finished
                          (save-excursion
                            (backward-char)
                            (condition-case nil
                                (forward-sexp)
                              (error (throw 'not-finished t)))
                            (unless (looking-at-p (concat ws-re "*{"))
                              (insert " {")
                              (end-of-line 2)
                              (newline)
                              (insert "}")
                              (indent-according-to-mode))))))
                    ("^\\s-*{" ,(lambda ()
                                  (catch 'not-began
                                    (when (save-excursion
                                            (end-of-line 0)
                                            (when (looking-back-p ")\\s-*")
                                              (goto-char 
                                               (1+ (match-beginning 0)))
                                              (condition-case nil
                                                  (backward-sexp)
                                                (error (throw 'not-began nil))))
                                            (looking-back-p 
                                             (concat control-re "\\s-*")))
                                    (delete-indentation)))))
                    ("}\\s-*$" ,(lambda ()
                                  (when (save-excursion
                                          (when (re-search-forward 
                                                 non-ws-re nil t)
                                            (backward-char)
                                            (looking-at-p "else")))
                                    (delete-indentation t))))
                    ("{[^\n]" ,(lambda ()
                                 (goto-char (1+ (match-beginning 0)))
                                 (delete-horizontal-space)
                                 (unless (looking-at-p "$")
                                   (newline-and-indent))))
                    (,statement-re "\\1 \\2;")))
      (save-excursion
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward (first pair) nil t)
            (let ((replacement (second pair)))
              (if (stringp replacement)
                  (replace-match replacement)
                (funcall replacement))))))
      (delete-trailing-whitespace))))
 
(defun php-force-string-quotes-statement (&optional statement-begin 
                                                   statement-end)
  "Trial function to turn all double-quoted strings in the
current PHP statement to single-quoted.  You may also specify
STATEMENT-BEGIN and/or STATEMENT-END or use point and mark."
  (interactive (when (region-active-p) `(,(region-beginning) ,(region-end))))
  (save-excursion
    (let ((statement-begin (if (integerp statement-begin)
                               statement-begin
                             (php-in-statementp)))
          (change 0))
      (when (integerp statement-begin)
        (goto-char statement-begin)
        (let ((statement-end (if (integerp statement-end)
                                 statement-end
                               (save-excursion (php-skip-this-statement)))))
          (when (php-next-text-struct statement-end)
            (catch 'done
              (while (< (point) statement-end)
                (when (looking-at-p "\"")
                  (sit-for 0)
                  (setf change (php-change-string-quotes)))
                (php-skip-this-text-struct)
                (unless (php-next-text-struct statement-end)
                  (throw 'done t)))))))
      change)))

(defun php-force-string-quotes ()
  "Trial function to turn all double-quoted strings in PHP
scripts to single-quoted."
  (interactive)
  (save-excursion
    (let ((scripts (rest (assoc 'scripts (php-parse-current 'file)))))
      (dolist (script scripts)
        (goto-char (rest (assoc 'begin script)))
        (while (< (point) (rest (assoc 'end script)))
          (php-next-text-struct)
          (when (looking-at-p "\"")
            (sit-for 0)
            (php-change-string-quotes))
          (php-skip-this-text-struct))))))

(provide 'php-format)
