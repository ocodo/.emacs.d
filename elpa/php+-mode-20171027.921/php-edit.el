;;; php-edit.el --- Functions that deal with PHP text editing

;; Version: 1.0
;; Created: 11-01-2011
;; Copyright © 2011 Michael Dwyer
;; Author(s): 
;; Michael Dwyer <mdwyer@ehtech.in>

;;; *****
;;; About
;;; *****

;; php-edit.el is a part of the php+-mode suite and contains convenience
;; functions for PHP text editing, such as killing the current
;; structure and formatting the whitespace around it.

;; ********************************************************************

;; ************
;; REQUIREMENTS
;; ************
(require 'php-parse)
(require 'php-format)
(require 'php-funcs)
(require 'php-structure)

;; *********
;; FUNCTIONS
;; *********
(defun php-mark-current (&optional type)
  "Put point at beginning and mark at end of current structure.
Optionally force TYPE of structure.  You probably should not use
this function in Lisp programs; it is usually a mistake for a
Lisp function to use any subroutine that uses or sets the mark."
  (interactive)
  (let* ((type (or type '(constant property method)))
         (parse (php-parse-current type)))
    (when (php-parse-p parse)
      (push-mark (point))
      (push-mark (rest (assoc 'begin parse)) nil t)
      (goto-char (rest (assoc 'end parse)))
      (exchange-point-and-mark))))

(defun php-comment-current (&optional type)
  "Comments or uncomments the current or given thing."
  (interactive)
  (save-excursion
    (php-mark-current (when type type))
    (comment-or-uncomment-region (region-beginning) (region-end))))

(defun php-kill-sexp-innard ()
  "Kills everything within the current sexp"
  (interactive)
  (kill-region (1+ (php-find-current-sexp-begin))
               (1- (php-find-current-sexp-end))))

(defun php-kill-current (&optional type no-fix-spacing)
  "Kills the current TYPE PHP structure at point.  TYPE follows
the semantics of php-parse-current.  Returns the parsed
structure.  If NO-FIX-SPACING is non-nil, don't run
``php-format-spacing'' afterwards.  Although you may yank what is
deleted, only undo is guaranteed to return the previous structure
to its previous placement.  You may want to use ``php-yank''
instead."
  (interactive)
  (save-match-data
    (let ((parse (php-parse-current '(constant property method))))
      (when (php-parse-p parse)
        (let ((begin (rest (assoc 'begin parse)))
              (end (rest (assoc 'end parse))))
          (goto-char begin)
          (when (looking-at-p ws-re)
            (re-search-forward non-ws-re nil t)
            (backward-char))
          (let ((begin (point)))
            (goto-char end)
            (when (looking-at-p ws-re)
              (re-search-backward non-ws-re nil t)
              (forward-char)
              (setq end (point)))
            (re-search-forward non-ws-re nil t)
            (backward-char)
            (if (not (looking-at-p "}"))
                (setq end (point))
              (goto-char begin)
              (re-search-backward non-ws-re nil t)
              (forward-char)
              (setq begin (point)))
            (setf (rest (assoc 'begin parse)) begin)
            (setf (rest (assoc 'end parse)) end)
            (setf (rest (assoc 'text parse))
                  (buffer-substring-no-properties begin end))
            (kill-region begin end)
            (unless no-fix-spacing
              (php-format-spacing t))
            (goto-char begin)
            parse))))))

(defun php-yank (&optional pos no-fix-spacing trim-prefixed-whitespace)
  "Yanks a PHP structure at position POS (or (point)) and then
corrects the whitespace around it.  If NO-FIX-SPACING is non-nil,
don't run ``php-format-spacing'' afterwards.  It may be told to
TRIM-PREFIXED-WHITESPACE even if NO-FIX-SPACING is non-nil.  Puts
point at end and set mark at beginning."
  (interactive)
  (let ((pos (if (integerp pos) pos (point))))
    (goto-char pos)
    (let ((correct-previous-spacing (looking-at-p "}")))
      (yank)
      (let ((end (point)))
        (when trim-prefixed-whitespace
          (goto-char pos)
          (re-search-forward non-ws-re nil t)
          (backward-char)
          (let ((gap (- (point) pos)))
            (delete-region pos (point))
            (setq end (- end gap))))
        (goto-char end)
        (unless no-fix-spacing
          (php-format-region pos (point) correct-previous-spacing)
          (php-format-spacing t))))))

(defun php-format-region (begin end &optional correct-previous-spacing)
  "Corrects the whitespace around a region from BEGIN to END.  If
CORRECT-PREVIOUS-SPACING is non-nil, correct spacing before BEGIN
as well."
  (interactive (when (region-active-p) `(,(region-beginning) ,(region-end))))
  (save-excursion
    (goto-char begin)
    (when correct-previous-spacing
      (re-search-backward non-ws-re nil t)
      (forward-char)
      (setq begin (point)))
    (save-match-data
      (when (save-excursion (re-search-forward non-ws-re end t))
        (let ((e (1- (match-end 0))))
          (setf end (- end (- e (point))))
          (delete-region (point) e))))
    (setq end (+ end (indent-according-to-mode)))
    (when correct-previous-spacing
      (let ((temp (point)))
        (newline)
        (newline-and-indent)
        (setq end (+ end (- (point) temp)))))
    (goto-char end)
    (re-search-backward non-ws-re nil t)
    (forward-char)
    (let ((end (point)))
      (re-search-forward non-ws-re nil t)
      (backward-char)
      (when (not (looking-at-p "}"))
        (delete-region end (point))
        (newline)
        (newline-and-indent)
        (setq end (point))))))

; defined for compiler
(defvar php+-mode-delete-trailing-whitespace)

(defun php-format-spacing (&optional no-indent-all-lines)
  "Formats the spacing in the current class/interface according
to defcustoms ``php-blank-line-at-start-of-class'' and
``php-blank-line-at-end-of-class''.  Will run
``indent-according-to-mode'' on all lines in the structure unless
passed NO-INDENT-ALL-LINES."
  (interactive)
  (save-excursion
    (save-match-data
      (let ((parse (php-parse-current '(class interface))))
        (when (php-parse-p parse)
          (let ((begin (rest (assoc 'begin parse)))
                (end (rest (assoc 'end parse))))
            (goto-char begin)
            (php-jump-to-first-statement)
            (re-search-backward "{" nil t)
            (forward-char)
            (let ((begin (point)))
              (re-search-forward non-ws-re nil t)
              (backward-char)
              (let ((gap (- (point) begin)))
                (delete-region begin (point))
                (setf end (- end gap)))
              (when php-blank-line-at-start-of-class
                (newline)
                (setf end (1+ end)))
              (setf end (+ end (newline-and-indent))))
            (goto-char end)
            (when (not (looking-at-p "}"))
              (re-search-backward "}" nil t))
            (let ((end (point)))
              (re-search-backward non-ws-re nil t)
              (forward-char)
              (delete-region (point) end)
              (when php-blank-line-at-end-of-class
                (newline))
              (newline-and-indent)
              ;; Something in here causes undo ring corruption
              (unless no-indent-all-lines
                (let ((end (point)))
                  (goto-char begin)
                  (setf end (+ end (indent-according-to-mode)))
                  (while (and (< (point) end) (< (forward-line) 1))
                    (when (not (looking-at-p "^$"))
                      (setf end 
                            (+ end 
                               (indent-according-to-mode)))))))))))
      (when php+-mode-delete-trailing-whitespace
        (delete-trailing-whitespace)))))

(defun fixup-whitespace ()
  "Fixup white space between objects around point.
Leave one space or none, according to the context.  This version
overrides the one built into Emacs, and respects PHP accessor
operators."
  (interactive "*")
  (save-excursion
    (save-match-data
      (php-delete-horizontal-space)
      (if (or (looking-at "^\\|\\s)")
              (looking-at "->\\|::")
              (save-excursion (forward-char -1)
                              (looking-at "$\\|\\s(\\|\\s'"))
              (save-excursion (forward-char -2)
                              (looking-at "->\\|::")))
          nil
        (insert ?\s)))))

(defun php-kill-chain-link ()
  "Kills the chain link around point."
  (interactive)
  (let ((begin (save-excursion
                 (let ((ident-begin (php-in-identifierp)))
                   (if (re-search-backward ">" ident-begin t)
                       (1+ (point))
                     ident-begin))))
        (end (save-excursion
               (save-match-data
                 (catch 'done
                   (let ((ident-end (save-excursion
                                      (php-skip-this-identifier))))
                     (while (re-search-forward "[-(]" ident-end t)
                       (if (not (string= (match-string-no-properties 0) "("))
                           (throw 'done (1+ (point)))
                         (backward-char)
                         (forward-sexp)))
                     (if (not (looking-back-p "-"))
                         (goto-char ident-end)
                       (backward-char)
                       (1+ (point)))))))))
    (let ((begin (if (save-excursion
                       (goto-char end)
                       (looking-back-p ">"))
                     begin
                   (- begin 2))))
      (delete-region begin end)
      (- end begin))))

(provide 'php-edit)
