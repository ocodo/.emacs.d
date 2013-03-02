;;; string-utils.el --- String utility functions

;; Version: 1.0
;; Created: 2011-07-17
;; Copyright © 2011 Michael Dwyer
;; Author(s):
;; Michael Dwyer <mdwyer@ehtech.in>

;;; *****
;;; About
;;; *****

;; Various string manipulation functions that make life easier

;; ********************************************************************

;; *********
;; CONSTANTS
;; *********
(defconst ws-re "[[:space:]\n]"
  "Regexp that is used to denote a whitespace character.")

(defconst non-ws-re "[^[:space:]\n]"
  "Regexp that is used to denote a non-whitespace character.")

;; *********
;; FUNCTIONS
;; *********
(defun nil-or-blank (arg)
  "Returns whether or not arg is nil or an empty string."
  (or (equal nil arg)
      (equal "" arg)))

(defun multilinep (string)
  (string-match "\n" string))

(defun safe-split-string (string &optional separators omit-nulls)
  "For some reason split-string modifies match data, causing
subtle bugs.  Using this will fix that.  See split-string for
more information."
  (save-match-data (split-string string separators omit-nulls)))

(defun reverse-string (str)
  "Reverses the string STR non-destructively."
  (apply 'string (reverse (string-to-list str))))

(defun chomp (s)
  "Chomp leading and tailing whitespace from STR."
  (save-match-data
    (when (stringp s)
      (while (and (> (length s) 0) (string-match ws-re (substring s 0 1)))
        (setq s (substring s 1)))
      (while (and (> (length s) 0) (string-match ws-re (substring s -1)))
        (setq s (substring s 0 -1)))
      s)))

(defun php-delete-horizontal-space (&optional backward-only include-newlines)
  "This wrapper around ``delete-horizontal-space'' returns the
amount of text-size change.  Can be told to INCLUDE-NEWLINES as
well."
  (let ((change (php-delete-horizontal-space-aux backward-only)))
    (when include-newlines
      (save-match-data
        (when (looking-at "\n[[:space:]]*")
          (let ((b (match-beginning 0))
                (e (match-end 0)))
            (delete-region b e)
            (setf change (+ change (- b e)))))))
    change))

(defun php-delete-horizontal-space-aux (&optional backward-only)
  "Re-implementation of ``delete-horizontal-space'' which returns
the change in text size."
    (unless (or backward-only (looking-at-p non-ws-re))
      (save-match-data
        (when (re-search-forward non-ws-re (line-end-position) t)
          (backward-char))))
    (let ((pos (point)))
      (delete-horizontal-space backward-only)
      (- (point) pos)))
      
(defun clean-up-whitespace-around-point (&optional ignore-newlines)
  "If point is on a non-whitespace character, reduce the
whitespace on both sides of point to one space.  Can be told to
IGNORE-NEWLINES.  Return the amount of text added."
  (save-excursion
    (save-match-data
      (let ((begin-change 0)
            (end-change 0)
            (ws-re (if ignore-newlines "[[:space:]]" ws-re))
            (non-ws-re (if ignore-newlines "[^[:space:]]" non-ws-re)))
        (when (looking-at-p non-ws-re)
          (when (re-search-backward ws-re nil t)
            (if (re-search-backward non-ws-re 
                                    (when ignore-newlines 
                                      (line-beginning-position)) t)
                (forward-char)
              (goto-char (if ignore-newlines 
                             (line-beginning-position) 
                           (point-min))))
            (setf begin-change (+ begin-change 
                                  (php-delete-horizontal-space 
                                   nil (not ignore-newlines))))
            (unless (looking-back-p "^")
              (insert " ")
              (setf begin-change (1+ begin-change)))
            (re-search-forward ws-re nil t)
            (backward-char)
            (when (looking-at-p ws-re)
              (setf end-change (+ end-change 
                                  (php-delete-horizontal-space 
                                   nil (not ignore-newlines)))))
            (insert " ")
            (setf end-change (1+ end-change))))
        `(,begin-change ,end-change)))))

(defun clean-up-whitespace-around-chunk (begin end &optional ignore-newlines)
  "Makes sure that the chunk of text from BEGIN to END is
surrounded by one space on each side.  Can be told to
IGNORE-NEWLINES.  Return the changes in begin and end."
  (save-excursion
    (let ((begin-change 0)
          (end-change 0))
      (goto-char begin)
      (unless (or (= (char-before) ?\ )
                  (= (char-after) ?\ ))
        (insert " ")
        (setf begin-change (1+ begin-change) begin (1+ begin) end (1+ end)))
      (goto-char end)
      (unless (or (= (char-before) ?\ )
                  (= (char-after) ?\ ))
        (insert " ")
        (setf end-change (1+ end-change)))
      (goto-char begin)
      (let ((change-deltas (clean-up-whitespace-around-point ignore-newlines)))
        `(,(+ begin-change (first change-deltas)) 
          ,(+ end-change (second change-deltas)))))))

(defun camelcase->hyphenated (str)
  (let ((new-str ""))
    (dotimes (i (length str) new-str)
      (let ((letter (elt str i)))
        (when (and (> i 0) (= letter (upcase letter)))
          (setq new-str (concat new-str "-")))
        (setq new-str (concat new-str (char-to-string (downcase letter))))))))

(defun break-at-commas (str &optional indent max-len)
  "Break string at commas to keep it under MAX-LEN characters.
Default is 80.  Optionally indent new lines INDENT spaces."
  (when (stringp str)
    (let* ((max-len (if (integerp max-len) max-len 80))
           (indent-str (make-string (if (integerp indent) indent 0) ?\ ))
           (lines (safe-split-string str "\n"))
           (new-str ""))
      (dolist (line lines (substring new-str 1))
        (setq new-str 
              (concat new-str "\n"
                      (if (< (length line) 80)
                          line
                        (let* ((parts (safe-split-string line ","))
                               (new-line (first parts))
                               (len (length new-line)))
                          (dolist (part (rest parts) new-line)
                            (let* ((part-len (length part))
                                   (new-len (+ len 2 part-len)))
                              (if (> new-len max-len)
                                  (progn
                                    (setq len 0)
                                    (setq new-line 
                                          (concat new-line 
                                                  ",\n" indent-str)))
                                (setq len new-len)
                                (setq new-line 
                                      (concat new-line ", ")))
                              (setq new-line 
                                    (concat new-line 
                                            (replace-regexp-in-string 
                                             "^[[:space:]]*" "" 
                                             part)))))))))))))

(defsubst looking-back-p (regexp)
  "Same as `looking-back' except this function does not change the
match data."
  (let ((inhibit-changing-match-data t))
    (looking-back regexp)))

(defun re-search-backward-greedy (regexp &optional bound noerror count)
  "Performs a greedy backward regexp search. See the function
`re-search-backwards' for parameter details.  This will match the
largest possible match containing the first point matched
backwards."
  (interactive "sRE search backward greedy: ")
  (when (re-search-backward regexp bound noerror count)
    (let* ((this-beg (match-beginning 0))
           (this-end (match-end 0))
           (curr-beg (point-min))
           (last-biggest this-beg)
           (last-too-far curr-beg))
      (while (and (>= curr-beg (point-min))
                  (< curr-beg last-biggest))
        (goto-char curr-beg)
        (re-search-forward regexp nil t)
        (let ((too-far (>= this-beg (match-end 0))))
          (if too-far
              (setq last-too-far (max (match-end 0) curr-beg)
                    curr-beg (/ (+ last-too-far last-biggest) 2))
            (let ((contains (or
                             (and
                              (>= this-beg (match-beginning 0))
                              (< this-end (match-end 0)))
                             (and
                              (> this-beg (match-beginning 0))
                              (<= this-end (match-end 0))))))
              (when contains
                (setq this-beg (match-beginning 0)
                      this-end (match-end 0)))
              (setq last-biggest curr-beg
                    curr-beg (/ (+ last-too-far last-biggest) 2))))))
      (goto-char this-beg)
      (re-search-forward regexp)
      (goto-char (match-beginning 0)))))

(defun re-search-current (regexp &optional bound noerror)
  "Assumes that point is within a valid match for REGEXP and
finds the largest such match.  Leaves point at end of match.
Currently ignores BOUND and NOERROR."
  (let* ((this-point (point))
         (curr-point (point-max))
         (last-nil this-point)
         (last-too-far curr-point))
    (catch 'found
      (while (and (<= curr-point (point-max))
                  (< last-nil curr-point))
        (goto-char curr-point)
        (if (not (re-search-backward-greedy regexp nil t))
            (setq last-nil curr-point
                  curr-point (/ (+ last-too-far last-nil) 2))
          (let ((too-far (< this-point (match-beginning 0))))
            (if too-far
                (setq last-too-far (min (1+ (match-beginning 0)) curr-point)
                      curr-point (/ (+ last-too-far last-nil) 2))
              (let ((contains (and
                               (>= this-point (match-beginning 0))
                               (<= this-point (match-end 0)))))
                (if contains
                    (throw 'found t)
                  (setq last-nil (max curr-point (match-end 0))
                        curr-point (/ (+ last-too-far last-nil) 2)))))))))))

(defun get-last-group-of-regexp (regexp)
  "Return a regexp for the last character or character grouping in REGEXP."
  (save-match-data
    (let ((str (first (last (safe-split-string regexp "\n")))))
      (when (string-match "\\(\\[.*]\\|(.*)\\|.\\)\\+?$" str)
        (match-string 0 str)))))

(defun upcase-first (str)
  "Upcases the first character of STR only."
  (when (and (stringp str)
             (> (length str) 0))
    (concat (upcase (substring str 0 1))
            (substring str 1))))

(defun matching-delimiter (delimiter)
  "Return a matching delimiter that makes sense."
  (cond ((string= delimiter "(") ")")
        ((string= delimiter "{") "}")
        ((string= delimiter "[") "]")
        ((string= delimiter "<") ">")
        ((string= delimiter ")") "(")
        ((string= delimiter "}") "{")
        ((string= delimiter "]") "[")
        ((string= delimiter ">") "<")
        ((string= delimiter "`") "`")
        ((string= delimiter "'") "'")
        ((string= delimiter "\"") "\"")
        (t (error (format (concat "Unable to determine a matching "
                                  "delimiter for `%s'") 
                          delimiter)))))

(defun align-on (delimeter &optional beg end)
  "This function aligns parts of lines in the region starting
from the delimeter to the end of line at the delimeter furthest
in its line.  Returns the change in string size."
  (interactive "sAlign on: ")
  (let ((beg (if (use-region-p) (min (mark) (point))
               (if (integerp beg) beg 
                 (let ((beg (php-find-current-sexp-begin)))
                   (when (integerp beg) beg)))))
        (end (if (use-region-p) (max (mark) (point))
               (if (integerp end) end
                 (let ((end (php-find-current-sexp-end)))
                   (when (integerp end) end)))))
        (col 0)
        (change 0))
    (when (and (integerp beg) (integerp end))
      (save-excursion
        (goto-char beg)
        (catch 'done
          (while (< (point) end)
            (when (search-forward delimeter (line-end-position) t)
              (backward-char (length delimeter))
              (setq col (max col (current-column))))
            (let ((cur-line (line-number-at-pos)))
              (forward-line)
              (when (= cur-line (line-number-at-pos))
                (throw 'done t)))
            (beginning-of-line)))
        (goto-char beg)
        (catch 'done
          (while (< (point) end)
            (when (search-forward delimeter (line-end-position) t)
              (backward-char (length delimeter))
              (let ((delta (- col (current-column))))
                (insert (make-string delta ?\ ))
                (setf change (+ change delta))))
            (let ((cur-line (line-number-at-pos)))
              (forward-line)
              (when (= cur-line (line-number-at-pos))
                (throw 'done t)))
            (beginning-of-line)))))
    change))

(defun break-string (str &optional fill-regexp max-length)
  "Inserts linebreaks into STR so that no lines are more than
MAX-LENGTH.  Will not break inside of a word.  Respects
indentation and FILL-REGEX at the beginning of STR."
  (let* ((max-length (if (wholenump max-length) max-length 80))
         (indent (save-match-data 
                   (if (string-match (concat "^[[:space:]]*" fill-regexp) str) 
                       (match-string-no-properties 0 str)
                     "")))
         (lines `(,str)))
    (catch 'done 
      (let ((cur-line (first (last lines))))
        (while (> (length cur-line) max-length)
          (let* ((last-break -1)
                 (break-point (1+ (length indent)))
                 (at-new-line
                  (catch 'newline
                    (while (and break-point 
                                (< break-point max-length))
                      (setf last-break break-point
                            break-point (string-match ws-re cur-line 
                                                      (1+ break-point)))
                      (when (and (wholenump last-break)
                                 (string= (match-string-no-properties 0 
                                                                      cur-line) 
                                          "\n"))
                        (throw 'newline t))))))
            (setf break-point (if at-new-line 
                                  break-point
                                (if (wholenump last-break)
                                    last-break
                                  break-point)))
            (unless break-point
              (throw 'done t))
            (setf lines 
                  (append (butlast lines)
                          `(,(substring cur-line 0 break-point)
                            ,(concat indent (chomp (substring cur-line 
                                                              break-point)))))
                  cur-line (first (last lines)))))))
      (mapconcat (lambda (str) 
                   (replace-regexp-in-string "[[:space:]]+$" "" str)) 
                 lines "\n")))

(defun beginning-of-line-non-whitespace (&optional n)
  "Moves to the first non-whitespace character after moving
forward N lines, without all the exceptions that
``beginning-of-line-text'' has."
  (beginning-of-line n)
  (when (re-search-forward non-ws-re nil t)
    (backward-char)))

(provide 'string-utils)
