;;; php-string.el --- Functions that deal with PHP strings

;; Version: 1.0
;; Created: 10-17-2011
;; Copyright © 2011 Michael Dwyer
;; Author(s): 
;; Michael Dwyer <mdwyer@ehtech.in>

;;; *****
;;; About
;;; *****

;; php-string.el is a part of the php+-mode suite and contains
;; convenience functions for dealing with PHP strings, such as quote
;; toggling and conversion to/from {here,now}docs.

;; ********************************************************************

;; ************
;; REQUIREMENTS
;; ************

(require 'php-parse)
(require 'php-structure)

;; *********
;; FUNCTIONS
;; *********
; delcarations for compiler
(declare-function php+-mode "php+-mode")
(defun php-remove-this-concat (&optional digest-variables)
  "Remove the concatenation character at point if it exists and
it is proper to do so.  Will not join strings of unlike
quotations.  Will only DIGEST-VARIABLES if told to and if the
string to be joined to is double-quoted.  Returns the number of
characters removed."
  (interactive "P")
  (save-excursion
    (save-match-data
      (when (and (looking-at-p "\\.")
                 (not (or (php-in-text-structp)
                          (php-in-numberp))))
        (let* ((removed 0)
               (left-operand (save-excursion
                               (re-search-backward non-ws-re nil t)
                               `(,(point)
                                 ,(if (looking-at-p "[\"']")
                                      (char-to-string (char-after))
                                    (php-in-variablep)))))
               (left-point (first left-operand))
               (left-type (second left-operand))
               (right-operand (save-excursion
                                (forward-char)
                                (re-search-forward non-ws-re nil t)
                                (backward-char)
                                `(,(point)
                                  ,(if (looking-at-p "[\"']")
                                       (char-to-string (char-after))
                                     (php-skip-this-identifier)))))
               (right-point (first right-operand))
               (right-type (second right-operand)))
          (when (or (and (stringp left-type)
                         (stringp right-type)
                         (string= left-type right-type))
                    (and digest-variables
                         (or (and (integerp left-type)
                                  (integerp right-type))
                             (and (stringp left-type)
                                  (string= left-type "\"")
                                  (integerp right-type))
                             (and (integerp left-type)
                                  (stringp right-type)
                                  (string= right-type "\"")))))
            (when digest-variables
              (when (integerp left-type)
                (save-excursion
                  (goto-char left-type)
                  (insert "\"")
                  (goto-char (+ 2 left-point))
                  (insert "\"")
                  (setf removed (- removed 2)
                        left-point (+ 2 left-point)
                        right-point (+ 2 right-point))
                  (when (integerp right-type)
                    (setf right-type (+ 2 right-type)))))
              (when (integerp right-type)
                (save-excursion
                  (goto-char right-point)
                  (insert "\"")
                  (goto-char (1+ right-type))
                  (insert "\"")
                  (setf removed (- removed 2)
                        right-type (+ 2 right-type)))))
            (goto-char left-point)
            (let ((num-to-remove (1+ (- right-point left-point))))
              (delete-char num-to-remove)
              (setf removed (+ removed num-to-remove))))
          removed)))))

(defun php-implode-concats-in-statement (&optional digest-variables)
  "Run ``php-implode-concat'' on all concats within the current
statement.  Optionally, can be told to DIGEST-VARIABLES where
appropriate (as specified by \\[php-remove-this-concat]).
Returns the number of characters removed."
  (interactive "P")
  (let ((statement-begin (php-in-statementp)))
    (when (integerp statement-begin)
      (save-excursion
        (goto-char statement-begin)
        (let ((statement-end (save-excursion (php-skip-this-statement)))
              (total-removed 0))
          (catch 'done
            (when (php-next-text-struct statement-end)
              (while (< (point) statement-end)
                (let ((removed (php-implode-concat digest-variables)))
                  (setf statement-end (- statement-end removed)
                        total-removed (+ total-removed removed))
                  (php-skip-this-concat)
                  (unless (php-next-text-struct statement-end)
                    (throw 'done t))))))
          total-removed)))))

(defun php-implode-concat (&optional digest-variables)
  "Remove all concatenation operators between strings in the
  current concat.  Optionally, can be told to DIGEST-VARIABLES
  where appropriate (as specified by \\[php-remove-this-concat]).
  Returns the number of characters removed."
  (interactive "P")
  (save-excursion
    (save-match-data
      (let ((removed 0)
            (begin (php-beginning-of-concat)))
        (when (integerp begin)
          (goto-char begin)
          (let ((end (save-excursion (php-skip-this-concat)))
                (last-point -1))
            (catch 'done
              (while (< (point) end)
                (when (<= (point) last-point)
                  (error (concat "Infinite loop at point %s detected in "
                                 "php-implode-concat.") (point)))
                (setf last-point (point))
                (when (looking-at-p ws-re)
                  (unless (re-search-forward non-ws-re end t)
                    (throw 'done t))
                  (backward-char))
                (if (and (not (looking-at-p "\\."))
                         (php-in-identifierp))
                    (php-skip-this-identifier)
                  (when (looking-at-p "\\.")
                    (let ((change (php-remove-this-concat digest-variables)))
                      (setf end (- end change) removed (+ removed change)))
                    (if (php-in-stringp)
                        (php-skip-this-identifier)
                      (forward-char))))))))
        removed))))

(defun php-change-string-quotes (&optional arg new-quote)
  "Toggle the quoting of the current PHP string.  You may also
force which NEW-QUOTE to use.  When changing from double to
single quotes variables will be exhumed, single quotes escaped,
escaped double quotes and dollar signs unescaped and character
codes replaced with literals when possible.  When changing from
single to double quotes, double quotes and dollar signs will be
escaped, escaped single quotes unescaped and whitespace replaced
with character codes.  C-u will process all strings in the
current concat (forcing them to the new quoting of the first
string)."
  (interactive "P")
  (save-excursion
    (let* ((concat-begin (php-beginning-of-concat))
           (concat-end (save-excursion (php-skip-this-concat)))
           (old-concat-end concat-end))
      (when (and arg (integerp concat-begin))
        (goto-char concat-begin)
        (catch 'found-string
          (while (< (point) concat-end)
            (when (php-in-stringp) (throw 'found-string t))
            (php-skip-this-identifier)
            (save-match-data (re-search-forward non-ws-re concat-end t))
            (backward-char))))
      (let ((string-begin (php-in-stringp)))
        (when (integerp string-begin)
          (goto-char string-begin)
          (let* ((old-quote (char-after))
                 (new-quote (if (stringp new-quote) 
                                (string-to-char new-quote)
                              new-quote))
                 (new-quote (or new-quote 
                                (if (char-equal old-quote ?') ?\" ?'))))
            (catch 'done
              (while (< (point) concat-end)
                (if (and (characterp new-quote)
                         (or (char-equal new-quote ?')
                             (char-equal new-quote ?\"))
                         (not (char-equal old-quote new-quote)))
                    (let* ((end (save-excursion (php-skip-this-text-struct)))
                           (old-end end))
                      (delete-char 1)
                      (insert (char-to-string new-quote))
                      (while (< (point) (1- end))
                        (save-match-data
                          (if (char-equal new-quote ?\")
                              (cond ((or (looking-at-p "\\$")
                                         (looking-at-p "\"")) 
                                     (insert "\\")
                                     (forward-char)
                                     (setf end (1+ end)))
                                    ((looking-at-p "\\\\'")
                                     (delete-char 1)
                                     (setf end (1- end))
                                     (forward-char))
                                    ((>= (char-after) 127)
                                     (insert (format "\\x%02x" (char-after)))
                                     (delete-char 1)
                                     (setf end (+ end 3)))
                                    ((looking-at "[\n\r\t\v\f]")
                                     (let* ((match 
                                             (match-string-no-properties 0))
                                            (code 
                                             (cond ((string= match "\n") "n")
                                                   ((string= match "\r") "r")
                                                   ((string= match "\t") "t")
                                                   ((string= match "\v") "v")
                                                   ((string= match "\f") "f"))))
                                       (delete-char 1)
                                       (insert (concat "\\" code))
                                       (setf end (1+ end))))
                                    (t (forward-char)))
                            (cond ((looking-at-p "'")
                                   (insert "\\")
                                   (forward-char)
                                   (setf end (1+ end)))
                                  ((or (looking-at-p "\\\\\"")
                                       (looking-at-p "\\\\$"))
                                   (delete-char 1)
                                   (setf end (1- end))
                                   (forward-char))
                                  ((looking-at "\\\\\\([0-7]\\{1,3\\}\\)")
                                   (let* ((code (match-string-no-properties 1))
                                          (char (read (concat "#o" code)))
                                          (match-len (- (match-end 0)
                                                        (match-beginning 0))))
                                     (delete-char match-len)
                                     (insert (char-to-string char))
                                     (setf end (1+ (- end match-len)))))
                                  ((looking-at 
                                    "\\\\x\\([0-9a-fA-F]\\{1,2\\}\\)")
                                   (let* ((code (match-string-no-properties 1))
                                          (char (read (concat "#x" code)))
                                          (match-len (- (match-end 0)
                                                        (match-beginning 0))))
                                     (delete-char match-len)
                                     (insert (char-to-string char))
                                     (setf end (1+ (- end match-len)))))
                                  ((looking-at "\\\\[nrtvf]")
                                   (let* ((match (match-string-no-properties 0))
                                          (code 
                                           (cond ((string= match "\\n") "\n")
                                                 ((string= match "\\r") "\r")
                                                 ((string= match "\\t") "\t")
                                                 ((string= match "\\v") "\v")
                                                 ((string= match "\\f") "\f"))))
                                     (delete-char 2)
                                     (insert code)
                                     (setf end (1- end))))
                                  ((looking-at-p 
                                    (concat "$" 
                                            (substring 
                                             (php-type-regexp 'identifier) 2)))
                                   (save-match-data
                                     (let (quote-deleted)
                                       (while (looking-back 
                                               "[^\\]\\(?1:'\\)\\|\\(?1:{\\)")
                                         (let ((num-chars 
                                                (- (match-end 1)
                                                   (match-beginning 1))))
                                           (setf quote-deleted
                                                 (string= (match-string 1) "'"))
                                           (delete-region (match-beginning 1)
                                                          (match-end 1))
                                           (setf end (- end num-chars))))
                                       (when (and (> (point) string-begin)
                                                  (not quote-deleted))
                                         (insert "' . ")
                                         (setf end (+ end 4))))
                                     (php-skip-this-identifier nil t)
                                     (when (looking-at "}")
                                       (let ((num-chars 
                                              (- (match-end 0)
                                                 (match-beginning 0))))
                                         (delete-region (match-beginning 0)
                                                        (match-end 0))
                                         (setf end (- end num-chars))))
                                     (insert " . '")
                                     (setf end (+ end 4))
                                     (save-excursion
                                       (backward-char 4)
                                       (when (looking-at-p " \\. '\"")
                                         (delete-char 5)
                                         (setf end (- end 5))))))
                                  (t (forward-char))))))
                      (when (= (char-after) old-quote)
                        (delete-char 1)
                        (insert (char-to-string new-quote)))
                      (setf concat-end (+ concat-end (- end old-end))))
                  (php-skip-this-text-struct))
                (unless arg (throw 'done t))
                (unless (catch 'next-string
                          (while (< (point) concat-end)
                            (unless (looking-at-p non-ws-re)
                              (save-match-data 
                                (re-search-forward non-ws-re concat-end t))
                              (backward-char))
                            (when (php-in-stringp) (throw 'next-string t))
                            (or (php-skip-this-identifier)
                                (forward-char))))
                  (throw 'done t)))))))
      (- old-concat-end concat-end))))

(defun php-change-string<->doc (&optional doc-tag)
  "This function changes a PHP string to it's appropriate
{here,now}doc.  When going from double quoted string to heredoc,
All character codes will be expanded.  When going from heredoc to
double-quoted string, they will be replaced.  Optionally, use
DOC-TAG instead of \\[php-doc-tag]."
  (interactive)
  (save-excursion
    (let ((begin (php-in-stringp)))
      (when (integerp begin)
        (goto-char begin)
        (let ((end (save-excursion (php-skip-this-text-struct))))
          (if (looking-at-p "[\"']")
              (let ((doc-tag 
                     (if (and (stringp doc-tag)
                              (string-match (php-type-regexp 'identifier)
                                            doc-tag))
                         doc-tag
                       php-doc-tag))
                    (quote-type (char-after)))
                (save-excursion
                  (forward-sexp)
                  (backward-char)
                  (delete-char 1)
                  (newline)
                  (insert doc-tag)
                  (setf end (+ end (length doc-tag)))
                  (unless (looking-at ";")
                    (newline)
                    (setf end (1+ end))))
                (insert "<<<")
                (setf end (+ 3 end))
                (forward-char)
                (insert (concat doc-tag (char-to-string quote-type)))
                (setf end (+ 1 (length doc-tag) end))
                (newline)
                (setf end (1+ end))
                (save-excursion
                  (save-match-data
                    (while (< (point) end)
                      (if (or (looking-at "\\\\n\\|\\\\t\\|\\\\\"\\|\\\\'")
                              (looking-at "\\\\\\\\"))
                          (let ((code (match-string-no-properties 0)))
                            (delete-char 2)
                            (cond ((string= code "\\n")
                                   (newline))
                                  ((string= code "\\t")
                                   (insert "\t"))
                                  (t (insert (substring code 1))))
                            (setf end (1- end)))
                        (forward-char))))))
            (save-match-data
              (when (looking-at doc-begin-tag-re)
                (let* ((mq (match-string-no-properties 1))
                       (this-quote (if (> (length mq) 0) mq "\""))
                       end)
                  (save-excursion
                    (when (php-skip-this-text-struct)
                      (let ((has-semi (char-equal (char-after) ?\;)))
                        (beginning-of-line)
                        (kill-line 1)
                        (backward-char)
                        (insert this-quote)
                        (setf end (point))
                        (when has-semi (insert ";")))))
                  (setf end (- end (- (line-end-position) (point))))
                  (kill-line 1)
                  (insert this-quote)
                  (save-excursion
                    (save-match-data
                      (while (< (point) (1- end))
                        (if (looking-at 
                             (concat "\n\\|\t\\|\\\\\\|" this-quote))
                            (let ((code (match-string-no-properties 0)))
                              (delete-char 1)
                              (setf end (1- end))
                              (cond ((and (string= code "\n")
                                          (string= this-quote "\""))
                                     (insert "\\n")
                                     (setf end (+ 2 end)))
                                    ((and (string= code "\t")
                                          (string= this-quote "\""))
                                     (insert "\\t")
                                     (setf end (+ 2 end)))
                                    ((and (string= code "\\")
                                          (string= this-quote "\""))
                                     (insert "\\\\")
                                     (setf end (+ 2 end)))
                                    ((string= this-quote code)
                                     (insert (concat "\\" code))
                                     (setf end (+ 2 end)))
                                    (t (forward-char))))
                          (forward-char))))))))))))))

(defun php-change-bare-html<->heredoc (&optional ignore-echo doc-tag)
  "This function will change bare HTML (between ``?>'' and
``<?\\(php\\)?'' into a here doc.  It will work in the other
direction as well.  A prefix argument will tell the function to
add/remove echo when necessary.  Optionally, use DOC-TAG instead
of \\[php-doc-tag].  It will try to intelligently add/remove echo
commands unless you tell it to IGNORE-ECHO."
  (interactive "P")
  (let ((bare-html-begin (php-in-bare-htmlp)))
    (if (integerp bare-html-begin)
        (let ((doc-tag (if (and (stringp doc-tag)
                                (string-match (php-type-regexp 'identifier)
                                              doc-tag))
                           doc-tag
                         php-doc-tag)))
          (goto-char bare-html-begin)
          (let* ((bare-html-end-stats 
                  (save-excursion 
                    (save-match-data
                      `(,(php-skip-this-bare-html)
                        ,(re-search-forward "<\\?\\(php\\|=\\)?" nil t)
                        ,(match-string-no-properties 0)
                        ,(looking-at-p (concat ws-re "*\n" ws-re "*}"))))))
                 (bare-html-end (first bare-html-end-stats))
                 (ignore-echo (or ignore-echo
                                  (looking-back-p (concat "echo" ws-re "*"))))
                 (html-string (buffer-substring-no-properties bare-html-begin
                                                              bare-html-end))
                 (bare-html-end (or (second bare-html-end-stats) bare-html-end))
                 (bare-html-end-string (third bare-html-end-stats))
                 (bare-html-end-brace (fourth bare-html-end-stats)))
            (setf html-string (concat "<<<\"" doc-tag "\"\n" 
                                      (chomp html-string) "\n" doc-tag ";" 
                                      (unless bare-html-end-brace "\n")
                                      (when (and (not ignore-echo)
                                                 (string= bare-html-end-string 
                                                          "<?="))
                                        "echo ")))
            (goto-char (max (- (point) 2) (point-min)))
            (unless ignore-echo
              (setf html-string (concat "echo " html-string))
              (when (not (looking-back-p (concat "^" ws-re "*")))
                (setf html-string (concat "\n" html-string))))
            (unless (looking-at-p "\\?>")
              (setf html-string (concat "<?php\n" html-string)))
            (delete-region (point) bare-html-end)
            (save-excursion
              (re-search-backward non-ws-re nil t)
              (forward-char)
              (when (looking-back-p (concat "[^[:space:]\n;{}]" ws-re "*"))
                (insert ";"))
              (php-delete-horizontal-space))
            (when (looking-back-p "[^[:space:]\n{][[:space:]]*\n[[:space:]]*")
              (newline-and-indent))
            (save-excursion
              (insert html-string)
              (unless (or (not (looking-back-p ";[[:space:]]*"))
                          (looking-at-p "[[:space:]]*[}\n]"))
                (newline)))
            (save-match-data
              (re-search-forward non-ws-re nil t))
            (indent-according-to-mode)
            (end-of-line)))
      (let ((string-begin (php-in-stringp)))
        (when (integerp string-begin)
          (goto-char string-begin)
          (save-match-data
            (when (looking-at doc-begin-tag-re)
              (let* ((doc-tag-length (- (match-end 0) (match-beginning 0)))
                     (string-end-pair 
                      (save-excursion
                        (php-skip-this-text-struct)
                        (forward-char)
                        `(,(point)
                          ,(re-search-backward doc-end-tag-re nil t))))
                     (string-end (second string-end-pair))
                     (text-string 
                      (buffer-substring-no-properties (+ string-begin
                                                         doc-tag-length)
                                                      string-end))
                     (string-end (first string-end-pair)))
                (unless (or ignore-echo
                            (not (looking-back (concat "echo" ws-re "*"))))
                  (setf string-begin (match-beginning 0)))
                (setf text-string (concat "?>\n" (chomp text-string) 
                                          "\n<?php"))
                (delete-region string-begin string-end)
                (save-excursion
                  (insert text-string)
                  (re-search-forward "<\\?php" nil t)
                  (beginning-of-line-non-whitespace)
                  (indent-according-to-mode))
                (indent-according-to-mode)
                (end-of-line)))))))))
                
(defun php-combine-scripts (previous-script)
  "This function will combine two PHP scripts whose are
consecutive and separated only by whitespace.  Will combine the
next script with it's previous script unless PREVIOUS-SCRIPT is
specified."
  (interactive "P")
  (save-excursion
    (let ((script-begin (php-in-scriptp)))
      (when (integerp script-begin)
        (catch 'nothing-to-combine
          (if (not previous-script)
              (php-skip-this-script)
            (goto-char script-begin)
            (save-match-data 
              (unless (re-search-backward "\\?>" nil t)
                (throw 'nothing-to-combine nil)))
            (forward-char 2))
          (save-match-data
            (when (looking-at (concat ws-re "*<\\?\\(php\\|=\\)?"))
              (let ((echo-tag (string= (match-string-no-properties 1) "=")))
                (backward-char 2)
                (delete-region (- (match-beginning 0) 2) (match-end 0))
                (php-delete-horizontal-space nil t)
                (newline-and-indent)
                (when echo-tag
                  (insert "echo "))))))))))

(defun php-single-quote-string (string)
  "This function single quotes with given string, escaping as necessary."
  (concat "'" (replace-regexp-in-string "'" "\\\\'" string) "'"))

(defun php-comment-dwim ()
  "This function will run comment-dwim if a region is set, otherwise it will
comment out the current statement."
  (interactive)
  (let ((column (current-column))
        (line (line-number-at-pos)))
    (save-excursion
      (if (region-active-p)
          (comment-dwim current-prefix-arg)
        (beginning-of-line-non-whitespace)
        (if (php-parse-current 'comment)
            (let (begin
                  end
                  (current-point (point))
                  (last-line (save-excursion
                               (goto-char (point-max))
                               (line-number-at-pos))))
              (while (and (php-parse-current 'comment)
                          (< 1 (line-number-at-pos)))
                (beginning-of-line-non-whitespace 0))
              (unless (php-parse-current 'comment)
                (forward-line))
              (beginning-of-line-non-whitespace)
              (setq begin (line-beginning-position))
              (while (and (php-parse-current 'comment)
                          (< (line-number-at-pos) last-line))
                (beginning-of-line-non-whitespace 2))
              (unless (php-parse-current 'comment)
                (forward-line -1))
              (end-of-line)
              (setq end (point))
              (let ((sub (buffer-substring begin end)))
                (kill-region begin end)
                (insert
                 (with-temp-buffer
                   (php+-mode)
                   (insert "<?php")
                   (newline)
                   (let ((offset (point)))
                     (insert sub)
                     (goto-char (+ offset (- current-point begin)))
                     (uncomment-region (point-min) (point-max))
                     (unless current-prefix-arg
                       (let ((statement-begin (php-in-statementp))
                             (statement-end (php-skip-this-statement)))
                         (unless (= statement-end (point-max))
                           (condition-case nil
                               (comment-region (1+ statement-end) (point-max))
                             (error
                              nil)))
                         (condition-case nil
                             (comment-region offset (1- statement-begin))
                           (error
                            nil))))
                     (buffer-substring offset (point-max)))))))
          (let ((begin (php-in-statementp)))
            (when (wholenump begin)
              (push-mark (point))
              (goto-char begin)
              (push-mark (point))
              (php-skip-this-statement)
              (exchange-point-and-mark)
              (comment-dwim current-prefix-arg))))))
    (goto-char (point-min))
    (forward-line (1- line))
    (beginning-of-line)
    (forward-char column)))

(provide 'php-string)
