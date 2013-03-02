;;; php-lineup.el --- Functions that deal with PHP indentation

;; Version: 1.0
;; Created: 11-23-2011
;; Copyright © 2011 Michael Dwyer
;; Author(s): 
;; Michael Dwyer <mdwyer@ehtech.in>

;;; *****
;;; About
;;; *****

;; php-lineup.el is a part of the php+-mode suite and contains functions
;; that are used by the c-mode indentation engine
;; (indent-according-to-mode et. al.).

;; ********************************************************************

;; ************
;; REQUIREMENTS
;; ************
(require 'cc-defs)
(require 'cc-align)
(require 'php-structure)

;; *********
;; CUSTOMIZE
;; *********

(defcustom php-basic-offset 4
  "This is the default indentation for PHP lines."
  :type 'integer
  :group 'php+-mode)

(defcustom php-html-basic-offset (/ php-basic-offset 2)
  "This is the default indentation for HTML elements"
  :type 'integer
  :group 'php+-mode)

;; *********
;; FUNCTIONS
;; *********

(defun php-cpp-macro-lineup (element)
  "This lineup function handles cpp-macro elements."
  (save-excursion
    (beginning-of-line-non-whitespace)
    (make-vector 1 (or (when (looking-at-p "\\?>")
                         (* (php-get-current-sexp-level "{") 
                            php-basic-offset))
                       0))))

(defun php-arglist-intro-lineup (element)
  "This lineup function handles arglist-intro elements."
  (make-vector 1 (save-excursion 
                   (goto-char (c-langelem-pos element))
                   (+ php-basic-offset (current-column)))))

(defun php-arglist-close-lineup (element)
  "This lineup function handles arglist-close elements."
  (make-vector 1 (save-excursion 
                   (goto-char (c-langelem-pos element))
                   (current-column))))

(defun php-knr-argdecl-lineup (element)
  "This lineup function handles knr-argdecl elements."
  (let* ((statement-begin (or (php-in-statementp)
                              (save-excursion
                                (re-search-backward non-ws-re nil t)
                                (- (php-in-statementp)))))
         (statement-cont (< 0 statement-begin))
         (statement-begin (abs statement-begin))
         (statement-begin-col/line (save-excursion
                                     (goto-char statement-begin)
                                     `(,(current-column)
                                       ,(line-number-at-pos))))
         (statement-begin-col (first statement-begin-col/line))
         (statement-cont (and statement-cont
                              (> (line-number-at-pos) 
                                 (second statement-begin-col/line)))))
    (make-vector 1 (+ statement-begin-col 
                      (if statement-cont php-basic-offset 0)))))
    
(defun php-topmost-intro-lineup (element)
  "This lineup function handles topmost intro elements."
  (save-excursion
    (goto-char (c-langelem-pos element))
    (if (looking-at-p "break;") '+ 0)))

(defun php-topmost-intro-cont-lineup (element)
  "This lineup function handles topmost intro cont elements."
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "[[:space:]]*<\\?\\(php\\|=\\)?")
        (make-vector 1 (save-match-data
                         (if (re-search-backward "\\?>" nil t)
                             (current-column)
                           0)))
      (let ((col (or (let ((this-script-begin (php-in-scriptp)))
                       (when (wholenump this-script-begin)
                         (goto-char this-script-begin)
                         (save-excursion
                           (let ((cur-col (current-column)))
                             (php-skip-this-identifier)
                             (save-match-data
                               (if (and (re-search-forward non-ws-re nil t)
                                        (looking-back-p "}"))
                                   (1- (current-column))
                                 cur-col))))))
                     0)))
        (make-vector 1 col)))))

(defun php-comment-intro-lineup (element)
  "This lineup function handles comment intros."
  (save-excursion
    (let ((comment-begin (php-in-commentp)))
      (if (integerp comment-begin)
          (goto-char comment-begin)
        (save-match-data
          (re-search-forward non-ws-re nil t))
        (backward-char))
      (re-search-backward non-ws-re nil t)
      (let ((in-brace (save-match-data 
                        (when (looking-at "{") (match-beginning 0)))))
        (or (looking-at-p "}")
            (goto-char (php-in-statementp)))
        (make-vector 1 (+ (current-column)
                          (if in-brace php-basic-offset 0)))))))

(defun php-comment-lineup (element)
  "This lineup function handles bare HTML and docs (which are
syntax classed as comments."
  (save-excursion
    (beginning-of-line-non-whitespace)
    (if (php-in-commentp)
        (c-lineup-C-comments element)
      (if (looking-at-p "<\\?")
          (make-vector 1 (or (save-excursion
                               (catch 'found
                                 (while (re-search-backward "\\?>" nil t)
                                   (if (< (save-excursion 
                                            (when (re-search-backward "<\\?" 
                                                                      nil t)
                                              (line-number-at-pos)))
                                          (line-number-at-pos))
                                       (throw 'found (current-column))))))
                             0))
        (let* ((in-string (php-in-text-structp))
               (end-tag (when (wholenump in-string)
                          (save-excursion
                            (goto-char in-string)
                            (save-match-data
                              (when (looking-at doc-begin-tag-re)
                                (match-string-no-properties 2)))))))
          (cond ((and end-tag
                      (let (case-fold-search) 
                        (looking-at-p (concat end-tag "[;\n]"))))
                 (make-vector 1 0))
                (t (if (looking-back-p (concat doc-begin-tag-re ws-re "*"))
                       (make-vector 1 0)
                     (let ((min-col (* (php-get-current-sexp-level "{") 
                                       php-basic-offset)))
                       (make-vector 1 
                                    (max min-col 
                                         (php-comment-lineup-calc-col))))))))))))

(defun php-comment-lineup-calc-col ()
  (save-match-data
    (let* ((tagp (looking-at "<\\(/?\\)\\(\\(?:\\sw\\|-\\)+\\)"))
           (end-tagp 
            (and tagp (not (zerop (length (match-string-no-properties 1))))))
           (tag-name (and tagp (downcase (match-string-no-properties 2))))
           (matching-tag-col-re 
            (if end-tagp
                (concat "<\\(/?\\)\\(" tag-name "\\)")
              "<\\(/?\\)\\(\\(?:\\sw\\|-\\)+\\)"))
           (end-tag-num (if (or end-tagp (not tagp)) -1 0))
           (matching-tag-col
            (php-comment-lineup-matching-tag-col))
           (matching-tag-name 
            (downcase (match-string-no-properties 2)))
           (matching-tag-endp 
            (or (not (zerop (length (match-string-no-properties 1))))
                (html-current-match-singleton-tag-p)))
           (tag-col 
            (if matching-tag-col
                (+ matching-tag-col
                   (if (and tagp
                            (or (zerop end-tag-num)
                                end-tagp 
                                matching-tag-endp))
                       0 
                     (if (and (string= matching-tag-name "script")
                              (save-excursion
                                (when (re-search-backward ">\\|<script" nil t)
                                  (string= (match-string 0) ">"))))
                         php-basic-offset
                       php-html-basic-offset)))
              (save-excursion
                (beginning-of-line-non-whitespace 0)
                (current-column)))))
      tag-col)))

(defun php-comment-lineup-matching-tag-col ()
  (or (save-excursion
        (catch 'found
          (while (re-search-backward matching-tag-col-re nil t)
            (when (eq 'bare-html (php-get-text-type))
              (let ((tag (match-string-no-properties 2))
                    (tag-end-tagp 
                     (not (zerop (length (match-string-no-properties 1))))))
                (php-comment-lineup-set-end-tag-num)
                (when (>= end-tag-num 0)
                  (throw 'found (current-column))))))))
      0))

(defun php-comment-lineup-set-end-tag-num ()
  (setf end-tag-num 
        (+ end-tag-num (if tag-end-tagp 
                           -1 
                         (if (and tagp (html-current-match-singleton-tag-p))
                             0 1)))))

(defun html-current-match-singleton-tag-p ()
  (save-excursion
    (goto-char (match-beginning 0))
    (catch 'found
      (save-match-data
        (while (re-search-forward "<\\?\\|[\"'>]" nil t)
          (let ((m (match-string-no-properties 0)))
            (cond ((member m '("\"" "'"))
                   (backward-char)
                   (unless (php-skip-this-text-struct nil t)
                     (throw 'found nil)))
                  ((string= m "<?")
                   (unless (php-skip-this-script)
                     (throw 'found nil)))
                  (t (throw 'found 
                            (char-equal (char-before (1- (point))) ?/))))))))))

(defun php-defun-close-lineup (element)
  "This lineup function handles defun close lineup."
  (save-excursion
    (goto-char (c-langelem-pos element))
    (beginning-of-line-non-whitespace)
    (make-vector 1 (current-column))))

(defun php-statement-lineup (element)
  "This lineup function handles a bug in switch statement
indentation."
  (save-excursion
    (beginning-of-line-non-whitespace)
    (cond ((php-in-text-structp)
           (if (looking-at-p doc-end-tag-re) (make-vector 1 0) 0))
          ((looking-at-p "<\\?\\(php\\|=\\)?") -1)
          (t (let ((level (php-get-current-sexp-level "{")))
               (goto-char (c-langelem-pos element))
               (if (and (not (php-in-text-structp))
                        (looking-at-p "case"))
                   '+ 
                 (make-vector 1 (current-column))))))))

(defun php-statement-cont-lineup (element)
  "This lineup function handles indentation for statements."
  (save-excursion
    (save-match-data
      (beginning-of-line-non-whitespace)
      (cond ((php-inside-text-structp) 0)
            ((looking-at-p "[\]})]")
             (forward-char)
             (backward-sexp)
             (beginning-of-line-non-whitespace)
             (make-vector 1 (current-column)))
            ((looking-at-p "<\\?\\(php\\|=\\)?")
             (re-search-backward "\\?>" nil t)
             (make-vector 1 (1- (current-column))))
            ((looking-back (concat "<\\?\\(php\\|=\\)?" ws-re "*")) 
             (make-vector 1 (* php-basic-offset 
                               (php-get-current-sexp-level "{"))))
            (t (make-vector 1 (save-excursion
                                (goto-char (php-in-statementp))
                                (+ (current-column) php-basic-offset))))))))

(defun php-string-lineup (element)
  "This lineup function handles indentation for strings."
  (save-excursion
    (make-vector 1 (or (let ((in-string (php-in-text-structp)))
                         (when (wholenump in-string)
                           (goto-char (1+ in-string))
                           (current-column)))
                       (current-column)))))

(defun php-brace-list-intro-lineup (element)
  "This lineup function handles indentation for brace list intros."
  (save-excursion
    (when (re-search-backward "\{" nil t)
      (beginning-of-line-non-whitespace)
      (make-vector 1 (+ php-basic-offset (current-column))))))

(defun php-brace-list-entry-lineup (element)
  "This lineup function handles indentation for brace list entries."
  (save-excursion
    (beginning-of-line)
    (save-match-data
      (when (re-search-backward non-ws-re nil t)
        (forward-char)
        (save-match-data
          (if (looking-back "<\\?\\(php\\)?")
              (save-excursion
                (goto-char (match-beginning 0))
                (make-vector 1 (current-column)))
            (backward-char)
            (let ((break-statement (not (looking-at-p ";"))))
              (unless (looking-at-p "}")
                (goto-char (php-in-statementp)))
              (make-vector 1 (+ (if break-statement php-basic-offset 0) 
                                (current-column))))))))))

(defun php-brace-list-close-lineup (element)
  "This lineup function handles indentation for brace list closes."
  (save-excursion
    (beginning-of-line)
    (save-match-data
      (when (re-search-forward "}" nil t)
        (backward-sexp)
        (beginning-of-line-non-whitespace)
        (make-vector 1 (current-column))))))

(defun php-func-decl-cont (element)
  "This lineup function handles indentation for function
declaration continuations."
  (make-vector 1 (* php-basic-offset (php-get-current-sexp-level))))

(provide 'php-lineup)
