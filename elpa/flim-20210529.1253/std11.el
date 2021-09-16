;;; std11.el --- STD 11 functions for GNU Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 1995,96,97,98,99,2000,01,02 Free Software Foundation, Inc.

;; Author:   MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: mail, news, RFC 822, STD 11

;; This file is part of FLIM (Faithful Library about Internet Message).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'pccl)
(require 'static)

;;; @ fetch
;;;

(defconst std11-field-name-regexp "[!-9;-~]+")
(defconst std11-field-head-regexp
  (concat "^" std11-field-name-regexp ":"))
(defconst std11-next-field-head-regexp
  (concat "\n" std11-field-name-regexp ":"))

(defun std11-field-end (&optional bound)
  "Move to end of field and return this point.
The optional argument BOUNDs the search; it is a buffer position."
  (if (re-search-forward std11-next-field-head-regexp bound t)
      (goto-char (match-beginning 0))
    (if (re-search-forward "^$" bound t)
	(goto-char (1- (match-beginning 0)))
      (end-of-line)
      (point))))

;;;###autoload
(defun std11-fetch-field (name)
  "Return the value of the header field NAME.
The buffer is expected to be narrowed to just the headers of the message."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^" name ":[ \t]*") nil t)
	  (buffer-substring-no-properties (match-end 0) (std11-field-end))))))

;;;###autoload
(defun std11-narrow-to-header (&optional boundary)
  "Narrow to the message header when needed.
If BOUNDARY is not nil, it is used as message header separator."
  (goto-char (point-min))
  (when (re-search-forward
	 (if boundary (concat "^\\(" (regexp-quote boundary) "\\)?$")
	   "^$")
	 nil t)
    (narrow-to-region (point-min) (match-beginning 0))))

;;;###autoload
(defun std11-field-body (name &optional boundary)
  "Return the value of the header field NAME.
If BOUNDARY is not nil, it is used as message header separator."
  (save-excursion
    (save-restriction
      (inline (std11-narrow-to-header boundary)
	      (std11-fetch-field name)))))

(defun std11-find-field-body (field-names &optional boundary)
  "Return the first found field-body specified by FIELD-NAMES
of the message header in current buffer. If BOUNDARY is not nil, it is
used as message header separator."
  (save-excursion
    (save-restriction
      (std11-narrow-to-header boundary)
      (let ((case-fold-search t)
	    field-name)
	(catch 'tag
	  (goto-char (point-min))
	  (while (setq field-name (car field-names))
	    (if (re-search-forward (concat "^" field-name ":[ \t]*") nil t)
		(throw 'tag
		       (buffer-substring-no-properties
			(match-end 0) (std11-field-end))))
	    (setq field-names (cdr field-names))))))))

(defun std11-field-bodies (field-names &optional default-value boundary)
  "Return list of each field-bodies of FIELD-NAMES of the message header
in current buffer. If BOUNDARY is not nil, it is used as message
header separator."
  (save-excursion
    (save-restriction
      (std11-narrow-to-header boundary)
      (let* ((case-fold-search t)
	     (dest (make-list (length field-names) default-value))
	     (s-rest field-names)
	     (d-rest dest)
	     field-name)
	(while (setq field-name (car s-rest))
	  (goto-char (point-min))
	  (if (re-search-forward (concat "^" field-name ":[ \t]*") nil t)
	      (setcar d-rest
		      (buffer-substring-no-properties
		       (match-end 0) (std11-field-end))))
	  (setq s-rest (cdr s-rest)
		d-rest (cdr d-rest)))
	dest))))

(defun std11-header-string (regexp &optional boundary)
  "Return string of message header fields matched by REGEXP.
If BOUNDARY is not nil, it is used as message header separator."
  (let ((case-fold-search t))
    (save-excursion
      (save-restriction
	(std11-narrow-to-header boundary)
	(goto-char (point-min))
	(let (field header)
	  (while (re-search-forward std11-field-head-regexp nil t)
	    (setq field
		  (buffer-substring (match-beginning 0) (std11-field-end)))
	    (when (string-match regexp field)
	      (setq header (cons "\n" (cons field header)))))
	  (apply 'concat (nreverse header)))))))

(defun std11-header-string-except (regexp &optional boundary)
  "Return string of message header fields not matched by REGEXP.
If BOUNDARY is not nil, it is used as message header separator."
  (let ((case-fold-search t))
    (save-excursion
      (save-restriction
	(std11-narrow-to-header boundary)
	(goto-char (point-min))
	(let (field header)
	  (while (re-search-forward std11-field-head-regexp nil t)
	    (setq field
		  (buffer-substring (match-beginning 0) (std11-field-end)))
	    (if (not (string-match regexp field))
		(setq header (cons "\n" (cons field header)))))
	  (apply 'concat (nreverse header)))))))

(defun std11-collect-field-names (&optional boundary)
  "Return list of all field-names of the message header in current buffer.
If BOUNDARY is not nil, it is used as message header separator."
  (save-excursion
    (save-restriction
      (std11-narrow-to-header boundary)
      (goto-char (point-min))
      (let (dest name)
	(while (re-search-forward std11-field-head-regexp nil t)
	  (setq name (buffer-substring-no-properties
		      (match-beginning 0)(1- (match-end 0))))
	  (or (member name dest)
	      (setq dest (cons name dest))))
	dest))))


;;; @ unfolding
;;;

(defcustom std11-unfold-strip-leading-tab t
  "When non-nil, `std11-unfold-string' strips leading TAB, which is
mainly added by incorrect folding."
  :group 'news
  :group 'mail
  :type 'boolean)

;;;###autoload
(defun std11-unfold-string (string)
  "Unfold STRING as message header field."
  (let (dest
	(p 0))
    (while (string-match "\\( ?\\)\n\\([ \t]\\)" string p)
      (setq dest (cons (unless (and std11-unfold-strip-leading-tab
				    (< (match-beginning 0) (match-end 1))
				    (eq (aref string (match-beginning 2)) ?\t))
			 (match-string 2 string))
		       (cons (substring string p (match-end 1))
			     dest))
	    p (match-end 0)))
    (apply 'concat (nreverse (cons (substring string p) dest)))))


;;; @ quoted-string
;;;

(defun std11-wrap-as-quoted-pairs (string specials)
  (let (dest
	(i 0)
	(b 0)
	(len (length string)))
    (while (< i len)
      (if (memq (aref string i) specials)
	  (setq dest (cons "\\" (cons (substring string b i) dest))
		b i))
      (setq i (1+ i)))
    (apply 'concat (nreverse (cons (substring string b) dest)))))

(defconst std11-non-qtext-char-list '(?\" ?\\ ?\r ?\n))

(defun std11-wrap-as-quoted-string (string)
  "Wrap STRING as RFC 822 quoted-string."
  (concat "\""
	  (std11-wrap-as-quoted-pairs string std11-non-qtext-char-list)
	  "\""))

(defun std11-strip-quoted-pair (string)
  "Strip quoted-pairs in STRING."
  (let (dest
	(b 0)
	(i 0)
	(len (length string)))
    (while (< i len)
      (if (eq (aref string i) ?\\)
	  (setq dest (cons (substring string b i) dest)
		b (1+ i)
		i (+ i 2))
	(setq i (1+ i))))
    (apply 'concat (nreverse (cons (substring string b) dest)))))

(defun std11-strip-quoted-string (string)
  "Strip quoted-string STRING."
  (let ((len (length string)))
    (or (and (>= len 2)
	     (let ((max (1- len)))
	       (and (eq (aref string 0) ?\")
		    (eq (aref string max) ?\")
		    (std11-strip-quoted-pair (substring string 1 max)))))
	string)))


;;; @ lexical analyze
;;;

(unless-broken ccl-usable
(define-ccl-program std11-default-ccl-lexical-analyzer
  ;; r0 input
  ;; r1 flag means any character exists.
  ;; r2 in parse flag
  ;;    1 atom, 2 spaces, 3 quoted string or domain literal, 4 comment
  ;; r3 comment depth
  (eval-when-compile
    (let* ((wrt `(if (r0 == ?\") (write "\\\"")
		   (if (r0 == ?\\) (write "\\\\")
		     (write r0))))
	   (atm `((branch r2
			  ((r2 = 1)
			   (write "(atom . \"")
			   (write-read-repeat r0))
			  (write-read-repeat r0)
			  ((write "\")")
			   (r2 = 1)
			   (write "(atom . \"")
			   (write-read-repeat r0)))))
	   (spc `((if r2 ((write "\")") (r2 = 0)))
		  (write "(specials . \"")
		  ,wrt
		  (write "\")")
		  (read r0)
		  (repeat)))
	   (sp  `((branch r2
			  ((r2 = 2)
			   (write "(spaces . \"")
			   (write-read-repeat r0))
			  ((write "\")")
			   (r2 = 2)
			   (write "(spaces . \"")
			   (write-read-repeat r0))
			  (write-read-repeat r0))))
	   (enc (lambda (name tag)
		  `((if r2 ((write "\")")))
		    (write ,(concat "(" name " . \""))
		    (r2 = 3)
		    (loop
		     (read-branch
		      r0
		      ,@(let* ((count (1+ (max tag ?\\)))
			       (result (make-vector count '(write-repeat r0))))
			  (aset result tag '(break))
			  (aset result ?\\ `((write "\\\\")
					     (read r0)
					     ,wrt
					     (repeat)))
			  (aset result ?\" '((write "\\\"")
					     (repeat)))
			  (mapcar 'identity result)))
		     (write-repeat r0))
		    (write "\")")
		    (r2 = 0)
		    (read r0)
		    (repeat))))
	   (qs (funcall enc "quoted-string" ?\"))
	   (dl (funcall enc "domain-literal" ?\]))
	   (cm  `((if r2 ((write "\")")))
		  (write "(comment . \"")
		  (r2 = 4)
		  (r3 = 1)
		  (loop
		   (read-branch
		    r0
		    ,@(let* ((count (1+ (max ?\( ?\) ?\\)))
			     (result (make-vector count '(write-repeat r0))))
			(aset result ?\( '((r3 += 1) (write-repeat r0)))
			(aset result ?\) '((r3 -= 1)
					   (if (r3 < 1) (break)
					     (write-repeat r0))))
			(aset result ?\\ `((write "\\\\")
					   (read r0)
					   ,wrt
					   (repeat)))
			(aset result ?\"
			      '((write "\\\"") (repeat)))
			(mapcar 'identity result)))
		   (write-repeat r0))
		  (write "\")")
		  (r2 = 0)
		  (read r0)
		  (repeat))))
      `(8
	((r2 = 0)
	 (read r0)
	 (r1 = 1)
	 (write "((")
	 (loop
	  (branch r0
		  ,@(mapcar (lambda (elt)
			      (eval elt))
			    '(atm atm atm atm atm atm atm atm
				  atm sp  sp  atm atm atm atm atm
				  atm atm atm atm atm atm atm atm
				  atm atm atm atm atm atm atm atm
				  sp  atm qs  atm atm atm atm atm
				  cm  spc atm atm spc atm spc atm
				  atm atm atm atm atm atm atm atm
				  atm atm spc spc spc atm spc atm
				  spc atm atm atm atm atm atm atm
				  atm atm atm atm atm atm atm atm
				  atm atm atm atm atm atm atm atm
				  atm atm atm dl  spc spc)))
	  ,@atm))
	((branch r1
		 (write "(nil . t)")
		 (branch r2
			 (write ") . t)")
			 (write "\")) . t)")
			 (write "\")) . t)")
			 (write "\")))")
			 (write "\")))")))))))))

(defcustom std11-ccl-lexical-analyzer
  (static-unless (or (broken-p 'ccl-execute-eof-block)
		     (broken-p 'ccl-usable))
    'std11-default-ccl-lexical-analyzer)
  "Specify CCL-program symbol for `std11-lexical-analyze'.
When nil, do not use CCL.

CCL-program returns a string which expresses a cons.  When cons's
cdr is non-nil, CCL-program succeeds in analyzing and car is
analyzed result.  When cdr is nil, CCL-program fails in analyzing.
If you modify `std11-lexical-analyzer', set this variable to nil
or prepare corresponding CCL-program."
  :group 'news
  :group 'mail
  :type '(choice symbol (const :tag "Do not use CCL." nil)))

(defcustom std11-lexical-analyzer
  '(std11-analyze-quoted-string
    std11-analyze-domain-literal
    std11-analyze-comment
    std11-analyze-spaces
    std11-analyze-special
    std11-analyze-atom)
  "*List of functions to return result of lexical analyze.
Each function must have two arguments: STRING and START.
STRING is the target string to be analyzed.
START is start position of STRING to analyze.

Previous function is preferred to next function.  If a function
returns nil, next function is used.  Otherwise the return value will
be the result."
  :group 'news
  :group 'mail
  :type '(repeat function))

(eval-and-compile
  (defconst std11-space-char-list '(?\s ?\t ?\n))
  (defconst std11-special-char-list '(?\] ?\[
					  ?\( ?\) ?< ?> ?@
					  ?, ?\; ?: ?\\ ?\"
					  ?.)))
;; (defconst std11-spaces-regexp
;;   (eval-when-compile (concat "[" std11-space-char-list "]+")))

(defconst std11-non-atom-regexp
  (eval-when-compile
    (concat "[" std11-special-char-list std11-space-char-list "]")))

(defconst std11-atom-regexp
  (eval-when-compile
    (concat "[^" std11-special-char-list std11-space-char-list "]+")))

(defun std11-analyze-spaces (string start)
  (if (and (string-match (eval-when-compile
			   (concat "[" std11-space-char-list "]+"))
			 string start)
	   (= (match-beginning 0) start))
      (let ((end (match-end 0)))
	(cons (cons 'spaces (substring string start end))
	      ;;(substring string end)
	      end))))

(defun std11-analyze-special (string start)
  (if (and (> (length string) start)
	   (memq (aref string start) std11-special-char-list))
      (cons (cons 'specials (substring string start (1+ start)))
	    ;;(substring string 1)
	    (1+ start))))

(defun std11-analyze-atom (string start)
  (if (string-match std11-non-atom-regexp string start)
      (if (> (match-beginning 0) start)
	  (cons (cons 'atom (substring string start (match-beginning 0)))
		(match-beginning 0))
	nil)
    (cons (cons 'atom (substring string start))
	  (length string)))
  ;; (if (and (string-match std11-atom-regexp string start)
  ;;          (= (match-beginning 0) start))
  ;;     (let ((end (match-end 0)))
  ;;       (cons (cons 'atom (substring string start end))
  ;;             ;;(substring string end)
  ;;             end)
  ;;       ))
  )

(defun std11-check-enclosure (string open close &optional recursive from)
  (let ((len (length string))
	(i (or from 0)))
    (if (and (> len i)
	     (eq (aref string i) open))
	(let (p chr)
	  (setq i (1+ i))
	  (catch 'tag
	    (while (< i len)
	      (setq chr (aref string i))
	      (cond ((eq chr ?\\)
		     (setq i (1+ i))
		     (if (>= i len)
			 (throw 'tag nil))
		     (setq i (1+ i)))
		    ((eq chr close)
		     (throw 'tag (1+ i)))
		    ((eq chr open)
		     (if (and recursive
			      (setq p (std11-check-enclosure
				       string open close recursive i)))
			 (setq i p)
		       (throw 'tag nil)))
		    (t
		     (setq i (1+ i))))))))))

(defun std11-analyze-quoted-string (string start)
  (let ((p (std11-check-enclosure string ?\" ?\" nil start)))
    (if p
	(cons (cons 'quoted-string (substring string (1+ start) (1- p)))
	      ;;(substring string p))
	      p))))

(defun std11-analyze-domain-literal (string start)
  (let ((p (std11-check-enclosure string ?\[ ?\] nil start)))
    (if p
	(cons (cons 'domain-literal (substring string (1+ start) (1- p)))
	      ;;(substring string p))
	      p))))

(defun std11-analyze-comment (string start)
  (let ((p (std11-check-enclosure string ?\( ?\) t start)))
    (if p
	(cons (cons 'comment (substring string (1+ start) (1- p)))
	      ;;(substring string p))
	      p))))

;;;###autoload
(defun std11-lexical-analyze (string &optional analyzer start)
  "Analyze STRING as lexical tokens of STD 11."
  (let (len dest ret)
    (if (and std11-ccl-lexical-analyzer
	     (null analyzer)
	     (cdr (setq ret (read (ccl-execute-on-string
				   std11-ccl-lexical-analyzer
				   (make-vector 9 0)
				   (if start (substring string start)
				     (or string "")))))))
	(car ret)
      (setq len (length string)
	    analyzer (or analyzer std11-lexical-analyzer)
	    start (or start 0))
      (while (< start len)
	(setq ret
	      (let ((rest analyzer)
		    func r)
		(while (and (setq func (car rest))
			    (null (setq r (funcall func string start))))
		  (setq rest (cdr rest)))
		(or r
		    (cons (cons 'error (substring string start)) (1+ len)))))
	(setq dest (cons (car ret) dest)
	      start (cdr ret)))
      (nreverse dest))))


;;; @ parser
;;;

(defun std11-ignored-token-p (token)
  (memq (car token) '(spaces comment)))

(defun std11-parse-token (lal)
  (let (token itl)
    (while (and lal
		(std11-ignored-token-p (setq token (car lal))))
      (setq lal (cdr lal))
      (setq itl (cons token itl)))
    (cons (nreverse (cons token itl))
	  (cdr lal))))

(defun std11-parse-ascii-token (lal)
  (let (token itl parsed token-value)
    (while (and lal
		(setq token (car lal))
		(or (std11-ignored-token-p token)
		    (if (and (setq token-value (cdr token))
			     (delq 'ascii (find-charset-string token-value)))
			(setq token nil))))
      (setq lal (cdr lal))
      (setq itl (cons token itl)))
    (if (and token
	     (setq parsed (nreverse (cons token itl))))
	(cons parsed (cdr lal)))))

(defun std11-parse-token-or-comment (lal)
  (let (token itl)
    (while (and lal
		(eq (car (setq token (car lal))) 'spaces))
      (setq lal (cdr lal))
      (setq itl (cons token itl)))
    (cons (nreverse (cons token itl))
	  (cdr lal))))

(defun std11-parse-word (lal)
  (let ((ret (std11-parse-ascii-token lal)))
    (if ret
	(let ((elt (car ret))
	      (rest (cdr ret)))
	  (if (or (assq 'atom elt)
		  (assq 'quoted-string elt))
	      (cons (cons 'word elt) rest))))))

(defun std11-parse-word-or-comment-or-period (lal)
  (let ((ret (std11-parse-token-or-comment lal)))
    (if ret
	(let ((elt (car ret))
	      (rest (cdr ret)))
	  (cond ((or (assq 'atom elt)
		     (assq 'quoted-string elt))
		 (cons (cons 'word elt) rest))
		((assq 'comment elt)
		 (cons (cons 'comment-word elt) rest))
		((string-equal (cdr (assq 'specials elt)) ".")
		 (cons (cons 'period elt) rest)))))))

(defun std11-parse-phrase (lal)
  (let (ret phrase)
    (while (setq ret (std11-parse-word-or-comment-or-period lal))
      (setq phrase (append phrase (cdr (car ret))))
      (setq lal (cdr ret)))
    (if phrase
	(cons (cons 'phrase phrase) lal))))

(defun std11-parse-local-part (lal)
  (let ((ret (std11-parse-word lal)))
    (if ret
	(let ((local-part (cdr (car ret))) dot)
	  (setq lal (cdr ret))
	  (while (and (setq ret (std11-parse-ascii-token lal))
		      (setq dot (car ret))
		      (string-equal (cdr (assq 'specials dot)) ".")
		      (setq ret (std11-parse-word (cdr ret)))
		      (setq local-part
			    (append local-part dot (cdr (car ret))))
		      (setq lal (cdr ret))))
	  (cons (cons 'local-part local-part) lal)))))

(defun std11-parse-sub-domain (lal)
  (let ((ret (std11-parse-ascii-token lal)))
    (if ret
	(let ((sub-domain (car ret)))
	  (if (or (assq 'atom sub-domain)
		  (assq 'domain-literal sub-domain))
	      (cons (cons 'sub-domain sub-domain)
		    (cdr ret)))))))

(defun std11-parse-domain (lal)
  (let ((ret (std11-parse-sub-domain lal)))
    (if ret
	(let ((domain (cdr (car ret))) dot)
	  (setq lal (cdr ret))
	  (while (and (setq ret (std11-parse-ascii-token lal))
		      (setq dot (car ret))
		      (string-equal (cdr (assq 'specials dot)) ".")
		      (setq ret (std11-parse-sub-domain (cdr ret)))
		      (setq domain
			    (append domain dot (cdr (car ret))))
		      (setq lal (cdr ret))))
	  (cons (cons 'domain domain) lal)))))

(defun std11-parse-at-domain (lal)
  (let ((ret (std11-parse-ascii-token lal)) at-sign)
    (if (and ret
	     (setq at-sign (car ret))
	     (string-equal (cdr (assq 'specials at-sign)) "@")
	     (setq ret (std11-parse-domain (cdr ret))))
	(cons (cons 'at-domain (append at-sign (cdr (car ret))))
	      (cdr ret)))))

(defun std11-parse-addr-spec (lal)
  (let ((ret (std11-parse-local-part lal))
	addr)
    (if (and ret
	     (prog1
		 (setq addr (cdr (car ret)))
	       (setq lal (cdr ret))
	       (and (setq ret (std11-parse-at-domain lal))
		    (setq addr (append addr (cdr (car ret))))
		    (setq lal (cdr ret)))))
	(cons (cons 'addr-spec addr) lal))))

(defun std11-parse-route (lal)
  (let ((ret (std11-parse-at-domain lal))
	route comma colon)
    (if (and ret
	     (progn
	       (setq route (cdr (car ret)))
	       (setq lal (cdr ret))
	       (while (and (setq ret (std11-parse-ascii-token lal))
			   (setq comma (car ret))
			   (string-equal (cdr (assq 'specials comma)) ",")
			   (setq ret (std11-parse-at-domain (cdr ret))))
		 (setq route (append route comma (cdr (car ret))))
		 (setq lal (cdr ret)))
	       (and (setq ret (std11-parse-ascii-token lal))
		    (setq colon (car ret))
		    (string-equal (cdr (assq 'specials colon)) ":")
		    (setq route (append route colon)))))
	(cons (cons 'route route)
	      (cdr ret)))))

(defun std11-parse-route-addr (lal)
  (let ((ret (std11-parse-ascii-token lal))
	< route addr-spec >)
    (if (and ret
	     (setq < (car ret))
	     (string-equal (cdr (assq 'specials <)) "<")
	     (setq lal (cdr ret))
	     (progn (and (setq ret (std11-parse-route lal))
			 (setq route (cdr (car ret)))
			 (setq lal (cdr ret)))
		    (setq ret (std11-parse-addr-spec lal)))
	     (setq addr-spec (cdr (car ret)))
	     (setq lal (cdr ret))
	     (setq ret (std11-parse-ascii-token lal))
	     (setq > (car ret))
	     (string-equal (cdr (assq 'specials >)) ">"))
	(cons (cons 'route-addr (append route addr-spec))
	      (cdr ret)))))

(defun std11-parse-phrase-route-addr (lal)
  (let ((ret (std11-parse-phrase lal)) phrase)
    (if ret
	(progn
	  (setq phrase (cdr (car ret)))
	  (setq lal (cdr ret))))
    (if (setq ret (std11-parse-route-addr lal))
	(cons (list 'phrase-route-addr
		    phrase
		    (cdr (car ret)))
	      (cdr ret)))))

(defun std11-parse-mailbox (lal)
  (let ((ret (or (std11-parse-phrase-route-addr lal)
		 (std11-parse-addr-spec lal)))
	mbox comment)
    (if (and ret
	     (prog1
		 (setq mbox (car ret))
	       (setq lal (cdr ret))
	       (if (and (setq ret (std11-parse-token-or-comment lal))
			(setq comment (cdr (assq 'comment (car ret)))))
		   (setq lal (cdr ret)))))
	(cons (list 'mailbox mbox comment)
	      lal))))

(defun std11-parse-group (lal)
  (let ((ret (std11-parse-phrase lal))
	phrase colon comma mbox semicolon)
    (if (and ret
	     (setq phrase (cdr (car ret)))
	     (setq lal (cdr ret))
	     (setq ret (std11-parse-ascii-token lal))
	     (setq colon (car ret))
	     (string-equal (cdr (assq 'specials colon)) ":")
	     (setq lal (cdr ret))
	     (progn
	       (and (setq ret (std11-parse-mailbox lal))
		    (setq mbox (list (car ret)))
		    (setq lal (cdr ret))
		    (while (and (setq ret (std11-parse-ascii-token lal))
				(setq comma (car ret))
				(string-equal
				 (cdr (assq 'specials comma)) ",")
				(setq lal (cdr ret))
				(setq ret (std11-parse-mailbox lal))
				(setq mbox (cons (car ret) mbox))
				(setq lal (cdr ret)))))
	       (and (setq ret (std11-parse-ascii-token lal))
		    (setq semicolon (car ret))
		    (string-equal (cdr (assq 'specials semicolon)) ";")
		    )))
	(cons (list 'group phrase (nreverse mbox))
	      (cdr ret)))))

(defun std11-parse-address (lal)
  (or (std11-parse-group lal)
      (std11-parse-mailbox lal)))

(defun std11-parse-addresses (lal)
  (let ((ret (std11-parse-address lal)))
    (if ret
	(let ((dest (list (car ret))))
	  (setq lal (cdr ret))
	  (while (and (setq ret (std11-parse-ascii-token lal))
		      (string-equal (cdr (assq 'specials (car ret))) ",")
		      (setq ret (std11-parse-address (cdr ret))))
	    (setq dest (cons (car ret) dest))
	    (setq lal (cdr ret)))
	  (nreverse dest)))))

(defun std11-parse-msg-id (lal)
  (let ((ret (std11-parse-ascii-token lal))
	< addr-spec >)
    (if (and ret
	     (setq < (car ret))
	     (string-equal (cdr (assq 'specials <)) "<")
	     (setq lal (cdr ret))
	     (setq ret (std11-parse-addr-spec lal))
	     (setq addr-spec (car ret))
	     (setq lal (cdr ret))
	     (setq ret (std11-parse-ascii-token lal))
	     (setq > (car ret))
	     (string-equal (cdr (assq 'specials >)) ">"))
	(cons (cons 'msg-id (cdr addr-spec))
	      (cdr ret)))))

(defun std11-parse-msg-ids (tokens)
  "Parse lexical TOKENS as `*(phrase / msg-id)', and return the result."
  (let ((ret (or (std11-parse-msg-id tokens)
		 (std11-parse-phrase tokens))))
    (if ret
	(let ((dest (list (car ret))))
	  (setq tokens (cdr ret))
	  (while (setq ret (or (std11-parse-msg-id tokens)
			       (std11-parse-phrase tokens)))
	    (setq dest (cons (car ret) dest))
	    (setq tokens (cdr ret)))
	  (nreverse dest)))))

(defalias 'std11-parse-in-reply-to 'std11-parse-msg-ids)
(make-obsolete 'std11-parse-in-reply-to 'std11-parse-msg-ids "23 Jan 1999")


;;; @ composer
;;;

(defun std11-addr-to-string (seq)
  "Return string from lexical analyzed list SEQ
represents addr-spec of RFC 822."
  (mapconcat (lambda (token)
	       (let ((name (car token)))
                 (cond
                  ((memq name '(spaces comment)) nil)
                  ((eq name 'quoted-string)
                   (concat "\"" (cdr token) "\""))
                  ((eq name 'domain-literal)
                   (concat "[" (cdr token) "]"))
                  (t (cdr token)))))
	     seq nil))

;;;###autoload
(defun std11-address-string (address)
  "Return string of address part from parsed ADDRESS of RFC 822."
  (cond ((eq (car address) 'group)
	 (mapconcat (function std11-address-string)
		    (nth 2 address)
		    ", "))
	((eq (car address) 'mailbox)
	 (let ((addr (nth 1 address)))
	   (std11-addr-to-string
	    (if (eq (car addr) 'phrase-route-addr)
		(nth 2 addr)
	      (cdr addr)))))))

(defun std11-comment-value-to-string (value)
  (if (stringp value)
      (std11-strip-quoted-pair value)
    (let (dest)
      (while value
	(setq dest
	      (if (stringp (car value))
		  (cons (car value) dest)
		(cons ")"
		      (cons (std11-comment-value-to-string
			     (cdr (car value)))
			    (cons "(" dest))))
	      value (cdr value)))
      (apply 'concat (nreverse dest)))))

;;;###autoload
(defun std11-full-name-string (address)
  "Return string of full-name part from parsed ADDRESS of RFC 822."
  (cond ((eq (car address) 'group)
	 (mapconcat 'cdr (nth 1 address) ""))
	((eq (car address) 'mailbox)
	 (let ((addr (nth 1 address))
	       (comment (nth 2 address))
	       phrase)
	   (if (eq (car addr) 'phrase-route-addr)
	       (setq phrase
		     (mapconcat
		      (lambda (token)
			(let ((type (car token)))
			  (cond ((eq type 'quoted-string)
				 (std11-strip-quoted-pair (cdr token)))
				((eq type 'comment)
				 (concat "("
					 (std11-comment-value-to-string
					  (cdr token))
					 ")"))
				(t
				 (cdr token)))))
		      (nth 1 addr) "")))
	   (cond ((> (length phrase) 0) phrase)
		 (comment (std11-comment-value-to-string comment)))))))

;;;###autoload
(defun std11-msg-id-string (msg-id)
  "Return string from parsed MSG-ID of RFC 822."
  (concat "<" (std11-addr-to-string (cdr msg-id)) ">"))

;;;###autoload
(defun std11-fill-msg-id-list-string (string &optional column)
  "Fill list of msg-id in STRING, and return the result."
  (or column
      (setq column 12))
  (let ((lal (std11-lexical-analyze string))
	dest)
    (let ((ret (std11-parse-msg-id lal)))
      (if ret
	  (let* ((str (std11-msg-id-string (car ret)))
		 (len (length str)))
	    (setq lal (cdr ret))
	    (if (> (+ len column) 76)
		(setq dest (cons str (cons "\n " dest))
		      column (1+ len))
	      (setq dest str
		    column (+ column len))))
	(setq dest (cons (cdr (car lal)) dest)
	      lal (cdr lal))))
    (while lal
      (let ((ret (std11-parse-msg-id lal)))
	(if ret
	    (let* ((str (std11-msg-id-string (car ret)))
		   (len (1+ (length str))))
	      (setq lal (cdr ret))
	      (if (> (+ len column) 76)
		  (setq dest (cons str (cons "\n " dest))
			column len)
		(setq dest (cons str (cons " " dest))
		      column (+ column len))))
	  (setq dest (cons (cdr (car lal)) dest)
		lal (cdr lal)))))
    (apply 'concat (nreverse dest))))


;;; @ parser with lexical analyzer
;;;

;;;###autoload
(defun std11-parse-address-string (string)
  "Parse STRING as mail address."
  (std11-parse-address (std11-lexical-analyze string)))

;;;###autoload
(defun std11-parse-addresses-string (string)
  "Parse STRING as mail address list."
  (std11-parse-addresses (std11-lexical-analyze string)))

;;;###autoload
(defun std11-parse-msg-id-string (string)
  "Parse STRING as msg-id."
  (std11-parse-msg-id (std11-lexical-analyze string)))

;;;###autoload
(defun std11-parse-msg-ids-string (string)
  "Parse STRING as `*(phrase / msg-id)'."
  (std11-parse-msg-ids (std11-lexical-analyze string)))

;;;###autoload
(defun std11-extract-address-components (string)
  "Extract full name and canonical address from STRING.
Returns a list of the form (FULL-NAME CANONICAL-ADDRESS).
If no name can be extracted, FULL-NAME will be nil."
  (let* ((structure (car (std11-parse-address-string
			  (std11-unfold-string string))))
         (phrase  (std11-full-name-string structure))
         (address (std11-address-string structure)))
    (list phrase address)))


;;; @ end
;;;

(provide 'std11)

;;; std11.el ends here
