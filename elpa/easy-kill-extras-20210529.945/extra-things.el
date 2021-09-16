;;; extra-things.el --- defines various extra "things".

;; Copyright (c) 2021 Akinori MUSHA
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;; Author: Akinori MUSHA <knu@iDaemons.org>
;; URL: https://github.com/knu/easy-kill-extras.el
;; Created: 22 May 2021
;; Package-Requires: ((easy-kill "0.9.4"))
;; Keywords: killing, convenience

;;; Commentary:
;;
;; This library defines "things" that can be used with easy-kill.
;;
;; * `WORD': a sequence of non-whitespace characters
;;
;;   This is much like Vim's WORD object.
;;
;; * `squoted-string': a sindle-quoted string ('...')
;; * `dquoted-string': a double-quoted string ("...")
;; * `bquoted-string': a back-quoted string (`...`)
;; * `quoted-string': any of the above quoted strings
;;
;;   The backslash character serves as escape character.  For
;;   performance reasons, it is assumed that the beginning of the
;;   current line is not inside of a quoted string.  In other words,
;;   multi-line quoted strings are not fully supported.
;;
;;   These things are aware of the current syntax table, and the
;;   quotation marks that are a word constituent or an expression
;;   prefix in the current mode are ignored.  For example,
;;   `squoted-string' would only work in some specific programming
;;   language modes where the single quotation mark is a quotation
;;   character.
;;
;; * `squoted-string-universal'
;; * `dquoted-string-universal'
;; * `bquoted-string-universal'
;; * `quoted-string-universal'
;;
;;   These versions recognize all quotation pairs ignoring the current
;;   syntax table and support nesting of different quotations.
;;
;; * `parentheses-pair': the block between a parentheses pair including the opening and closing parentheses
;; * `brackets-pair': the block between a brackets pair including the opening and closing brackets
;; * `curlies-pair': the block between a curlies pair including the opening and closing curlies
;; * `angles-pair': the block between an angles pair including the opening and closing angles
;; * `parentheses-pair-content': the content inside of a parentheses pair without whitespace at both ends.
;; * `brackets-pair-content': the content inside of a brackets pair without whitespace at both ends.
;; * `curlies-pair-content': the content inside of a curlies pair without whitespace at both ends.
;; * `angles-pair-content': the content inside of an angles pair without whitespace at both ends.
;;
;;   Quotation marks or different types of pair characters are not
;;   taken into account.  Each type of things only cares about the
;;   nest level of their pair characters.

;;; Code:

(require 'cl-lib)
(require 'rx)
(eval-when-compile
  (require 'cl-lib)
  (require 'rx)
  (require 'easy-kill))

;;
;; WORD
;;

(defun end-op-WORD ()
  (save-match-data
    (re-search-forward "\\=[^[:space:]]*" nil t)))

(defun beginning-op-WORD ()
  (save-match-data
    (if (re-search-backward "[[:space:]]" nil t)
        (forward-char)
      (goto-char (point-min)))))

(put 'WORD 'end-op 'end-op-WORD)
(put 'WORD 'beginning-op 'beginning-op-WORD)

;;
;; quoted strings
;;

(defvar-local extra-things--quote-chars nil)
(defvar-local extra-things--quoted-string-literal-regexp nil)
(defvar-local extra-things--nonquoted-string-regexp nil)

(add-hook 'change-major-mode-hook
          '(lambda () (setq extra-things--quote-chars nil
                            extra-things--quoted-string-literal-regexp nil
                            extra-things--nonquoted-string-regexp nil)))

(defvar extra-things--all-quote-chars '(?\' ?\" ?\`))

(defun extra-things--get-quote-chars ()
  (or extra-things--quote-chars
      (setq-local
       extra-things--quote-chars
       (cl-remove-if '(lambda (c) (member (char-syntax c) '(?w ?\' ?\`))) extra-things--all-quote-chars))))

(defmacro extra-things--generate-quoted-string-literal-regexp (quote-chars)
  `(rx-to-string
    `(| ,@(mapcar (lambda (c)
                    `(: point
                        (group-n 1 ,(char-to-string c))
                        (* (| (+ (not (any ,c ?\\)))
                              (: "\\" anychar)))
                        (| ,(char-to-string c)
                           eos)))
                  ,quote-chars))))

(defun extra-things--get-quoted-string-literal-regexp (&optional all)
  (if all
      (extra-things--generate-quoted-string-literal-regexp extra-things--all-quote-chars)
    (or extra-things--quoted-string-literal-regexp
        (setq-local
         extra-things--quoted-string-literal-regexp
         (extra-things--generate-quoted-string-literal-regexp (extra-things--get-quote-chars))))))

(defmacro extra-things--generate-nonquoted-string-regexp (quote-chars)
  `(rx-to-string
    `(: point
        (+ (| (+ (not (any ?\\ ,@,quote-chars)))
              (: "\\" anychar))))))

(defun extra-things--get-nonquoted-string-regexp (&optional all)
  (if all
      (extra-things--generate-nonquoted-string-regexp extra-things--all-quote-chars)
    (or extra-things--nonquoted-string-regexp
        (setq-local
         extra-things--nonquoted-string-regexp
         (extra-things--generate-nonquoted-string-regexp (extra-things--get-quote-chars))))))

(defmacro define-quoted-string-thing (thing quote)
  "Define THING as a string quoted with QUOTE."
  (let* ((end-op (intern (format "end-of-%s" thing)))
         (beginning-op (intern (format "beginning-of-%s" thing)))
         (thing-universal (intern (format "%s-universal" thing)))
         (end-op-universal (intern (format "end-of-%s" thing-universal)))
         (beginning-op-universal (intern (format "beginning-of-%s" thing-universal))))
    `(progn
       (defun ,end-op (&optional arg)
         (interactive "P")
         (let ((start (point))
               (bound (line-end-position))
               (re-nonquoted (extra-things--get-nonquoted-string-regexp arg))
               (re-quoted (extra-things--get-quoted-string-literal-regexp arg)))
           (beginning-of-line)
           (save-match-data
             (cl-loop do
                      (cond ((re-search-forward re-nonquoted bound t)
                             (when (< start (point))
                               (goto-char start)
                               (return nil)))
                            ((re-search-forward re-quoted nil t)
                             ,(if quote
                                  `(if (string= (match-string-no-properties 1) ,quote)
                                       (when (<= start (point))
                                         (return nil))
                                     (when (< start (point))
                                       (goto-char start)
                                       (return nil)))
                                `(when (<= start (point))
                                   (if arg
                                       (let ((qbeg (match-beginning 0))
                                             (qend (point)))
                                         (goto-char start)
                                         (or (save-restriction
                                               (narrow-to-region (1+ qbeg) (1- qend))
                                               (,end-op arg))
                                             (goto-char qend))))
                                   (return t))))
                            (t
                             (error "BUG")))))))

       (defun ,beginning-op (&optional arg)
         (interactive "P")
         (let ((start (point))
               (bound (line-end-position))
               (re-nonquoted (extra-things--get-nonquoted-string-regexp arg))
               (re-quoted (extra-things--get-quoted-string-literal-regexp arg)))
           (beginning-of-line)
           (save-match-data
             (cl-loop do
                      (cond ((re-search-forward re-nonquoted bound t)
                             (when (< start (point))
                               (goto-char start)
                               (return nil)))
                            ((re-search-forward re-quoted nil t)
                             ,(if quote
                                  `(if (string= (match-string-no-properties 1) ,quote)
                                       (when (<= start (point))
                                         (goto-char (match-beginning 0))
                                         (return t))
                                     (when (< start (point))
                                       (goto-char start)
                                       (return nil)))
                                `(cond ((= start (point))
                                        (goto-char (match-beginning 0))
                                        (return t))
                                       ((< start (point))
                                        (if arg
                                            (let ((qbeg (match-beginning 0))
                                                  (qend (point)))
                                              (goto-char start)
                                              (or (save-restriction
                                                    (narrow-to-region (1+ qbeg) (1- qend))
                                                    (,beginning-op arg))
                                                  (goto-char qbeg)))
                                          (goto-char (match-beginning 0)))
                                        (return t)))))
                            (t
                             (error "BUG")))))))

       (put ',thing 'end-op ',end-op)
       (put ',thing 'beginning-op ',beginning-op)

       (defun ,end-op-universal ()
         (interactive)
         (,end-op t))
       (defun ,beginning-op-universal ()
         (interactive)
         (,beginning-op t))

       (put ',thing-universal 'end-op ',end-op-universal)
       (put ',thing-universal 'beginning-op ',beginning-op-universal))))

(define-quoted-string-thing squoted-string "'")
(define-quoted-string-thing dquoted-string "\"")
(define-quoted-string-thing bquoted-string "`")
(define-quoted-string-thing quoted-string nil)

;;
;; pairs
;;

(defvar pair-thing-limit 10000
  "How far pair things functions should go look for matching characters.")

(defmacro define-pair-thing (name opening closing)
  "Define a `thing' as the content inside of a pair of OPENING and CLOSING without whitespace at both ends.  NAME is the name of the pair in the plural form."
  (let* ((pair-thing (intern (format "%s-pair" name)))
         (forward-pair-internal (intern (format "forward-%s--internal" pair-thing)))
         (backward-pair-internal (intern (format "backward-%s--internal" pair-thing)))
         (end-of-pair (intern (format "end-of-%s" pair-thing)))
         (beginning-of-pair (intern (format "beginning-of-%s" pair-thing)))
         (forward-pair (intern (format "forward-%s" pair-thing)))
         (backward-pair (intern (format "backward-%s" pair-thing)))
         (re-at-end-of-pair (concat (regexp-quote closing) "\\="))
         (re-at-beginning-of-pair (concat "\\=" (regexp-quote opening)))
         (pair-content-thing (intern (format "%s-pair-content" name)))
         (forward-pair-content-internal (intern (format "forward-%s--internal" pair-content-thing)))
         (backward-pair-content-internal (intern (format "backward-%s--internal" pair-content-thing)))
         (end-of-pair-content (intern (format "end-of-%s" pair-content-thing)))
         (beginning-of-pair-content (intern (format "beginning-of-%s" pair-content-thing)))
         (forward-pair-content (intern (format "forward-%s" pair-content-thing)))
         (backward-pair-content (intern (format "backward-%s" pair-content-thing)))
         (re-at-end-of-pair-content (concat "\\=[[:space:]]*" (regexp-quote closing)))
         (re-at-beginning-of-pair-content (concat (regexp-quote opening) "[[:space:]]*\\="))
         (re-either (regexp-opt (list opening closing))))
    `(progn
       (defun ,forward-pair-internal (&optional bound)
         (save-match-data
           (let ((start (point))
                 (level 1))
             (re-search-forward ,re-at-beginning-of-pair nil t)
             (cl-loop while (re-search-forward ,re-either bound t)
                      do
                      (if (string= (match-string-no-properties 0) ,opening)
                          (setq level (1+ level))
                        (setq level (1- level))
                        (if (zerop level)
                            (return))))
             (or (zerop level)
                 (progn
                   (goto-char start)
                   nil)))))

       (defun ,backward-pair-internal (&optional bound)
         (save-match-data
           (let ((start (point))
                 (level 1))
             (re-search-backward ,re-at-end-of-pair nil t)
             (cl-loop while (re-search-backward ,re-either bound t)
                      do
                      (if (string= (match-string-no-properties 0) ,closing)
                          (setq level (1+ level))
                        (setq level (1- level))
                        (if (zerop level)
                            (return))))
             (or (zerop level)
                 (progn
                   (goto-char start)
                   nil)))))

       (defun ,beginning-of-pair ()
         (,backward-pair-internal (max (- (point) pair-thing-limit) (point-min))))

       (defun ,end-of-pair ()
         (,forward-pair-internal (min (+ (point) pair-thing-limit) (point-max))))

       (defun ,forward-pair (arg)
         ,(format "Move to the end of the current %1$s pair.\nWith ARG, move to the end of the n-th outer %1$s pair.  Negative -N means `(%2$s N)'." name backward-pair)
         (interactive "p")
         (if (natnump arg)
             (let ((bound (min (+ (point) pair-thing-limit) (point-max))))
               (cl-loop repeat arg
                        while (,forward-pair-internal bound)))
           (,backward-pair (- arg))))

       (defun ,backward-pair (arg)
         ,(format "Move to the beginning of the current %1$s pair.\nWith ARG, move to the beginning of the n-th outer %1$s pair.  Negative -N means `(%2$s N)'." name forward-pair)
         (interactive "p")
         (if (natnump arg)
             (let ((bound (max (- (point) pair-thing-limit) (point-min))))
               (cl-loop repeat (- arg)
                        while (,backward-pair-internal bound)))
           (,forward-pair (- arg))))

       (put ',pair-thing 'beginning-op ',beginning-of-pair)
       (put ',pair-thing 'end-op ',end-of-pair)
       (put ',pair-thing 'forward-op ',forward-pair)

       (eval-after-load 'easy-kill
         '(progn
            (defun ,(intern (format "easy-kill-on-%s" pair-thing)) (n)
              (let* ((origin (easy-kill-get origin))
                     (n (if (eq n 1)
                            (if (eq (easy-kill-get thing) ',pair-thing) '+ 1)
                          n))
                     (end
                      (save-excursion
                        (pcase n
                          (`+ (goto-char (easy-kill-get end))
                              (and (,end-of-pair)
                                   (point)))
                          (`- (let ((current (easy-kill-get end))
                                    prev)
                                (goto-char origin)
                                (cl-loop while (and (,end-of-pair)
                                                    (< (point) current))
                                         do (setq prev (point)))
                                (and (<= current (point))
                                     prev)))
                          (_ (goto-char (or (and (eq (easy-kill-get thing) ',pair-thing)
                                                 (easy-kill-get end))
                                            origin))
                             (,forward-pair n)
                             (unless (= (point) origin)
                               (point))))))
                     (beg
                      (and end
                           (save-excursion
                             (goto-char end)
                             (,beginning-of-pair)
                             (point)))))
                (if beg
                    (easy-kill-adjust-candidate ',pair-thing beg end))))))

       (defun ,forward-pair-content-internal (&optional bound)
         (save-match-data
           (let ((start (point))
                 (level 1))
             (re-search-forward ,re-at-end-of-pair-content nil t)
             (cl-loop while (re-search-forward ,re-either bound t)
                      do
                      (if (string= (match-string-no-properties 0) ,opening)
                          (setq level (1+ level))
                        (setq level (1- level))
                        (if (zerop level)
                            (return))))
             (if (< 0 level)
                 (progn
                   (goto-char start)
                   nil)
               (backward-char)
               (if (re-search-backward "[^[:space:]]" (line-beginning-position) t)
                   (forward-char)
                 (beginning-of-line))
               t))))

       (defun ,backward-pair-content-internal (&optional bound)
         (save-match-data
           (let ((start (point))
                 (level 1))
             (re-search-backward ,re-at-beginning-of-pair-content nil t)
             (cl-loop while (re-search-backward ,re-either bound t)
                      do
                      (if (string= (match-string-no-properties 0) ,closing)
                          (setq level (1+ level))
                        (setq level (1- level))
                        (if (zerop level)
                            (return))))
             (if (< 0 level)
                 (progn
                   (goto-char start)
                   nil)
               (forward-char)
               (if (re-search-forward "[^[:space:]]" (line-end-position) t)
                   (backward-char)
                 (forward-line))
               t))))

       (defun ,beginning-of-pair-content ()
         (,backward-pair-content-internal (max (- (point) pair-thing-limit) (point-min))))

       (defun ,end-of-pair-content ()
         (,forward-pair-content-internal (min (+ (point) pair-thing-limit) (point-max))))

       (defun ,forward-pair-content (arg)
         ,(format "Move to the end of the current %1$s pair content.\nWith ARG, move to the end of the n-th outer %1$s pair content.  Negative -N means `(%2$s N)'." name backward-pair-content)
         (interactive "p")
         (if (natnump arg)
             (let ((bound (min (+ (point) pair-thing-limit) (point-max))))
               (cl-loop repeat arg
                        while (,forward-pair-content-internal bound)))
           (,backward-pair-content (- arg))))

       (defun ,backward-pair-content (arg)
         ,(format "Move to the beginning of the current %1$s pair content.\nWith ARG, move to the beginning of the n-th outer %1$s pair content.  Negative -N means `(%2$s N)'." name forward-pair-content)
         (interactive "p")
         (if (natnump arg)
             (let ((bound (max (- (point) pair-thing-limit) (point-min))))
               (cl-loop repeat (- arg)
                        while (,backward-pair-content-internal bound)))
           (,forward-pair-content (- arg))))

       (put ',pair-content-thing 'beginning-op ',beginning-of-pair-content)
       (put ',pair-content-thing 'end-op ',end-of-pair-content)
       (put ',pair-content-thing 'forward-op ',forward-pair-content)

       (eval-after-load 'easy-kill
         '(progn
            (defun ,(intern (format "easy-kill-on-%s" pair-content-thing)) (n)
              (let* ((origin (easy-kill-get origin))
                     (n (if (eq n 1)
                            (if (eq (easy-kill-get thing) ',pair-content-thing) '+ 1)
                          n))
                     (end
                      (save-excursion
                        (pcase n
                          (`+ (goto-char (easy-kill-get end))
                              (and (,end-of-pair-content)
                                   (point)))
                          (`- (let ((current (easy-kill-get end))
                                    prev)
                                (goto-char origin)
                                (cl-loop while (and (,end-of-pair-content)
                                                    (< (point) current))
                                         do (setq prev (point)))
                                (and (<= current (point))
                                     prev)))
                          (_ (goto-char (or (and (eq (easy-kill-get thing) ',pair-content-thing)
                                                 (easy-kill-get end))
                                            origin))
                             (,forward-pair-content n)
                             (unless (= (point) origin)
                               (point))))))
                     (beg
                      (and end
                           (save-excursion
                             (goto-char end)
                             (,beginning-of-pair-content)
                             (point)))))
                (if beg
                    (easy-kill-adjust-candidate ',pair-content-thing beg end)))))))))

(define-pair-thing parentheses "(" ")")
(define-pair-thing brackets "[" "]")
(define-pair-thing curlies "{" "}")
(define-pair-thing angles "<" ">")

(provide 'extra-things)
;;; extra-things.el ends here
