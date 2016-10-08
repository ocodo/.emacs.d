;;; easy-kill-extras.el --- Extra functions for easy-kill.

;; Copyright (c) 2014-2015 Akinori MUSHA
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
;; Created: 29 Jul 2014
;; Version: 0.9.4
;; Package-Requires: ((easy-kill "0.9.4"))
;; Keywords: killing, convenience

;;; Commentary:
;;
;; This package contains extra functions for easy-kill/easy-mark:
;;
;; * easy-mark-word
;; * easy-mark-sexp
;; * easy-mark-to-char
;; * easy-mark-up-to-char
;;
;; It also provides the following easy-kill/easy-mark targets:
;;
;; * `buffer'
;;
;;   This selects the whole buffer.
;;
;; * `buffer-before-point'
;; * `buffer-after-point'
;;
;;   These work like vi's gg/G commands, respectively.
;;
;; * `backward-line-edge'
;; * `forward-line-edge'
;;
;;   The former is like vi's ^/0 commands, and the latter is just like
;;   that in the opposite direction.
;;
;; * `string-to-char-forward'
;; * `string-to-char-backward'
;; * `string-up-to-char-forward'
;; * `string-up-to-char-backward'
;;
;;   These work like vi's f/F/t/T commands, respectively.
;;
;; Experimental ace-jump integration into easy-kill is enabled by
;; default.  `ace-jump-*-mode' can be invoked for selection when in
;; easy-kill/easy-mark mode.  You can disable this feature via a
;; customize variable `easy-kill-ace-jump-enable-p'.
;;
;; Suggested settings are as follows:
;;
;;   ;; Upgrade `mark-word' and `mark-sexp' with easy-mark
;;   ;; equivalents.
;;   (global-set-key (kbd "M-@") 'easy-mark-word)
;;   (global-set-key (kbd "C-M-@") 'easy-mark-sexp)
;;
;;   ;; `easy-mark-to-char' or `easy-mark-up-to-char' could be a good
;;   ;; replacement for `zap-to-char'.
;;   (global-set-key [remap zap-to-char] 'easy-mark-to-char)
;;
;;   ;; Add the following tuples to `easy-kill-alist', preferrably by
;;   ;; using `customize-variable'.
;;   (add-to-list 'easy-kill-alist '(?^ backward-line-edge ""))
;;   (add-to-list 'easy-kill-alist '(?$ forward-line-edge ""))
;;   (add-to-list 'easy-kill-alist '(?b buffer ""))
;;   (add-to-list 'easy-kill-alist '(?< buffer-before-point ""))
;;   (add-to-list 'easy-kill-alist '(?> buffer-after-point ""))
;;   (add-to-list 'easy-kill-alist '(?f string-to-char-forward ""))
;;   (add-to-list 'easy-kill-alist '(?F string-up-to-char-forward ""))
;;   (add-to-list 'easy-kill-alist '(?t string-to-char-backward ""))
;;   (add-to-list 'easy-kill-alist '(?T string-up-to-char-backward ""))

;;; Code:

(require 'easy-kill)

;;;###autoload
(defgroup easy-kill-extras nil
  "Extras for easy-kill."
  :group 'killing) ;; No 'easy-kill yet

(require 'easy-kill-to-char)
(require 'easy-kill-buffer)
(require 'easy-kill-line-edge)

;;;###autoload
(defadvice easy-mark
    (around per-thing activate)
  "Enable `easy-mark-word' and `easy-mark-sexp'."
  (let ((easy-mark-try-things
         (pcase this-command
           (`easy-mark-word
            (if (bound-and-true-p subword-mode)
                '(subword) '(word)))
           (`easy-mark-sexp
            '(sexp))
           (`easy-mark-to-char
            '(string-to-char-forward))
           (`easy-mark-up-to-char
            '(string-up-to-char-forward))
           (_
            easy-mark-try-things))))
    ad-do-it))

;;;###autoload
(defun easy-mark-word (n)
  "Start easy-mark with a word selected."
  (interactive "p")
  (easy-mark n))

;;;###autoload
(defun easy-mark-sexp (n)
  "Start easy-mark with a sexp selected."
  (interactive "p")
  (easy-mark n))

;;;###autoload
(eval-after-load 'ace-jump-mode
  #'(require 'easy-kill-aj))

(provide 'easy-kill-extras)
;;; easy-kill-extras.el ends here
