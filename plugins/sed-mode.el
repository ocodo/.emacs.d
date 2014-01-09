;;; sed-mode.el --- a major-mode for editing sed scripts in emacs
;;
;; Filename: sed-mode.el
;; Description: a major-mode for editing sed scripts in emacs
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; Keywords: languages
;; 
;; This file is not part of GNU Emacs.
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;;; Code:
;(require 'markup-faces)

;(require 'hi-lock-ext)

(defvar sed-mode-hook nil)

(defvar sed-mode-map nil "Keymap for sed-mode.")
(unless sed-mode-map
  (setq sed-mode-map (make-sparse-keymap)))
 
(defun sed-mode-regex-cust (arg)
  ;; not quite true because its possible that delimiter is part of regex
  (concat "\\\\\\(.\\)\\(?:[^\\\n]\\|\\\\.\\)*?\\" arg))

(defun sed-mode-1addr (arg)
  (concat "\\(?:"
          "/\\(?:[^\\/]\\|\\\\.\\)*/\\|" ; regex with / as delimiter
          (regex-cust-del arg)           ; regex with custom delimiter
          "[0-9]+\\|"                    ; n-th line
          "\\$\\|"                       ; last line
          "[0-9]+[ \t]*~[ \t]*[0-9]+\\|" ; every x-th line starting at y-th
          "\\)"))

(defun sed-mode-addr ()
  (let ((2nd-1addr (concat "\\(?:" (sed-mode-1addr 3) "\\|[+~][0-9]+\\)")))
  (concat "\\(?:"
          (sed-mode-1addr 2)                    ; 1addr form
          "\\(?:[ \t]*,[ \t]*" 2nd-1addr "\\)?" ; 2addr form
          "\\)")))

(defconst sed-mode-font-lock-keywords
  (let* ((fulladdr (concat "\\(?:[;{}]\\|^\\)[ \t]*\\(" (sed-mode-addr) "?\\)\\(\\(?:[ \t]*!\\)?\\)[ \t]*")))
         
    ;; todo:
    ;; - custom delimter for regex addr, s, y
    ;; - multiline schmarrn for a c e i
    ;; - be independent of hi-lock-ext
    ;; - use variables instead faces to customice which faces are used for which syntax elements
    ;;
         
    ;; does }, as ;, also delimit certain arguments?
    
  (list
   ;; ordered after occurence probability for a small performance improvement
   (list (concat fulladdr "\\([sy]\\)\\(/\\)\\(\\(?:[^\\/]\\|\\\\.\\)*\\)\\(/\\)\\(\\(?:[^\\/]\\|\\\\.\\)*\\)\\(/\\)\\(\\sw*\\)") '(1 font-lock-variable-name-face) '(2 font-lock-negation-char-face) '(3 font-lock-keyword-face) '(4 hi-unimportant) '(5 font-lock-constant-face) '(6 hi-unimportant) '(7 font-lock-constant-face) '(8 hi-unimportant))
   (list (concat fulladdr "\\([=dDgGhhHnNpPxz]\\)") '(1 font-lock-variable-name-face) '(2 font-lock-negation-char-face) '(3 font-lock-keyword-face))
   (list (concat fulladdr "\\([btT]\\)[ \t]*\\([^;\n]*\\)") '(1 font-lock-variable-name-face) '(2 font-lock-negation-char-face) '(3 font-lock-keyword-face) '(4 font-lock-preprocessor-face))
   (list (concat fulladdr "\\([lqQ]\\)[ \t]*\\([0-9]+\\)") '(1 font-lock-variable-name-face) '(2 font-lock-negation-char-face) '(3 font-lock-keyword-face) '(4 font-lock-constant-face))
   (list (concat fulladdr "\\(:[ \t]*[^;\n]*\\)") '(1 font-lock-variable-name-face) '(2 font-lock-negation-char-face) '(3 font-lock-preprocessor-face))
   (list (concat fulladdr "[{}]") '(1 font-lock-variable-name-face) '(2 font-lock-negation-char-face))
   (list (concat fulladdr "\\([acei]\\)[ \t]*\\(\\(?:[^\\\n]\\|\\\\\\(.\\|\n\\)\\)*\\)$") '(1 font-lock-variable-name-face) '(2 font-lock-negation-char-face) '(3 font-lock-keyword-face) '(4 font-lock-string-face))
   (list (concat fulladdr "\\([rRwW]\\)[ \t]*\\([^\n]+\\)") '(1 font-lock-variable-name-face) '(2 font-lock-negation-char-face) '(3 font-lock-keyword-face) '(4 font-lock-constant-face))
   
   ;; comment.  
   (list "\\(#\\)\\(.*\\)$" '(1 font-lock-comment-delimiter-face) '(2 font-lock-comment-face)) 
   )))

(defun sed-mode()
  (interactive)
  (kill-all-local-variables)
  
  (use-local-map sed-mode-map)  
        
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+[ \t]*")
  
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
﻿  '(sed-mode-font-lock-keywords t nil nil nil))
  
  (setq major-mode 'sed-mode mode-name "sed")

  (run-hooks 'sed-mode-hook))

(provide 'sed-mode)

;;; sed-mode.el ends here