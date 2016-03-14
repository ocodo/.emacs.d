;;; sed-mode.el --- Major mode to edit sed scripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 1.0
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; If you need this major mode, you might also want to
;; consider spending some time with `M-x doctor'.

;;; Code:

(require 'cl-lib)
(require 'smie)

(defgroup sed-mode nil
  "Major mode to edit sed code."
  :group 'programming)


(defvar sed-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\\ "." st)
    st))

(defconst sed-commands ":=aiqQrRbcdDhHgGlnNpPstTwWxy")

(eval-and-compile
  (defconst sed-command-prefix-regexp "\\(?:^\\|[$/0-9;]\\)[ \t]*")
  (defconst sed-address-prefix-regexp "\\(?:^\\|[,;]\\)[ \t]*"))

(defconst sed-label-regexp "[[:alnum:]]+")

(defun sed-syntax-propertize (beg end)
  (goto-char beg)
  (sed-syntax-propertize-string end)
  (funcall
   (syntax-propertize-rules
    ("\\\\$"
     (0 (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
          (put-text-property (match-beginning 0) (match-end 0)
                             'syntax-table (string-to-syntax "|"))
          (sed-syntax-propertize-string end)
          nil)))
    ((concat "\\(?:" sed-address-prefix-regexp
             "\\(?:\\(?1:/\\)\\|\\\\\\(?1:.\\)\\)"
             "\\|" sed-command-prefix-regexp "[sy]\\(?1:.\\)"
             "\\)")
     (0 (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
          (put-text-property (match-beginning 1) (match-end 1)
                             'syntax-table (string-to-syntax "\""))
          (sed-syntax-propertize-string end)
          nil))))
   (point) end))

(defun sed-syntax-propertize-string (end)
  (let* ((ppss (syntax-ppss))
         (c (nth 3 ppss)))
    (when c
      (let ((count (cond
                    ((or (eq c t)
                         (not (memq (char-before (nth 8 ppss)) '(?s ?y))))
                     1)
                    (t 2))))
        (goto-char (1+ (nth 8 ppss)))
        (when (re-search-forward
               (if (eq c t) "[^\\]\n" (regexp-quote (string c)))
               end 'move count)
          (put-text-property (1- (match-end 0)) (match-end 0)
                             'syntax-table
                             (if (eq c t) (string-to-syntax "|")
                               (string-to-syntax "\""))))))))

(defun sed--font-lock-command (cmd)
  (unless (nth 8 (syntax-ppss))
    (pcase cmd
      (?: (if (looking-at (concat "[ 	]*\\(" sed-label-regexp "\\)"))
              (put-text-property (match-beginning 1) (match-end 1) 'face
                                 font-lock-function-name-face)))
      ((or ?b ?t ?T)
       (if (looking-at (concat "[ 	]*\\(" sed-label-regexp "\\)"))
           (put-text-property (match-beginning 1) (match-end 1) 'face
                              font-lock-constant-face))))
    font-lock-keyword-face))

(defconst sed-font-lock-keywords
  `((,(concat sed-command-prefix-regexp "\\([" sed-commands "]\\)")
     (1 (sed--font-lock-command (char-after (match-beginning 1)))))))

(defconst sed-smie-grammar nil)

(defun sed-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:list-intro . ,_) t)
    ))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.sed\\'" . sed-mode))
;;;###autoload (add-to-list 'interpreter-mode-alist '("sed" . sed-mode))

;;;###autoload
(define-derived-mode sed-mode prog-mode "Sed"
  "Sed editing mode."
  ;; (setq-local font-lock-support-mode nil) ;; To help debugging.
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local parse-sexp-lookup-properties t)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local syntax-propertize-function #'sed-syntax-propertize)
  (setq-local font-lock-defaults '(sed-font-lock-keywords))
  (smie-setup sed-smie-grammar #'sed-smie-rules
              ;; :backward-token #'sm-c-smie-backward-token
              ;; :forward-token #'sm-c-smie-forward-token
              )
  ;; Backslash auto-realign.
  ;; (add-hook 'after-change-functions #'sm-c--bs-after-change nil t)
  ;; (add-hook 'post-command-hook #'sm-c--bs-realign nil t)
  ;; (setq-local add-log-current-defun-header-regexp sm-c--def-regexp)
  ;; (setq-local imenu-generic-expression `((nil ,sm-c--def-regexp 1)))
  )

;;;; ChangeLog:

;; 2016-02-26  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* sed-mode/sed-mode.el: Bump up version for first release
;; 
;; 	(interpreter-mode-alist): Register ourselves.
;; 
;; 2016-02-25  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* sed-mode: New package
;; 


(provide 'sed-mode)
;;; sed-mode.el ends here
