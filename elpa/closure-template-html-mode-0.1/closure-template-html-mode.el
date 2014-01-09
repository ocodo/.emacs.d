;;; closure-template-html-mode.el --- highlighting for google closure templates
;;;
;;; This file is part of the cl-closure-template library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>
;;; Version: 0.1

(require 'apropos)
(require 'font-lock)

(require 'sgml-mode)

(defgroup closure-template-html nil
  "Customizations for `closure-template-html-mode'."
  :prefix "closure-template-html-"
  :group 'closure-template)


(defconst closure-template-html-font-lock-keywords-1
  (append sgml-font-lock-keywords-1)

  "First level keyword highlighting.")

(defconst closure-template-html-font-lock-keywords-2
  (append closure-template-html-font-lock-keywords-1
          sgml-font-lock-keywords-2))

(add-to-list 'auto-mode-alist '("\\.soy\\'" . closure-template-html-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; closure-template keywords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface closure-template-tag
  '((t (:inherit font-lock-type-face :bold t)))
  "`closure-template-html-mode' face used to highlight the tags."
  :group 'closure-template-html)

(defvar closure-template-tag-face 'closure-template-tag)


(defvar *closure-template-comment-keywords*
  `(("//.*" . font-lock-comment-face)))

(defvar *closure-template-namespace-keywords*
  `((,(rx "{"
          (group "namespace")
          (1+ space)
          (group (1+ (not space)))
          (0+ space)
          "}")
     (1 closure-template-tag-face)
     (2 font-lock-function-name-face))))

(defvar *closure-template-literal-keywords*
  `((,(rx (or "{literal}" "{/literal}"))
     (1 closure-template-tag-face)
     (2 font-lock-function-name-face))))

(defvar *closure-template-template-keywords*
  `((,(rx "{"
          (group (or "template" "call" "param"))
          (1+ space)
          (group (1+ (not space)))
          (0+ (: (1+ space) (1+ (not (any space "}")))))
          (0+ space)          
          "}")
     (1 closure-template-tag-face)
     (2 font-lock-function-name-face))
    (,(rx "{"
          (group (or "/template" "/call" "/param"))
          "}")
     (1 closure-template-tag-face))))


(defvar *closure-template-variable-keywords*
  `((,(rx "$"
          (group (1+ (or word "."))))
     (1 font-lock-variable-name-face))))
          

(defvar *closure-template-substition-keywords*
  `((,(rx (or "{sp}"
              "{nil}"
              "{\\r}"
              "{\\n}"
              "{\\t}"
              "{lb}"
              "{rb}"))
     . closure-template-tag-face)))

(defvar *closure-template-foreach-keywords*
  `((,(rx "{"
          (group "foreach")
          (1+ space)
          (1+ (not space))
          (1+ space)
          (group "in")
          (1+ space)
          (1+ (not space))
          (0+ space)
          "}")
     (1 closure-template-tag-face)
     (2 closure-template-tag-face))
    (,(rx "{"
         (group (or "/foreach" "ifempty"))
         "}")
     (1 closure-template-tag-face))))

(defvar *closure-template-if-switch-keywords*
  `((,(rx "{"
          (group (or "if" "elseif" "switch" "case"))
          (1+ space)
          (1+ (not (any "}")))
          (0+ space)
          "}")
     (1 closure-template-tag-face))
    (,(rx "{"
          (group (or "/if" "else" "/switch" "default"))
          "}")
     (1 closure-template-tag-face))))
     
(defun closure-template-html-font-lock-keywords-3 ()
  (append *closure-template-comment-keywords*
          *closure-template-namespace-keywords*
          *closure-template-literal-keywords*
          *closure-template-template-keywords*
          *closure-template-variable-keywords*
          *closure-template-substition-keywords*
          *closure-template-foreach-keywords*
          *closure-template-if-switch-keywords*

          closure-template-html-font-lock-keywords-1
          closure-template-html-font-lock-keywords-2))

(defvar closure-template-html-font-lock-keywords
  closure-template-html-font-lock-keywords-1)

(defvar closure-template-html-mode-syntax-table
  (let ((closure-template-html-mode-syntax-table (make-syntax-table)))
    closure-template-html-mode-syntax-table)
  "Syntax table for closure-template-html-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *closure-open* "^[ \t]*{[a-z]+")
(defvar *closure-close* "^[ \t]*{/[a-z]+")
(defvar *closure-short-com* "^[ \t]*//")
(defvar *closure-star-at-begining* "^[ \t]*\\*")
(defvar *closure-long-com* "^[ \t]*/\\*")
(defvar *closure-tag-opened* "^[^\n]*\\({[^\n^{/]+[^\n^{]*[^\n^{/]+}\\)[^\n^{]*$")
(defvar *closure-tag-closed* "^[^\n]*\\({/[^\n^{]+}\\|{[^\n^{]+/}\\)[^\n^{]*$")

(defun closure-short-com ()
  (save-excursion
    (beginning-of-line)
    (looking-at *closure-short-com*)))

(defun closure-any-com ()
  (save-excursion
    (beginning-of-line)
    (or (closure-short-com)
        (looking-at *closure-star-at-begining*)
	    (looking-at *closure-long-com*))))

(defun closure-open ()
  (save-excursion
    (beginning-of-line)
    (looking-at *closure-open*)))

(defun closure-close ()
  (save-excursion
    (beginning-of-line)
    (looking-at *closure-close*)))

(defun closure-tag-closed ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at *closure-tag-closed*)
        (let ((str (match-string 1)))
          (or (some (lambda (obj)
                      (string-match (caar obj)
                                    str))
                    (list *closure-template-template-keywords*
                          *closure-template-foreach-keywords*
                          *closure-template-if-switch-keywords*))
              (some (lambda (obj)
                      (string-match (caadr obj)
                                    str))
                    (list *closure-template-template-keywords*
                          *closure-template-foreach-keywords*
                          *closure-template-if-switch-keywords*)))))))

(defun closure-tag-opened ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at *closure-tag-opened*)
        (let ((str (match-string 1)))
          (some (lambda (obj)
                  (string-match (caar obj)
                                str))
                (list *closure-template-template-keywords*
                      *closure-template-foreach-keywords*
                      *closure-template-if-switch-keywords*))))))

(defun closure-previous-indent ()
  (save-excursion
    (while (and (zerop (forward-line -1))
                (or (looking-at "[ \t]*$")
                    (closure-any-com))))
    (back-to-indentation)
    (list (cond
           ((closure-tag-closed) 'closed)
           ((closure-tag-opened) 'opened)
           (t nil))
          (current-column))))

(defun indent-sgml-in-closure ()
  (let ((prev (closure-previous-indent)))
    (case (first prev)
      (opened (indent-line-to (+ (second prev)
                                 sgml-basic-offset)))
      (otherwise
       (let* ((savep (point))
              (indent-col (save-excursion
                                 (back-to-indentation)
                                 (if (>= (point) savep) (setq savep nil))
                                 (sgml-calculate-indent))))
         (cond
          ((eql (first prev) 'closed)
           (setf indent-col
                 (min indent-col (second prev))))
          ((= indent-col 0)
           (setf indent-col (second prev))))
         (if (null indent-col)
             'noindent
           (if savep
               (save-excursion (indent-line-to indent-col))
             (indent-line-to indent-col))))))))


(defun indent-closure-open ()
  (let ((ind (closure-previous-indent)))
    (case (car ind)
      (opened (indent-line-to (+ (second ind)
                                 sgml-basic-offset)))
      (closed (indent-line-to (second ind)))
      ((nil) (indent-sgml-in-closure)))))
       

(defun indent-closure-close ()
  (let ((prev (closure-previous-indent)))
    (case (first prev)
      (opened (indent-line-to (second prev)))
      (otherwise (indent-line-to (- (second prev) sgml-basic-offset))))))
          
(defun closure-indent-line ()
  (interactive)
  (cond
   ((closure-close) (indent-closure-close))
   ((closure-open) (indent-closure-open))
   ((closure-any-com) nil)
   (t (indent-sgml-in-closure))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode closure-template-html-mode html-mode  "closure-template-html"
  "Major mode for editing Closure Templates.
\\{closure-template-html-mode-map}"
  :group 'closure-template-html

  ;; it mainly from sgml-mode font lock setting
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        `((closure-template-html-font-lock-keywords
           closure-template-html-font-lock-keywords-1
           closure-template-html-font-lock-keywords-2
           ,(closure-template-html-font-lock-keywords-3))
          nil t nil nil
          (font-lock-syntactic-keywords
           . sgml-font-lock-syntactic-keywords)))

  ;; Setting up syntax recognition
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)

  (setq comment-start "/* "
	comment-end " */"
	comment-start-skip "/\\*[ \n\t]+")
  ;; Setting up syntax table
  (modify-syntax-entry ?* ". 23")
  (modify-syntax-entry ?/ ". 14")
(set (make-local-variable 'indent-line-function) 'closure-indent-line))




(provide 'closure-template-html-mode)

;;; closure-template-html-mode.el ends here
