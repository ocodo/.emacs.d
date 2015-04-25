;;; lispyscript-mode.el --- Major mode for LispyScript code.

;; Copyright 2013 Kris Jenkins

;; Author: Kris Jenkins <krisajenkins@gmail.com>
;; Maintainer: Kris Jenkins <krisajenkins@gmail.com>
;; Keywords: lisp languages
;; Package-Version: 20130828.719
;; URL: https://github.com/krisajenkins/lispyscript-mode
;; Created: 16th April 2013
;; Version: 0.3.6

;;; Commentary:
;;
;; A major mode for LispyScript http://lispyscript.com/

(require 'lisp-mode)
(require 'font-lock)
(require 'rx)

;;; Code:

(defvar lispyscript-font-lock-defaults
  `((,(rx "("
          (group "macro")
          (one-or-more whitespace) (group (one-or-more word)))
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    (,(rx "("
          (group (or "var" "template" "doMonad" "monad" "withMonad" "testGroup" "testRunner"))
          (one-or-more (or "\n" whitespace)) (group (one-or-more word)))
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    (,(rx "("
          (group (or "->"
                     "array" "arrayInit" "arrayInit2d" "object" "new" "javascript"
                     "if" "cond" "when" "unless" "do" "function" "try" "catch" "throw" "loop"
                     "each" "each2d" "eachKey" "reduce" "map" "for"
                     "template-repeat" "template-repeat-key"
                     "include"
                     "true" "false"
                     "assert"))
	  word-end)
     (1 font-lock-keyword-face))
    ;; Match a single-quoted string, handling escapes, using "Friedl's Unrolled Loop" pattern.
    (,(rx (group "'"
		 (* (not (in "\\'")))
		 (* "\\" anything
		    (* (not (in "\\'"))))
		 "'"))
     (1 font-lock-string-face))
    (,(rx bow (group (or "true" "false")))
     (1 font-lock-keyword-face))
    (,(rx bow (group "~" (opt ?@) (one-or-more word) (opt "...")))
     (1 font-lock-type-face))
    (,(rx bow (group "___" (one-or-more word)))
     (1 font-lock-type-face))))

;;;###autoload
(define-derived-mode lispyscript-mode lisp-mode "LispyScript"
  "Major mode for LispyScript"
  (dolist '(lambda (char)
	     (modify-syntax-entry char "w" lispyscript-mode-syntax-table))
    '(?_ ?~ ?. ?- ?> ?< ?! ??))
  (setq font-lock-defaults '(lispyscript-font-lock-defaults)))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.ls\\'" 'lispyscript-mode))

(provide 'lispyscript-mode)
;;; lispyscript-mode.el ends here
