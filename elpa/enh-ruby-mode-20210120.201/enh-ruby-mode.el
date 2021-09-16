;;; enh-ruby-mode.el --- Major mode for editing Ruby files

;; Copyright (C) 2012 -- Ryan Davis
;; Copyright (C) 2010, 2011, 2012
;;   Geoff Jacobsen

;; Author: Geoff Jacobsen
;; Maintainer: Ryan Davis
;; URL: http://github.com/zenspider/Enhanced-Ruby-Mode
;; Created: Sep 18 2010
;; Keywords: languages, elisp, ruby
;; Package-Requires: ((emacs "24.3"))
;; Version: 1.2.0

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with it.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a fork of https://github.com/jacott/Enhanced-Ruby-Mode
;; to provide further enhancements and bug fixes.
;;
;; It has been renamed to enh-ruby-mode.el to avoid name conflicts
;; with ruby-mode that ships with Emacs. All symbols that started with
;; 'ruby now start with 'enh-ruby. This also makes it possible to
;; switch back and forth for testing purposes.

;; Provides fontification, indentation, syntax checking, and navigation for Ruby code.
;;
;; If you're installing manually, you should add this to your .emacs
;; file after putting it on your load path:
;;
;;    (add-to-list 'load-path "(path-to)/Enhanced-Ruby-Mode") ; must be added after any path containing old ruby-mode
;;    (setq enh-ruby-program "(path-to-ruby1.9)/bin/ruby") ; so that still works if ruby points to ruby1.8
;;

(require 'cl-lib)                       ; for cdddr, caddr
(require 'files)                        ; for mode-require-final-newline
(require 'paren)                        ; show-paren-data-function
(require 'seq)                          ; seq-remove, seq-difference
(require 'subr-x)                       ; string-trim-right
(require 'cus-edit)                     ; custom-variable-state
(require 'find-func)                    ; find-library-name

;;; Properties & Other Bullshit Codes:

;; 'l - [, (, {, %w/%i open  or | goalpost open
;; 'r - ], ), }, %w/%i close or | goalpost close
;; 'b - begin, def, case, if
;; 'd - do, {, embexpr (interpolation) start
;; 'e - end, embexpr (interpolation) end, close block }
;; 's - statement start on BACKDENT_KW else/when/rescue etc
;; 'c - continue - period followed by return (or other way around?)

;; pc
;; bc
;; nbc
;; npc

;;; Variables:

(defcustom enh-ruby-add-encoding-comment-on-save nil
  "Adds ruby magic encoding comment on save when non-nil."
  :type 'boolean
  :safe #'booleanp
  :group 'enh-ruby)

(defcustom enh-ruby-bounce-deep-indent nil
  "Bounce between normal indentation and deep indentation when non-nil."
  :type 'boolean
  :safe #'booleanp
  :group 'enh-ruby)

(defcustom enh-ruby-check-syntax 'errors-and-warnings
  "Highlight syntax errors and warnings."
  :type '(radio (const :tag "None" nil)
                (const :tag "Errors" errors)
                (const :tag "Errors and warnings" errors-and-warnings))
  :safe #'enh/symbol-or-null-p
  :group 'enh-ruby)

(defcustom enh-ruby-comment-column 32
  "*Indentation column of comments."
  :type 'integer
  :safe #'integerp
  :group 'enh-ruby)

(defcustom enh-ruby-deep-indent-construct t
  "*Deep indent constructs such as if, def, class and module when non-nil."
  :type 'boolean
  :safe #'booleanp
  :group 'enh-ruby)

(defcustom enh-ruby-deep-indent-paren t
  "*Deep indent lists in parenthesis when non-nil."
  ;; FIX: this applies to square brackets as well
  :type 'boolean
  :safe  #'booleanp
  :group 'enh-ruby)

(defcustom enh-ruby-encoding-map
  '((us-ascii       . nil)       ;; Do not put coding: us-ascii
    (utf-8          . nil)       ;; Do not put coding: utf-8
    (shift-jis      . cp932)     ;; Emacs charset name of Shift_JIS
    (shift_jis      . cp932)     ;; MIME charset name of Shift_JIS
    (japanese-cp932 . cp932))    ;; Emacs charset name of CP932
  "Alist to map encoding name from Emacs to ruby."
  :type '(alist :key-type   (symbol :tag "Encoding")
                :value-type (choice (const :tag "Ignore" nil)
                                    (symbol :tag "Charset")))
  :safe (lambda (xs)
          (and (listp xs)
           (cl-every (lambda (x)
                       (and (symbolp (car x))
                            (enh/symbol-or-null-p (cdr x))))
                     xs)))
  :group 'enh-ruby)

(defcustom enh-ruby-extra-keywords nil
  "*A list of idents that will be fontified as keywords.

`erm-reset' will need to be called in order for any global
changes to take effect.

This variable can also be buffer local in which case it will
override the global value for the buffer it is local
to. `ruby-local-enable-extra-keywords' needs to be called after
the value changes."
  :type '(repeat string)
  :safe #'listp
  :group 'enh-ruby)

(defcustom enh-ruby-hanging-brace-deep-indent-level 0
  "*Extra hanging deep indentation for continued ruby curly or square braces."
  :type 'integer
  :safe #'integerp
  :group 'enh-ruby)

(defcustom enh-ruby-hanging-brace-indent-level 2
  "*Extra hanging indentation for continued ruby curly braces."
  :type 'integer
  :safe #'integerp
  :group 'enh-ruby)

(defcustom enh-ruby-hanging-indent-level 2
  "*Extra hanging Indentation for continued ruby statements."
  :type 'integer
  :safe #'integerp
  :group 'enh-ruby)

(defcustom enh-ruby-hanging-paren-deep-indent-level 0
  "*Extra hanging deep indentation for continued ruby parenthesis."
  :type 'integer
  :safe #'integerp
  :group 'enh-ruby)

(defcustom enh-ruby-hanging-paren-indent-level 2
  "*Extra hanging indentation for continued ruby parenthesis."
  :type 'integer
  :safe #'integerp
  :group 'enh-ruby)

(defcustom enh-ruby-indent-level 2
  "*Indentation of ruby statements."
  :type 'integer
  :safe #'integerp
  :group 'enh-ruby)

(defcustom enh-ruby-indent-tabs-mode nil
  "*Indentation can insert tabs in ruby mode if this is non-nil."
  :type 'boolean
  :safe #'booleanp
  :group 'enh-ruby)

(defcustom enh-ruby-preserve-indent-in-heredocs nil
  "Indent heredocs and multiline strings like ‘text-mode’.

Warning: does not play well with command ‘electric-indent-mode’."
  :type 'boolean
  :safe #'booleanp
  :group 'enh-ruby)

(defcustom enh-ruby-program "ruby"
  "The ruby program to parse the source."
  :type 'string
  :safe #'stringp
  :group 'enh-ruby)

(defcustom enh-ruby-use-encoding-map t
  "*Use `enh-ruby-encoding-map' to set encoding magic comment if this is non-nil."
  :type 'boolean
  :safe #'booleanp
  :group 'enh-ruby)

(defvar enh-ruby-use-ruby-mode-show-parens-config nil
  "This flag has no effect anymore as ERM supports command ‘show-paren-mode’ directly.")

(make-obsolete-variable 'enh-ruby-use-ruby-mode-show-parens-config nil "2018-04-03")

;; TODO: renames:
;;
;; enh-ruby-indent-level:                    2
;; enh-ruby-hanging-indent-level:            2
;; enh-ruby-hanging-brace-deep-indent-level: 0
;; enh-ruby-hanging-brace-indent-level:      2
;; enh-ruby-hanging-paren-deep-indent-level: 0
;; enh-ruby-hanging-paren-indent-level:      2
;;
;; Versus:
;;
;; enh-ruby-indent-level:                    2
;; enh-ruby-indent-level-hanging:            2
;; enh-ruby-indent-level-hanging-paren:      2
;; enh-ruby-indent-level-hanging-paren-deep: 0
;; enh-ruby-indent-level-hanging-brace:      2
;; enh-ruby-indent-level-hanging-brace-deep: 0

(defvar need-syntax-check-p)
(defvar erm-buff-num)
(defvar erm-e-w-status)
(defvar erm-full-parse-p)

;;; Constants

(defconst enh-ruby-block-end-re "\\_<end\\_>")

(defconst enh-ruby-symbol-chars "a-zA-Z0-9_=?!")

(defconst enh-ruby-symbol-re (concat "[" enh-ruby-symbol-chars "]"))

(defconst enh-ruby-defun-beg-keywords
  '("class" "module" "def")
  "Keywords at the beginning of definitions.")

(defconst enh-ruby-defun-beg-re
  (regexp-opt enh-ruby-defun-beg-keywords)
  "Regexp to match the beginning of definitions.")

(defconst enh-ruby-defun-and-name-re
  (concat "\\(" enh-ruby-defun-beg-re "\\)[ \t]+\\("
                                         ;; \\. and :: for class method
                                         "\\([A-Za-z_]" enh-ruby-symbol-re "*\\|\\.\\|::" "\\)"
                                         "+\\)")
  "Regexp to match definitions and their name.")

(defconst erm-process-delimiter
  "\n\0\0\0\n")

(define-abbrev-table 'enh-ruby-mode-abbrev-table ()
  "Abbrev table used by enhanced-ruby-mode.")

(define-abbrev enh-ruby-mode-abbrev-table "end" "end"
  #'indent-for-tab-command :system t)

(defvar enh-ruby-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "{"             #'enh-ruby-electric-brace)
    (define-key map "}"             #'enh-ruby-electric-brace)
    (define-key map (kbd "M-C-a")   #'enh-ruby-beginning-of-defun)
    (define-key map (kbd "M-C-e")   #'enh-ruby-end-of-defun)
    (define-key map (kbd "M-C-b")   #'enh-ruby-backward-sexp)
    (define-key map (kbd "M-C-f")   #'enh-ruby-forward-sexp)
    (define-key map (kbd "M-C-p")   #'enh-ruby-beginning-of-block)
    (define-key map (kbd "M-C-n")   #'enh-ruby-end-of-block)
    (define-key map (kbd "M-C-h")   #'enh-ruby-mark-defun)
    (define-key map (kbd "M-C-q")   #'enh-ruby-indent-exp)
    (define-key map (kbd "C-c C-f") #'enh-ruby-find-file)
    (define-key map (kbd "C-c C-e") #'enh-ruby-find-error)
    (define-key map (kbd "C-c /")   #'enh-ruby-insert-end)
    (define-key map (kbd "C-c {")   #'enh-ruby-toggle-block)
    (define-key map (kbd "M-C-u")   #'enh-ruby-up-sexp)
    (define-key map (kbd "C-j")     #'reindent-then-newline-and-indent)
    map)
  "Syntax table in use in ‘enh-ruby-mode’ buffers.")

(defvar enh-ruby-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\` "\"" table)
    (modify-syntax-entry ?#  "<"  table)
    (modify-syntax-entry ?\n ">"  table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?$  "."  table)
    (modify-syntax-entry ??  "_"  table)
    (modify-syntax-entry ?_  "_"  table)
    (modify-syntax-entry ?:  "_"  table)
    (modify-syntax-entry ?<  "."  table)
    (modify-syntax-entry ?>  "."  table)
    (modify-syntax-entry ?&  "."  table)
    (modify-syntax-entry ?|  "."  table)
    (modify-syntax-entry ?%  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?/  "."  table)
    (modify-syntax-entry ?+  "."  table)
    (modify-syntax-entry ?*  "."  table)
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?\; "."  table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table used by ‘enh-ruby-mode’ buffers.")

(defconst enh-ruby-font-lock-keyword-beg-re "\\(?:^\\|[^.@$:]\\|\\.\\.\\)")

(defconst enh-ruby-font-lock-keywords
  `(;; Core methods that have required arguments.
    (,(concat
       enh-ruby-font-lock-keyword-beg-re
       (regexp-opt
        '( ;; built-in methods on Kernel TODO: add more via reflection?
          "at_exit"
          "autoload"
          "autoload?"
          "callcc"
          "catch"
          "eval"
          "exec"
          "format"
          "lambda"
          "load"
          "loop"
          "open"
          "p"
          "print"
          "printf"
          "proc"
          "putc"
          "puts"
          "require"
          "require_relative"
          "spawn"
          "sprintf"
          "syscall"
          "system"
          "throw"
          "trace_var"
          "trap"
          "untrace_var"
          "warn"
          ;; keyword-like private methods on Module
          "alias_method"
          "attr"
          "attr_accessor"
          "attr_reader"
          "attr_writer"
          "define_method"
          "extend"
          "include"
          "module_function"
          "prepend"
          "private_class_method"
          "private_constant"
          "public_class_method"
          "public_constant"
          "refine"
          "using")
        'symbols))
     (1 (unless (looking-at " *\\(?:[]|,.)}=]\\|$\\)")
          font-lock-builtin-face)))
    ;; Kernel methods that have no required arguments.
    (,(concat
       enh-ruby-font-lock-keyword-beg-re
       (regexp-opt
        '("__callee__"
          "__dir__"
          "__method__"
          "abort"
          "binding"
          "block_given?"
          "caller"
          "exit"
          "exit!"
          "fail"
          "fork"
          "global_variables"
          "local_variables"
          "private"
          "protected"
          "public"
          "raise"
          "rand"
          "readline"
          "readlines"
          "sleep"
          "srand")
        'symbols))
     (1 font-lock-builtin-face))
    )
  "Additional expressions to highlight in ‘enh-ruby-mode’.")

(defconst enh-ruby-font-names
  '(nil
    font-lock-string-face
    font-lock-type-face
    font-lock-variable-name-face
    font-lock-comment-face
    font-lock-constant-face
    font-lock-string-face
    enh-ruby-string-delimiter-face
    enh-ruby-regexp-delimiter-face
    font-lock-function-name-face
    font-lock-keyword-face
    enh-ruby-heredoc-delimiter-face
    enh-ruby-op-face
    enh-ruby-regexp-face)
  "Font faces used by ‘enh-ruby-mode’.")

;;; Code:

;;;###autoload
(defun enh/symbol-or-null-p (x)
  "Return true if X is either a symbol or null. Used for defcustom safe check."
  (or (symbolp x)
      (null x)))

;;;###autoload
(define-derived-mode enh-ruby-mode prog-mode "EnhRuby"
  "Enhanced Major mode for editing Ruby code.

\\{enh-ruby-mode-map}"

  (setq-local comment-column               enh-ruby-comment-column)
  (setq-local comment-end                  "")
  (setq-local comment-start                "#")
  (setq-local comment-start-skip           "#+ *")
  (setq-local erm-buff-num                 nil)
  (setq-local erm-e-w-status               nil)
  (setq-local erm-full-parse-p             nil)
  (setq-local indent-line-function         #'enh-ruby-indent-line)
  ;; (setq-local forward-sexp-function        #'enh-ruby-forward-sexp)
  (setq-local need-syntax-check-p          nil)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local parse-sexp-ignore-comments   t)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local require-final-newline        mode-require-final-newline)
  (setq-local beginning-of-defun-function  #'enh-ruby-beginning-of-defun)
  (setq-local end-of-defun-function        #'enh-ruby-end-of-defun)
  (setq-local show-paren-data-function     #'erm-show-paren-data-function)
  (setq-local paragraph-start              (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate           paragraph-start)

  (setq-local add-log-current-defun-function
              'enh-ruby-add-log-current-method)

  (setq-local font-lock-keywords    enh-ruby-font-lock-keywords)
  (setq font-lock-defaults          '((enh-ruby-font-lock-keywords) t))
  (setq indent-tabs-mode            enh-ruby-indent-tabs-mode)
  (setq imenu-create-index-function #'enh-ruby-imenu-create-index)

  (if enh-ruby-add-encoding-comment-on-save
    (add-hook 'before-save-hook #'enh-ruby-mode-set-encoding nil t))

  (add-hook 'change-major-mode-hook #'erm-major-mode-changed     nil t)
  (add-hook 'kill-buffer-hook       #'erm-buffer-killed          nil t)

  (abbrev-mode)
  (erm-reset-buffer))

;;; Faces:

(require 'color nil t)

(defun erm-darken-color (name)
  "Return color NAME with foreground 20% darker."
  (color-darken-name (face-attribute name :foreground) 20))

(defun erm-define-faces ()
  "Define faces for ‘enh-ruby-mode’."

 (defface enh-ruby-string-delimiter-face
   `((t :foreground ,(erm-darken-color font-lock-string-face)))
   "Face used to highlight string delimiters like quotes and %Q."
   :group 'enh-ruby)

 (defface enh-ruby-heredoc-delimiter-face
   `((t :foreground ,(erm-darken-color font-lock-string-face)))
   "Face used to highlight string heredoc anchor strings like <<END and END"
   :group 'enh-ruby)

 (defface enh-ruby-regexp-delimiter-face
   `((t :foreground ,(erm-darken-color font-lock-string-face)))
   "Face used to highlight regexp delimiters like / and %r."
   :group 'enh-ruby)

 (defface enh-ruby-regexp-face
   `((t :foreground ,(face-attribute font-lock-string-face :foreground)))
   "Face used to highlight the inside of regular expressions"
   :group 'enh-ruby)

 (defface enh-ruby-op-face
   `((t :foreground ,(erm-darken-color font-lock-keyword-face)))
   "Face used to highlight operators like + and ||"
   :group 'enh-ruby)

 (defface erm-syn-errline
   '((t (:box (:line-width 1 :color "red"))))
   "Face used for marking error lines."
   :group 'enh-ruby)

 (defface erm-syn-warnline
   '((t (:box (:line-width 1 :color "orange"))))
   "Face used for marking warning lines."
   :group 'enh-ruby))

(add-hook 'enh-ruby-mode-hook #'erm-define-faces)

;;; Support Functions:

(defun enh-ruby-mode-set-encoding ()
  "Check encoding on save and create a magic comment if non-standard encoding."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward "[^[:ascii:]]" nil t)
      (goto-char (point-min))
      (let ((coding-system
             (or coding-system-for-write
                 buffer-file-coding-system)))
        (if coding-system
            (setq coding-system
                  (or (coding-system-get coding-system 'mime-charset)
                      (coding-system-change-eol-conversion coding-system nil))))
        (setq coding-system
              (if coding-system
                  (symbol-name
                   (or (and enh-ruby-use-encoding-map
                            (cdr (assq coding-system enh-ruby-encoding-map)))
                       coding-system))
                "ascii-8bit"))
        (if (looking-at "^#!") (beginning-of-line 2))
        (cond ((looking-at "\\s *#.*-\*-\\s *\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)\\s *\\(;\\|-\*-\\)")
               (unless (string= (match-string 2) coding-system)
                 (goto-char (match-beginning 2))
                 (delete-region (point) (match-end 2))
                 (and (looking-at "-\*-")
                      (let ((n (skip-chars-backward " ")))
                        (cond ((= n 0) (insert "  ") (backward-char))
                              ((= n -1) (insert " "))
                              ((forward-char)))))
                 (insert coding-system)))
              ((looking-at "\\s *#.*coding\\s *[:=]"))
              ((equal "utf-8" coding-system) 'do-nothing) ; hack? should check version?
              (t (insert "# -*- coding: " coding-system " -*-\n"))
              )))))

(defvar erm-ruby-process nil "The current erm process where Emacs is interacting with.")
(defvar erm-response     nil "Private variable.")
(defvar erm-parsing-p    nil "Parsing: t, nil, 'a (all?), 'p (partial?).")

(defun erm-ruby-get-process ()
  "Return (or create) the current ruby parser process."
  (when (and erm-ruby-process (not (equal (process-status erm-ruby-process) 'run)))
    (let ((message (and erm-parsing-p erm-response)))
      (erm-reset)
      (if message
          (error "%s" message)
        (throw 'interrupted t))))
  (unless erm-ruby-process
    (let ((process-connection-type nil))
      (setq erm-ruby-process
            (start-process "erm-ruby-process"
                           nil
                           enh-ruby-program (concat (erm-source-dir)
                                                    "ruby/erm.rb")))
      (set-process-coding-system erm-ruby-process 'utf-8 'utf-8)
      (set-process-filter erm-ruby-process #'erm-filter)
      (set-process-query-on-exit-flag erm-ruby-process nil)
      (process-send-string
       erm-ruby-process
       (concat "x0:"
               (mapconcat #'identity (default-value 'enh-ruby-extra-keywords) " ")
               ":"
               erm-process-delimiter))))

  erm-ruby-process)

(defvar erm-no-parse-needed-p nil "Private variable.")
(defvar erm-source-dir        nil "Private variable.")

(defun erm-source-dir ()
  "Return the directory for enh-ruby-mode.el."
  (or erm-source-dir
    (setq erm-source-dir (file-name-directory (find-lisp-object-file-name
                                               'erm-source-dir
                                               (symbol-function 'erm-source-dir))))))


(defvar erm-next-buff-num nil "Private variable.")
(defvar erm-parse-buff nil "Private variable.")
(defvar erm-reparse-list nil "Private variable.")
(defvar erm-syntax-check-list nil "Private variable.")

(defun erm-reset-syntax-buffers (list)
  (let ((buffer (car list)))
    (when buffer
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (setq need-syntax-check-p nil)))
      (erm-reset-syntax-buffers (cdr list)))))

(defun erm-reset ()
  "Reset all ‘enh-ruby-mode’ buffers and restart the ruby parser."
  (interactive)
  (erm-reset-syntax-buffers erm-syntax-check-list)
  (setq erm-reparse-list nil
        erm-syntax-check-list nil
        erm-parsing-p nil
        erm-parse-buff nil
        erm-next-buff-num 1)
  (when erm-ruby-process
    (delete-process erm-ruby-process)
    (setq erm-ruby-process nil))

  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq 'enh-ruby-mode major-mode)
        (erm-reset-buffer)))))

(defun erm-major-mode-changed ()
  (remove-hook 'kill-buffer-hook #'erm-buffer-killed t)
  (erm-buffer-killed))

(defun erm-proc-string (prefix)
  (concat prefix (number-to-string erm-buff-num) ":" erm-process-delimiter))

(defun erm-buffer-killed ()
  (remove-hook 'kill-buffer-hook #'erm-buffer-killed t)
  (catch 'interrupted
   (process-send-string (erm-ruby-get-process) (erm-proc-string "k"))))

(defun erm-reset-buffer ()
  (setq erm-buff-num erm-next-buff-num)
  (setq erm-next-buff-num (1+ erm-buff-num))
  (add-hook 'after-change-functions #'erm-req-parse nil t)
  (unless
      (enh-ruby-local-enable-extra-keywords)
    (enh-ruby-fontify-buffer)))

(defun enh-ruby-local-enable-extra-keywords ()
  "If the variable `ruby-extra-keywords' is buffer local then enable the keywords for current buffer."
  (when (local-variable-p 'enh-ruby-extra-keywords)
      (process-send-string (erm-ruby-get-process)
                           (concat "x"
                                   (number-to-string erm-buff-num) ":"
                                   (mapconcat #'identity enh-ruby-extra-keywords " ")
                                   ":" erm-process-delimiter))
      (enh-ruby-fontify-buffer)
      t))

(defun enh-ruby-electric-brace (arg)
  (interactive "P")
  (insert-char last-command-event 1)
  (enh-ruby-indent-line)
  (delete-char -1)
  (self-insert-command (prefix-numeric-value arg)))

(defun enh-ruby-brace-to-do-end (orig end)
  (let (beg-marker end-marker)
    (goto-char end)
    (when (eq (char-before) ?\})
      (delete-char -1)
      (when (save-excursion
              (skip-chars-backward " \t")
              (not (bolp)))
        (insert "\n"))
      (insert "end")
      (setq end-marker (point-marker))
      (when (and (not (eobp)) (eq (char-syntax (char-after)) ?w))
        (insert " "))
      (goto-char orig)
      (delete-char 1)
      (when (eq (char-syntax (char-before)) ?w)
        (insert " "))
      (insert "do")
      (setq beg-marker (point-marker))
      (when (looking-at "\\(\\s \\)*|")
        (unless (match-beginning 1)
          (insert " "))
        (goto-char (1+ (match-end 0)))
        (search-forward "|"))
      (unless (looking-at "\\s *$")
        (insert "\n"))
      (indent-region beg-marker end-marker)
      (goto-char beg-marker))))

(defun enh-ruby-do-end-to-brace (orig end)
  (let (beg-marker end-marker beg-pos end-pos)
    (goto-char (- end 3))
    (when (looking-at enh-ruby-block-end-re)
      (delete-char 3)
      (setq end-marker (point-marker))
      (insert "}")
      (goto-char orig)
      (delete-char 2)
      ;; Maybe this should be customizable, let's see if anyone asks.
      (insert "{ ")
      (setq beg-marker (point-marker))
      (when (looking-at "\\s +|")
        (delete-char (- (match-end 0) (match-beginning 0) 1))
        (forward-char)
        (re-search-forward "|" (line-end-position) t))
      (save-excursion
        (skip-chars-forward " \t\n\r")
        (setq beg-pos (point))
        (goto-char end-marker)
        (skip-chars-backward " \t\n\r")
        (setq end-pos (point)))
      (when (or
             (< end-pos beg-pos)
             (and (= (line-number-at-pos beg-pos) (line-number-at-pos end-pos))
                  (< (+ (current-column) (- end-pos beg-pos) 2) fill-column)))
        (just-one-space -1)
        (goto-char end-marker)
        (just-one-space -1))
      (goto-char beg-marker))))

(defun enh-ruby-toggle-block ()
  "Toggle block type from do-end to braces or back.
The block must begin on the current line or above it and end after the point.
If the result is do-end block, it will always be multiline."
  (interactive)
  (let* ((pos (point))
         (block-start (save-excursion
                        (end-of-line)
                        (while (and (not (bobp))
                                    (not (enh-ruby-point-block-p)))
                          (backward-char))
                        (point)))
         (block-end (save-excursion
                      (goto-char block-start)
                      (enh-ruby-forward-sexp)
                      (point))))
    (if (< pos block-end)
        (if (eq (char-after block-start) ?{)
            (enh-ruby-brace-to-do-end block-start block-end)
          (enh-ruby-do-end-to-brace block-start block-end)))))

(defun enh-ruby-imenu-create-index-in-block (prefix beg end)
  (let* ((index-alist '())
         (pos beg)
         (prop (get-text-property pos 'indent)))
    (setq end (or end (point-max)))
    (while (and pos (< pos end))
      (goto-char pos)
      (when (and (eq prop 'b) (looking-at enh-ruby-defun-and-name-re))
        (push (cons (concat (match-string 1) " " (match-string 2)) pos) index-alist))

      (setq prop (and (setq pos (enh-ruby-next-indent-change pos))
                      (get-text-property pos 'indent))))

    index-alist))

(defun enh-ruby-imenu-create-index ()
  (nreverse (enh-ruby-imenu-create-index-in-block nil (point-min) nil)))

(defun enh-ruby-add-log-current-method ()
  "Return current method string."
  (condition-case nil
      (save-excursion
        (enh-ruby-beginning-of-defun 1)
        (when (looking-at enh-ruby-defun-and-name-re)
          (let ((def-or-mod (match-string-no-properties 1))
                (def-name   (match-string-no-properties 2)))
            (if (string= "def" def-or-mod)
                (progn
                  (enh-ruby-up-sexp)
                  (when (looking-at enh-ruby-defun-and-name-re)
                    (let ((mod-or-class (match-string-no-properties 1))
                          (mod-name   (match-string-no-properties 2)))
                      (let* ((meth-name-re (concat
                                            (regexp-opt (list "self" mod-name)
                                                        'words)
                                            "\\.\\(.+\\)"))
                             (cls-meth (and (string-match meth-name-re def-name)
                                            (match-string 2 def-name)))
                             (name (or cls-meth def-name))
                             (sep (if cls-meth "." "#")))
                        (concat mod-name sep name)))))
              nil))))))

;; Stolen shamelessly from James Clark's nxml-mode.
(defmacro erm-with-unmodifying-text-property-changes (&rest body)
  "Evaluate BODY without modifying the buffer's text properties.
Any text properties changes happen as usual but the changes are
not treated as modifications to the buffer."
  (let ((modified (make-symbol "modified")))
    `(let ((,modified (buffer-modified-p))
           (inhibit-read-only t)
           (inhibit-modification-hooks t)
           (buffer-undo-list t)
           (deactivate-mark nil)
           ;; Apparently these avoid file locking problems.
           (buffer-file-name nil)
           (buffer-file-truename nil))
       (unwind-protect
           (progn ,@body)
         (unless ,modified
           (restore-buffer-modified-p nil))))))

(defun enh-ruby-fontify-buffer ()
  "Fontify the current buffer. Useful if faces are out of sync."
  (interactive)
  (if (and erm-parsing-p
           (not (eq erm-parse-buff (current-buffer))))
      (erm-reparse-diff-buf)
    (setq erm-full-parse-p t)
    (condition-case nil
        (erm-req-parse nil nil nil)
      (error nil))))

(defun erm-reparse-diff-buf ()
  (setq erm-reparse-list (cons (current-buffer) erm-reparse-list)))

(defun erm-req-parse (min max len)
  (when (and enh-ruby-check-syntax (not need-syntax-check-p))
    (setq need-syntax-check-p t)
    (setq erm-syntax-check-list (cons (current-buffer) erm-syntax-check-list)))
  (let ((pc (if erm-parsing-p
                (if (eq erm-parse-buff (current-buffer))
                    (setq erm-parsing-p 'a)
                  'dbuf)
              (setq erm-response "")
              (setq erm-parsing-p t)
              (if (not erm-full-parse-p)
                  (if erm-no-parse-needed-p
                      (progn (setq erm-parsing-p nil) 'a)
                    'p)
                (setq min (point-min)
                      max (point-max)
                      len 0
                      erm-full-parse-p nil)
                'r)))
        interrupted-p)
    (setq interrupted-p
          (catch 'interrupted
            (if (eq pc 'dbuf)
                (erm-reparse-diff-buf)
              (setq erm-parse-buff (current-buffer))
              (process-send-string (erm-ruby-get-process)
                                   (format "%s%d:%d:%d:%d:%d:"
                                           pc
                                           erm-buff-num
                                           (point-min)
                                           (point-max)
                                           min
                                           len))
              (process-send-region erm-ruby-process min max)
              (process-send-string erm-ruby-process erm-process-delimiter))
            nil))
    (when interrupted-p
      (setq erm-full-parse-p t))))

(defun erm-wait-for-parse ()
  (while erm-parsing-p
    (accept-process-output (erm-ruby-get-process) 0.5)))

(defun erm-filter (proc response)
  (setq erm-response (concat erm-response response))
  (when (and (> (length erm-response) 5)
             (string= erm-process-delimiter (substring erm-response -5 nil)))
    (setq response (substring erm-response 0 -5))
    (setq erm-response "")
    (unless (buffer-live-p erm-parse-buff)
      (erm-reset))
    (when (buffer-live-p erm-parse-buff)
      (with-current-buffer erm-parse-buff
        (erm-with-unmodifying-text-property-changes
         (erm-parse response))))))

(defun erm-ready ()
  (if erm-full-parse-p
      (enh-ruby-fontify-buffer)
    (setq erm-parsing-p t)
    (process-send-string (erm-ruby-get-process) (erm-proc-string "g"))))

(defun extra-col-% ()
  "Return extra column adjustments in case we are ‘looking-at’ a % construct."
  (or (and (looking-at "%\\([^[:alnum:]]\\|[QqWwIixrs].\\)")
           (1- (length (match-string-no-properties 0))))
      0))

(defun enh-ruby-continue-p (prop)
  "Return whether PROP is a continue property."
  (eq 'c prop))

(defun enh-ruby-block-p (prop)
  "Return whether PROP is a block property."
  (eq 'd prop))

(defun enh-ruby-point-continue-p (point)
  "Return whether property at POINT is a continue property."
  (enh-ruby-continue-p (get-text-property point 'indent)))

(defun enh-ruby-point-block-p (&optional point)
  "Return whether property at POINT is a block property."
  (or point (setq point (point)))
  (enh-ruby-block-p (get-text-property point 'indent)))

(defun enh-ruby-calculate-indent (&optional start-point)
  "Calculate the indentation of the previous line and its level at START-POINT."
  (save-excursion
    (when start-point (goto-char start-point))
    (if (bobp)
        0
      (forward-line 0)
      (skip-syntax-forward " " (line-end-position))
      (let ((pos (line-beginning-position))
            (prop (get-text-property (point) 'indent))
            (face (get-text-property (point) 'font-lock-face)))
        (cond
         ((or (eq 'e prop) (eq 's prop))
          (when (eq 's prop) (forward-char))
          (enh-ruby-backward-sexp)
          (let ((bprop (get-text-property (point) 'indent)))
            (cond ((eq 'd bprop)
                   (setq pos (point))
                   (enh-ruby-skip-non-indentable)
                   (let ((indent (enh-ruby-calculate-indent-1 pos (line-beginning-position)))
                         (chained-stmt-p (save-excursion
                                           (forward-line 0)
                                           (enh-ruby-point-continue-p (point)))))
                     (+ indent
                        (if chained-stmt-p enh-ruby-hanging-indent-level 0))))
                  ((and (not enh-ruby-deep-indent-construct)
                        (eq 'b bprop))
                   (current-indentation))
                  (t
                   (current-column)))))
         ((eq 'r prop)                  ; TODO: make these consistent file-wide
          (let (opening-col opening-is-last-thing-on-line)
            (save-excursion
              (enh-ruby-backward-sexp)
              (setq opening-col (+ (current-column)
                                   (extra-col-%)))
              (forward-char 1)
              (skip-syntax-forward " " (line-end-position))
              (setq opening-is-last-thing-on-line (eolp)))
            (if (and enh-ruby-deep-indent-paren
                     (not enh-ruby-bounce-deep-indent)
                     (not opening-is-last-thing-on-line))
                opening-col             ; deep + !bounce + !hanging = match open
              (forward-line -1)
              (enh-ruby-skip-non-indentable)
              (let* ((opening-char (save-excursion
                                     (enh-ruby-backward-sexp)
                                     (char-after)))
                     (proposed-col (enh-ruby-calculate-indent-1 pos
                                                                (line-beginning-position)))
                     (chained-stmt-p (save-excursion (enh-ruby-backward-sexp)
                                                     (forward-line 0)
                                                     (enh-ruby-point-continue-p (point))))
                     (offset (if (char-equal opening-char ?{)
                                 enh-ruby-hanging-brace-indent-level
                               enh-ruby-hanging-paren-indent-level)))
                (cond ((and chained-stmt-p (not enh-ruby-bounce-deep-indent)) (- proposed-col offset))
                      ((< proposed-col opening-col) (- proposed-col offset))
                      (t opening-col))))))

         ((or (memq face '(font-lock-string-face enh-ruby-heredoc-delimiter-face))
              (and (eq 'font-lock-variable-name-face face)
                   (looking-at "#")))
          (when enh-ruby-preserve-indent-in-heredocs
            (forward-line -1)
            (back-to-indentation))
          (current-column))

         (t
          (forward-line -1)

          (enh-ruby-skip-non-indentable)
          (enh-ruby-calculate-indent-1 pos (line-beginning-position))))))))

(defun erm-looking-at-not-indentable ()
  (skip-syntax-forward " " (line-end-position))
  (let ((face (get-text-property (point) 'font-lock-face)))
    (or (= (point) (line-end-position))
        (memq face '(font-lock-string-face font-lock-comment-face enh-ruby-heredoc-delimiter-face))
        (and (eq 'font-lock-variable-name-face face)
             (looking-at "#"))
        (and (memq face '(enh-ruby-regexp-delimiter-face enh-ruby-string-delimiter-face))
             (> (point) (point-min))
             (eq (get-text-property (1- (point)) 'font-lock-face)
                 'font-lock-string-face)))))

(defun enh-ruby-skip-non-indentable ()
  (forward-line 0)
  (while (and (> (point) (point-min))
              (erm-looking-at-not-indentable))
    (skip-chars-backward " \n\t\r\v\f")
    (forward-line 0)))

(defvar enh-ruby-last-bounce-line nil
  "The last line that `erm-bounce-deep-indent-paren` was run against.")

(defvar enh-ruby-last-bounce-deep nil
  "The last result from `erm-bounce-deep-indent-paren`.")

(defun enh-ruby-calculate-indent-1 (limit pos)
  (goto-char pos)

  (let* ((start-pos pos)
         (start-prop (get-text-property pos 'indent))
         (prop start-prop)
         (indent (- (current-indentation)
                    (if (eq 'c prop) enh-ruby-hanging-indent-level 0)))
         (nbc 0)
         (npc 0)
         col max bc pc)

    (setq enh-ruby-last-bounce-deep
          (and (eq enh-ruby-last-bounce-line (line-number-at-pos))
               (not enh-ruby-last-bounce-deep)))
    (setq enh-ruby-last-bounce-line (line-number-at-pos))

    (while (< pos limit)
      (unless prop
        (setq pos (next-single-property-change pos 'indent (current-buffer) limit))
        (when (< pos limit)
          (setq prop (get-text-property pos 'indent))))
      (setq col (- pos start-pos -1))

      (cond
       ((eq prop 'l)                    ; TODO: comment wtf these mean
        (let ((shallow-indent
               (if (char-equal (char-after pos) ?{)
                   (+ enh-ruby-hanging-brace-indent-level indent)
                 (+ enh-ruby-hanging-paren-indent-level indent)))
              (deep-indent
               (cond ((char-equal (char-after pos) ?{)
                      (+ enh-ruby-hanging-brace-deep-indent-level col))
                     ((char-equal (char-after pos) ?%)
                      (+ enh-ruby-hanging-brace-deep-indent-level
                         col
                         (save-excursion
                           (goto-char pos)
                           (extra-col-%))))
                     (t (+ enh-ruby-hanging-paren-deep-indent-level col))))
              (at-eol (save-excursion
                        (goto-char (1+ pos))
                        (skip-syntax-forward " " (line-end-position))
                        (eolp))))
          (if enh-ruby-bounce-deep-indent
              (setq pc (cons (if enh-ruby-last-bounce-deep
                                 shallow-indent
                               deep-indent)
                             pc))
            (setq pc (cons (if (and (not at-eol) enh-ruby-deep-indent-paren)
                               deep-indent
                             (let ((chained-stmt-p (enh-ruby-continue-p start-prop)))
                               (+ shallow-indent (if chained-stmt-p enh-ruby-hanging-paren-indent-level 0))))
                           pc)))))

       ((eq prop 'r)
        (if pc (setq pc (cdr pc)) (setq npc col)))

       ((memq prop '(b d s))
        (and (not enh-ruby-deep-indent-construct)
             (eq prop 'b)
             (setq col
                   (- col (- (save-excursion
                               (goto-char pos)
                               (current-column))
                             (current-indentation)))))
        (setq bc (cons col bc)))

       ((eq prop 'e)
        (if bc
            (setq bc (cdr bc))
          (setq nbc col))))

      (when (< (setq pos (1+ pos)) limit)
        (setq prop (get-text-property pos 'indent))))

    ;;(prin1 (list indent nbc bc npc pc))
    (setq pc (or (car pc) 0))
    (setq bc (or (car bc) 0))
    (setq max (max pc bc nbc npc))

    (+
     (if (eq 'c (get-text-property limit 'indent)) enh-ruby-hanging-indent-level 0)
     (cond
      ((= max 0)
       (if (not (memq (get-text-property start-pos 'font-lock-face)
                      '(enh-ruby-heredoc-delimiter-face font-lock-string-face)))
           indent
         (goto-char (or (enh-ruby-string-start-pos start-pos) limit))
         (current-indentation)))

      ((= max pc) (if (eq 'c (get-text-property limit 'indent))
                      (- pc enh-ruby-hanging-indent-level)
                    pc))

      ((= max bc)
       (if (eq 'd (get-text-property (+ start-pos bc -1) 'indent))
           (let ((chained-stmt-p (enh-ruby-continue-p start-prop)))
             (+ (enh-ruby-calculate-indent-1 (+ start-pos bc -1) start-pos)
                (* (if chained-stmt-p 2 1) enh-ruby-indent-level)))
         (+ bc enh-ruby-indent-level -1)))

      ((= max npc)
       (goto-char (+ start-pos npc))
       (enh-ruby-backward-sexp)
       (enh-ruby-calculate-indent-1 (point) (line-beginning-position)))

      ((= max nbc)
       (goto-char (+ start-pos nbc -1))
       (enh-ruby-backward-sexp)
       (enh-ruby-calculate-indent-1 (point) (line-beginning-position)))

      (t 0)))))

(defun enh-ruby-string-start-pos (pos)
  (when (< 0 (or (setq pos (previous-single-property-change pos 'font-lock-face)) 0))
    (previous-single-property-change pos 'font-lock-face)))

(defun enh-ruby-show-errors-at (pos face)
  (let ((overlays (overlays-at pos))
        overlay
        messages)

    ;; TODO:
    ;; (-map (lambda (o) (overlay-get o 'help-echo))
    ;;       (-filter (lambda (o) (and (overlay-get o 'erm-syn-overlay)
    ;;                                 (eq (overlay-get o 'font-lock-face) face)))))

    (while overlays
      (setq overlay (car overlays))
      (when (and (overlay-get overlay 'erm-syn-overlay)
                 (eq (overlay-get overlay 'font-lock-face) face))
        (setq messages (cons (overlay-get overlay 'help-echo) messages)))
      (setq overlays (cdr overlays)))

    (message "%s" (mapconcat #'identity messages "\n"))
    messages))

(defun enh-ruby-find-error (&optional warnings)
  "Search back, then forward for a syntax error/warning. Display contents in mini-buffer. Optional WARNINGS will highlight warnings instead of errors. (I think)."
  (interactive "^P")
  (let (overlays
        overlay
        (face (if warnings 'erm-syn-warnline 'erm-syn-errline))
        messages
        (pos (point)))
    (unless (eq last-command #'enh-ruby-find-error)
      (while (and (not messages) (> pos (point-min)))
        (setq messages (enh-ruby-show-errors-at (setq pos (previous-overlay-change pos)) face))))

    (unless messages
      (while (and (not messages) (< pos (point-max)))
        (setq messages (enh-ruby-show-errors-at (setq pos (next-overlay-change pos)) face))))

    (if messages
        (goto-char pos)
      (unless warnings
        (enh-ruby-find-error t)))))

(defun enh-ruby-find-file (filename)
  "Search for and edit FILENAME. Searching is done with `gem
which` but works for standard lib as well as gems."
  (interactive "sgem which ")
  (let* ((command (concat "gem which " filename))
         (output  (shell-command-to-string command))
         (path    (string-trim-right output)))
    (if (file-exists-p path)
        (find-file path)
      (message "%S found nothing" command))))

(defun enh-ruby-up-sexp (&optional arg)
  "Move up one balanced expression (sexp).
With ARG, do it that many times."
  (interactive "^p")
  (unless arg (setq arg 1))
  (while (>= (setq arg (1- arg)) 0)
    (let* ((count 1)
           prop)
      (goto-char
       (save-excursion
         (while (and (not (= (point) (point-min)))
                     (< 0 count))
           (goto-char (enh-ruby-previous-indent-change (point)))
           (setq prop (get-text-property (point) 'indent))
           (setq count (cond
                        ((or (eq prop 'l) (eq prop 'b) (eq prop 'd)) (1- count))
                        ((or (eq prop 'r) (eq prop 'e)) (1+ count))
                        (t count))))
         (point))))))

(defun enh-ruby-beginning-of-defun (&optional arg)
  "Move backward across expression (sexp) looking for a definition begining.
With ARG, do it that many times."
  (interactive "^p")
  (unless arg (setq arg 1))
  (let ((cont t)
        prop)
    (goto-char
     (save-excursion
       (while (>= (setq arg (1- arg)) 0)
         (while (and
                 (> (point) (point-min))
                 (progn
                  (enh-ruby-backward-sexp 1)
                  (setq prop (get-text-property (point) 'indent))
                  (not (and (eq prop 'b) (looking-at enh-ruby-defun-beg-re)))))))
       (point)))))

(defun enh-ruby-mark-defun ()
  "Put mark at end of this Ruby definition, point at beginning."
  (interactive)
  (push-mark (point))
  (enh-ruby-beginning-of-defun 1)
  (enh-ruby-forward-sexp 1)
  (forward-line 1)
  (push-mark (point) nil t)
  (forward-line -1)
  (end-of-line)
  (enh-ruby-backward-sexp 1)
  (forward-line 0))

(defun enh-ruby-indent-exp (&optional shutup-p)
  "Indent each line in the balanced expression following point syntactically.
If optional SHUTUP-P is non-nil, no errors are signalled if no
balanced expression is found."
  (interactive "*P")
  (erm-wait-for-parse)
  (let ((end-pos (save-excursion (enh-ruby-forward-sexp 1) (point))))
    (indent-region (point) end-pos)))

(defun enh-ruby-beginning-of-block (&optional arg)
  "Move backward across one expression (sexp) looking for a block begining.
With ARG, do it that many times."
  (interactive "^p")
  (unless arg (setq arg 1))
  (let ((cont t)
        prop
        pos)
    (goto-char
     (save-excursion
       (while (>= (setq arg (1- arg)) 0)
         (while (progn
                  (enh-ruby-backward-sexp 1)
                  (setq pos (point))
                  (setq prop (get-text-property pos 'indent))
                  (and
                   (> pos (point-min))
                   (not (or (eq prop 'b) (eq prop 'd)))))))
       (point)))))

(defun enh-ruby-end-of-defun (&optional arg)
  "Move forwards across one expression (sexp) looking for a definition end.
With ARG, do it that many times."
  (interactive "^p")
  (unless arg (setq arg 1))
  (let ((cont t)
        prop)
    (while (>= (setq arg (1- arg)) 0)
         (while (and
                 (< (point) (point-max))
                 (progn
                   (enh-ruby-forward-sexp 1)
                   (setq prop (get-text-property (- (point) 3) 'indent))
                   (not (and (eq prop 'e)
                             (save-excursion
                               (enh-ruby-backward-sexp 1)
                               (looking-at enh-ruby-defun-beg-re))))))))
    (point)))

(defun enh-ruby-end-of-block (&optional arg)
  "Move forwards across one balanced expression (sexp) looking for a block end.
With ARG, do it that many times."
  (interactive "^p")
  (unless arg (setq arg 1))
  (let ((cont t)
        prop
        pos)
    (goto-char
     (save-excursion
       (while (>= (setq arg (1- arg)) 0)
         (while (progn
                  (enh-ruby-forward-sexp 1)
                  (setq pos (point))
                  (setq prop (get-text-property (- pos 3) 'indent))
                  (and (< pos (point-max)) (not (eq prop 'e))))))
       (point)))))

(defun enh-ruby-backward-sexp (&optional arg)
  "Move backward across one balanced expression (sexp).
With ARG, do it that many times."
  (interactive "^p")

  (unless arg (setq arg 1))
  (while (>= (setq arg (1- arg)) 0)
    (let* ((pos (point))
           (prop (get-text-property pos 'indent))
           (count 0))

      (unless (or (eq prop 'r) (eq prop 'e))
        (setq prop (and (setq pos (enh-ruby-previous-indent-change pos))
                        (get-text-property pos 'indent))))

      (while (< 0 (setq count
                        (cond
                         ((or (eq prop 'l) (eq prop 'b) (eq prop 'd)) (1- count))
                         ((or (eq prop 'r) (eq prop 'e)) (1+ count))
                         ((eq prop 'c) count)
                         ((eq prop 's) (if (= 0 count) 1 count))
                         (t 0))))
        (goto-char pos)
        (setq prop (and (setq pos (enh-ruby-previous-indent-change pos))
                        (get-text-property pos 'indent))))

      (goto-char (if prop pos (point-min))))))

(defun enh-ruby-forward-sexp (&optional arg)
  "Move backward across one balanced expression (sexp).
With ARG, do it that many times."
  (interactive "^p")
  (unless arg (setq arg 1))
  (let ((i (or arg 1)))
    (cond
     ((< i 0)
      (enh-ruby-backward-sexp (- i)))
     (t
      (skip-syntax-forward " ")
      (while (> i 0)
        (let* ((pos (point))
               (prop (get-text-property pos 'indent))
               (count 0))

          (unless (memq prop '(l b d))
            (setq prop (and (setq pos (enh-ruby-next-indent-change pos))
                            (get-text-property pos 'indent))))

          (while (< 0 (setq count
                            (cond
                             ((memq prop '(l b d)) (1+ count))
                             ((memq prop '(r e))   (1- count))
                             ((memq prop '(c))     count)
                             ((memq prop '(s))     (if (= 0 count) 1 count))
                             (t                    0))))
            (goto-char pos)
            (setq prop (and (setq pos (enh-ruby-next-indent-change pos))
                            (get-text-property pos 'indent))))

          (goto-char (if prop pos (point-max)))

          (cond ((looking-at "end")     ; move past end/}/]/)
                 (forward-word 1))
                ((looking-at "}\\|)\\|]")
                 (forward-char 1))))

        (setq i (1- i)))))))

(defun enh-ruby-insert-end ()
  (interactive)
  (let ((text (save-excursion
                (forward-line 0)
                (if (looking-at "^[ \t]*$")
                    "end"
                  (if (looking-at ".*{[^}]*$")
                      "\n}"
                    "\nend")))))
    (insert text)
    (enh-ruby-indent-line)
    ))

(defun enh-ruby-previous-indent-change (pos)
  (and pos
       (setq pos (1- pos))
       (>= pos (point-min))
       (or (and (get-text-property pos 'indent)
                pos)
           (and (> pos (point-min))
                (get-text-property (1- pos) 'indent)
                (1- pos))
           (enh-ruby-previous-indent-change (previous-single-property-change pos 'indent))
           (point-min)
           )))

(defun enh-ruby-next-indent-change (pos)
  (and pos (setq pos (1+ pos))
       (<= pos (point-max))
       (or (and (get-text-property pos 'indent) pos)
           (and (< pos (point-max))
                (get-text-property (1+ pos) 'indent)
                (1+ pos))
           (next-single-property-change pos 'indent))))

(defun enh-ruby-indent-line (&optional _ignored)
  "Correct indentation of the current ruby line."
  (erm-wait-for-parse)
  (unwind-protect
      (progn
        (setq erm-no-parse-needed-p t)
        (enh-ruby-indent-to (enh-ruby-calculate-indent)))
    (setq erm-no-parse-needed-p nil)))

(defun enh-ruby-indent-to (indent)
  "Indent the current line until INDENT is reached."
  (unless (= (current-indentation) indent)
    (save-excursion
      (beginning-of-line)
      (let ((pos (point))
            (prop (get-text-property (point) 'indent)))
        (delete-horizontal-space)
        (indent-to indent)
        (if (eq 'c prop) (put-text-property pos (1+ pos) 'indent 'c)))))

  (if (< (current-column) (current-indentation))
      (back-to-indentation)))

(defun enh-ruby-add-faces (list)
  (let* ((ipos     (car   list))
         (buf-size (car   ipos))
         (istart   (cadr  ipos))
         (iend     (cl-caddr ipos))
         (rpos     (cdr   (cadr list))))

    (unless (and (= (buffer-size) buf-size))
      (throw 'interrupted t))

    (if (or (/= (point-min) istart) (/= (point-max) iend))
        (setq erm-full-parse-p t)

      (when (> iend 0)
        (remove-text-properties istart iend '(indent nil))

        (setq ipos (cl-cdddr ipos))

        (while ipos
          (put-text-property (cadr ipos) (1+ (cadr ipos)) 'indent (car ipos))
          (setq ipos (cddr ipos))
          )

        (while rpos
          (remove-text-properties (car rpos) (cadr rpos) '(font-lock-face nil))
          (setq rpos (cddr rpos))
          ))

      (while (setq list (cdr list))
        (let ((face (nth (caar list) enh-ruby-font-names))
              (pos (cdar list)))
          (while pos
            (put-text-property (car pos) (cadr pos) 'font-lock-face face)
            (setq pos (cddr pos))))))))

(defun erm-syntax-response (response)
  (save-excursion
    (dolist (ol (overlays-in (point-min) (point-max)))
    (when (and (overlayp ol) (overlay-get ol 'erm-syn-overlay))
        (delete-overlay ol)
        ))
    (goto-char (point-min))
    (let ((warn-count 0)
          (error-count 0)
          (e-w erm-e-w-status)
          (last-line 1))
      (while (string-match ":\\([0-9]+\\): *\\(\\(warning\\)?[^\n]+\\)\n" response)
        (let (beg end ov
                  (line-no (string-to-number (match-string 1 response)))
                  (msg (match-string 2 response))
                  (face (if (string= "warning" (match-string 3 response)) 'erm-syn-warnline 'erm-syn-errline)))
          (setq response (substring response (match-end 0)))
          (forward-line (- line-no last-line))

          (when (or (eq face 'erm-syn-errline) (eq enh-ruby-check-syntax 'errors-and-warnings))
            (if (and (not (eq ?: (string-to-char response)))
                     (string-match "\\`[^\n]*\n\\( *\\)\\^\n" response))
                (progn
                  (setq beg (point))
                  (condition-case nil
                      (forward-char  (length (match-string 1 response)))
                    (error (goto-char (point-max))))
                  (setq end (point))

                  (condition-case nil
                      (progn
                        (backward-sexp)
                        (forward-sexp))

                    (error (back-to-indentation)))
                  (setq beg (if (>= (point) end)
                                (1- end)
                              (if (< (point) beg)
                                  (if (>= beg end) (1- end) beg)
                                (point)))))

              (move-end-of-line nil)
              (skip-chars-backward " \n\t\r\v\f")
              (while (and (> (point) (point-min))
                          (eq 'font-lock-comment-face (get-text-property (point) 'font-lock-face)))
                (backward-char))
              (skip-chars-backward " \n\t\r\v\f")
              (setq end (point))
              (back-to-indentation)
              (setq beg (point)))

            (if (eq face 'erm-syn-warnline)
                (setq warn-count (1+ warn-count))
              (setq error-count (1+ error-count)))

            (setq ov (make-overlay beg end nil t t))
            (overlay-put ov 'font-lock-face face)
            (overlay-put ov 'help-echo      msg)
            (overlay-put ov 'erm-syn-overlay  t)
            (overlay-put ov 'priority (if (eq 'erm-syn-warnline face) 99 100)))

          (setq last-line line-no)
          ))
      (if (eq (+ error-count warn-count) 0)
          (setq e-w nil)
        (setq e-w (format ":%d/%d" error-count warn-count)))
      (when (not (string= e-w erm-e-w-status))
        (setq erm-e-w-status e-w)
        (force-mode-line-update)))))

(defun erm-do-syntax-check ()
  (unless erm-parsing-p
    (let ((buffer (car erm-syntax-check-list)))
      (setq erm-syntax-check-list (cdr erm-syntax-check-list))
      (if (buffer-live-p buffer)
          (with-current-buffer buffer
            (when need-syntax-check-p
              (setq need-syntax-check-p nil)
              (setq erm-parsing-p t)
              (process-send-string (erm-ruby-get-process) (erm-proc-string "c"))))
        (if erm-syntax-check-list
            (erm-do-syntax-check))))))

(defun erm-parse (response)
  (let (interrupted-p
        (cmd (aref response 0))
        (send-next-p (eq 'a erm-parsing-p)))
    (setq erm-parsing-p nil)
    (cond
     ((eq ?\( cmd)
          (setq interrupted-p
                (condition-case nil
                    (catch 'interrupted
                      (if send-next-p
                          (erm-ready)
                        (enh-ruby-add-faces (car (read-from-string response))))
                      nil)
                  (error t)))
          (if interrupted-p
              (setq erm-full-parse-p t)

            (if erm-full-parse-p
                (enh-ruby-fontify-buffer)
              (if (car erm-reparse-list)
                  (with-current-buffer (car erm-reparse-list)
                    (setq erm-reparse-list (cdr erm-reparse-list))
                    (enh-ruby-fontify-buffer))
                (erm-do-syntax-check)
                ))))

     ((eq ?c cmd)
      (unless need-syntax-check-p
        (erm-syntax-response (substring response 1)))
      (erm-do-syntax-check))

     (t
      (setq erm-full-parse-p t)
      (error "%s" (substring response 1))))))

(defun erm--end-p ()
  "Is point directly after a block closing \"end\"."
  (let ((end-pos (- (point) 3)))
    (and (>= end-pos (point-min))
         (string= "end" (buffer-substring end-pos (point)))
         (eq (get-text-property end-pos 'indent) 'e))))

(defun erm-show-paren-data-function ()
  ;; First check if we are on opening ('b or 'd). We only care about
  ;; the word openers "if", "do" etc (normal show-paren handles "{")
  (if (and (memq (get-text-property (point) 'indent) '(b d))
           (looking-at "\\w"))
      (save-excursion
        (let ((opener-beg (point))
              (opener-end (save-excursion (forward-word) (point)))
              (closer-end (progn (enh-ruby-forward-sexp 1) (point))))
          (list
           opener-beg
           opener-end
           (save-excursion (skip-syntax-backward ")w") (point))
           closer-end
           (not (erm--end-p)))))
    ;; Now check if we are at a closer ("end")
    (if (erm--end-p)
        (let ((end-pos (point)))
          (save-excursion
            (enh-ruby-backward-sexp 1)
            (list
             (- end-pos 3)
             end-pos
             (point)
             (save-excursion (skip-syntax-forward "(w") (point))
             (or (not (looking-at "\\w"))
                 (not (memq (get-text-property (point) 'indent) '(b d)))))))
      (show-paren--default))))

;;; Debugging / Bug Reporting:

(defun enh-ruby--all-vars-with (pattern)
  "Return all defcustom variables that match PATTERN.
Used for inserting file-local-variables and sending in bug reports."
  (let (mode-vars)
    (mapatoms (lambda (symbol)
                (when (and (string-match-p pattern (symbol-name symbol))
                           (get symbol 'standard-value))
                  (add-to-list 'mode-vars symbol))))
    (sort mode-vars
          'symbol<)))

(defun enh-ruby--variable-standard-p (sym)
  (and (equal (custom-variable-state sym (symbol-value sym))
              'standard)
       (equal (symbol-value sym)
              (default-value sym))))

(defun enh-ruby--changed-vars-with (pattern)
  "Return all changed defcustom varibles that match PATTERN.
Used for inserting file-local-variables and sending in bug reports."
  (seq-remove
   #'enh-ruby--variable-standard-p
   (enh-ruby--all-vars-with pattern)))

(defun enh-ruby--variable-values (vars)
  "Map VARS to a list of (variable value) pairs."
  (mapcar (lambda (symbol) (list symbol (symbol-value symbol)))
          vars))

(defun enh-ruby--uptime-seconds ()
  "Return the number of seconds that Emacs has been running."
  (float-time (time-subtract (current-time) before-init-time)))

(defun enh-ruby-eval-file-local-variables ()
  "Re-evaluate file-local variables and reindent the file.

Really only useful for my debugging sessions when I'm debugging
stuff by changing vars over and over."
  (interactive)
  (hack-local-variables)
  (indent-region (point-min) (point-max)))

(defun enh-ruby--add-fl-variables (pairs)
  (mapc (lambda (kv) (apply 'add-file-local-variable kv))
        (enh-ruby--variable-values pairs)))

(defun enh-ruby-add-file-local-variables ()
  "Insert all currently customized variables for this mode as file-local variables. This is mainly for providing a complete example in a bug report."
  (interactive)
  (enh-ruby--add-fl-variables (enh-ruby--changed-vars-with "enh-ruby")))

(defun enh-ruby-add-all-file-local-variables ()
  "Insert all variables for this mode as file-local variables. This is mainly for providing a complete example in a bug report."
  (interactive)
  (enh-ruby--add-fl-variables (enh-ruby--all-vars-with "enh-ruby")))

(defun enh-ruby-add-indent-file-local-variables ()
  "Insert all indent variables for this mode as file-local variables. This is mainly for providing a complete example in a bug report."
  (interactive)
  (enh-ruby--add-fl-variables (enh-ruby--all-vars-with "enh-ruby.*indent")))

(defun enh-ruby-del-file-local-variables ()
  "Delete all file-local-variables that aren't customized"
  (interactive)
  (mapc #'delete-file-local-variable
        (seq-difference (enh-ruby--all-vars-with "enh-ruby")
                        (enh-ruby--changed-vars-with "enh-ruby"))))

(defun enh-ruby-bug-report ()
  "Fill a buffer with data to make a ‘enh-ruby-mode’ bug report."
  (interactive)
  (with-help-window "*enh-ruby-mode bug report*"
    (princ "Please provide the following output in your bug report:\n")
    (princ "\n")
    (let ((print-quoted t))
      (pp (append `((emacs-uptime ,(enh-ruby--uptime-seconds))
                    (mode-path    ,(find-library-name "enh-ruby-mode")))
                  (enh-ruby--variable-values
                   (append '(emacs-version system-type major-mode)
                           (enh-ruby--changed-vars-with "enh-ruby"))))))
    (princ "\n")
    (princ "Also consider using enh-ruby-add-file-local-variables with any code you provide.\n\n")
    (princ "Hit 'q' to close this buffer.")))

(erm-reset)

(provide 'enh-ruby-mode)

;;; enh-ruby-mode.el ends here
