;;; dart-mode.el --- Major mode for editing Dart files -*- lexical-binding: t; -*-

;; Author: Natalie Weizenbaum
;; URL: http://code.google.com/p/dart-mode
;; Package-Version: 20170127.1652
;; Version: 0.15
;; Package-Requires: ((cl-lib "0.5") (dash "2.10.0") (flycheck "0.23"))
;; Keywords: language

;; Copyright (C) 2011 Google Inc.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file contains several functions and variables adapted from the
;; code in https://github.com/dominikh/go-mode.el
;;
;; go-mode.el uses this license:
;;
;; Copyright (c) 2014 The go-mode Authors. All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;    * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;    * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;;    * Neither the name of the copyright holder nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; To install, see https://github.com/nex3/dart-mode/blob/master/README.md
;;
;; Known bugs:
;;
;; * Multiline strings using """ and ''' are not recognized. They fontify
;;   correctly, but only because they look like three strings in a row.
;; * In a map with identifier keys, the first key is fontified like a label.
;; * Return values for operator methods aren't fontified correctly.
;; * Untyped parameters aren't fontified correctly.
;; * Quotes immediately after string interpolation are marked as unclosed.
;; * Sexp movement doesn't properly ignore quotes in interpolation.
;; * Methods using "=>" can cause indentation woes.
;; * C and C++ modes seem to be hosed.

;; Definitions adapted from go-mode.el are
;;
;; gofmt-command gofmt-args gofmt-show-errors gofmt go--apply-rcs-patch
;; gofmt--kill-error-buffer gofmt--process-errors gofmt-before-save
;; go--goto-line go--delete-whole-line

;;; Code:

(require 'cc-mode)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile (c-add-language 'dart-mode 'java-mode))

(require 'cl-lib)
(require 'dash)
(require 'flycheck)
(require 'json)

;;; CC configuration

(c-lang-defconst c-symbol-start
  dart (concat "[" c-alpha "_]"))

(c-lang-defconst c-identifier-ops
  dart nil)

(c-lang-defconst c-after-id-concat-ops
  dart nil)

(c-lang-defconst c-multiline-string-start-char
  dart ?@)

(c-lang-defconst c-opt-cpp-prefix
  dart "\\s *#\\s *")

(c-lang-defconst c-cpp-message-directives
  dart nil)

(c-lang-defconst c-cpp-include-directives
  dart nil)

(c-lang-defconst c-opt-cpp-macro-define
  dart nil)

(c-lang-defconst c-cpp-expr-directives
  dart '("import" "source" "library" "resource"))

(c-lang-defconst c-cpp-expr-functions
  dart nil)

(c-lang-defconst c-operators
  dart `((prefix "#")
         (postfix-if-paren "<" ">")
         (prefix "super")
         (left-assoc ".")
         (postfix "++" "--" "[" "]" "(" ")")
         (unary "++" "--" "+" "-" "!" "~" "negate" "new" "const")
         (left-assoc "*" "/" "%")
         (left-assoc "+" "-")
         (left-assoc "<<" ">>" ">>>")
         (left-assoc "<" ">" "<=" ">=")
         (left-assoc "==" "!=" "===" "!==" "is" "is!")
         (left-assoc "&")
         (left-assoc "^")
         (left-assoc "|")
         (left-assoc "&&")
         (left-assoc "||")
         (right-assoc-sequence "?" ":")
         (left-assoc "=>")
         (right-assoc ,@(c-lang-const c-assignment-operators))
         (left-assoc ",")))

(c-lang-defconst c-overloadable-operators
  dart '("==" "<" ">" "<=" ">=" "-" "+" "*" "/" "%" "|" "^" "&"
         "<<" ">>" ">>>" "[]=" "[]" "~" "negate"))

(c-lang-defconst c-opt-op-identifier-prefix
  dart (c-make-keywords-re t '("operator")))

(c-lang-defconst c-doc-comment-start-regexp
  dart nil)

(c-lang-defconst c-paragraph-start
  dart "$")

(c-lang-defconst c-primitive-type-kwds
  dart '("Dynamic" "void" "num" "int" "double" "bool"))

(c-lang-defconst c-class-decl-kwds
  dart '("class" "interface"))

;; Don't put these in c-modifier-kwds because they can be used without a type
;; following them.
(c-lang-defconst c-typeless-decl-kwds
  dart '("abstract" "const" "factory" "final" "operator" "static" "typedef" "var"))

(c-lang-defconst c-modifier-kwds
  dart nil)

(c-lang-defconst c-other-decl-kwds
  dart nil)

(c-lang-defconst c-decl-hangon-kwds
  dart '("get" "set" "native"))

(c-lang-defconst c-postfix-decl-spec-kwds
  dart '("extends" "implements" "factory"))

(c-lang-defconst c-type-list-kwds
  dart '("new" "const" "is" "is!" "extends" "implements" "factory"))

(c-lang-defconst c-ref-list-kwds
  dart nil)

(c-lang-defconst c-block-stmt-2-kwds
  dart '("for" "if" "switch" "while" "catch"))

(c-lang-defconst c-simple-stmt-kwds
  dart '("break" "continue" "return" "throw"))

(c-lang-defconst c-before-label-kwds
  dart '("break" "continue"))

(c-lang-defconst c-nonlabel-token-key
  dart (concat (concat "\\s\(\\|" (c-lang-const c-nonlabel-token-key))))

(c-lang-defconst c-inexpr-class-kwds
  dart nil)

(c-lang-defconst c-inexpr-brace-list-kwds
  dart nil)

(c-lang-defconst c-other-kwds
  dart '("in"))

(c-lang-defconst c-decl-prefix-re
  dart "\\([\{\}\([;,<]+\\)")

(c-lang-defconst c-cast-parens
  dart nil)

(c-lang-defconst c-block-prefix-disallowed-chars
  dart (cl-set-difference (c-lang-const c-block-prefix-disallowed-chars)
                          '(?\" ?')))

(c-lang-defconst c-type-decl-prefix-key
  dart "\\(\(\\)\\([^=]\\|$\\)")

(c-lang-defconst c-after-suffixed-type-decl-key
  dart (concat (c-lang-const c-after-suffixed-type-decl-key) "\\|:"))

(c-lang-defconst c-opt-type-suffix-key
  dart nil)

(c-lang-defconst c-recognize-typeless-decls
  dart t)

(c-lang-defconst c-recognize-<>-arglists
  dart t)

(c-lang-defconst c-opt-postfix-decl-spec-kwds
  dart '("native"))

(c-lang-defconst c-opt-postfix-decl-spec-kwds
  dart '("native"))

(push '(dart-brace-list-cont-nonempty . 0)
      (get 'c-offsets-alist 'c-stylevar-fallback))

(defconst dart-c-style
  '("java"
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)
    (fill-column . 80)
    (c-offsets-alist . ((arglist-intro . ++)
                        (arglist-cont-nonempty . ++)
                        (statement-block-intro . dart-block-offset)
                        (block-close . dart-block-offset)
                        (dart-brace-list-cont-nonempty .
                         dart-brace-list-cont-nonempty-offset)
                        (case-label . +))))
  "The default Dart styles.")

(c-add-style "dart" dart-c-style)

(defvar dart-mode-map (c-make-inherited-keymap)
  "Keymap used in dart-mode buffers.")

;;; CC indentation support

(defvar c-syntactic-context nil
  "A dynamically-bound variable used by cc-mode.")

(defun dart-block-offset (info)
  "Calculate the correct indentation for inline functions.

When indenting inline functions, we want to pretend that
functions taking them as parameters essentially don't exist."
  (cl-destructuring-bind (syntax . anchor) info
    (let ((arglist-count
           (cl-loop for (symbol . _) in c-syntactic-context
                    count (eq symbol 'arglist-cont-nonempty))))
      (if (> arglist-count 0)
          (- (* -1 c-basic-offset arglist-count)
             (if (eq syntax 'block-close) c-basic-offset 0))
        (if (eq syntax 'block-close) 0 '+)))))

(defun dart-brace-list-cont-nonempty-offset (info)
  "Indent a brace-list line in the same style as arglist-cont-nonempty.
This could be either an actual brace-list or an optional parameter."
  (cl-destructuring-bind (_ . anchor) info
    ;; If we're in a function definition with optional arguments, indent as if
    ;; the brace wasn't there. Currently this misses the in-function function
    ;; definition, but that's probably acceptable.
    (if (and
         (save-excursion (backward-up-list) (eq (char-after) ?\[))
         (assq 'topmost-intro
               (save-excursion (goto-char anchor) (c-guess-basic-syntax))))
        '++
      ;; Otherwise, we're in an actual brace list, in which case only indent
      ;; once.
      '+)))

(defun dart-in-block-p (syntax-guess)
  "Return whether or not the immediately enclosing {} block is a code block.
The other option, of course, is a map literal.

SYNTAX-GUESS is the output of `c-guess-basic-syntax'."
  (save-excursion
    (c-safe
      ;; If we're in a continued statement within a class, we want to know we're
      ;; in a class so we can return true.
      (when (eq 'statement-cont (caar syntax-guess))
        (save-excursion
          (c-beginning-of-statement-1 nil t t)
          (setq syntax-guess (c-guess-basic-syntax))))

      (backward-up-list)
      (when (= (char-after) ?\{)
        (c-backward-comments)
        (or
         ;; Both anonymous and named functions have a ")" immediately before the
         ;; code block.
         (= (char-before) ?\))
         ;; "else" and "try" are the only keywords that come immediately before
         ;; a block.  Look only back at most 4 characters (the length of
         ;; "else") for performance reasons.
         (looking-back "\\<\\(else\\|try\\)\\>" (- (point) 4))
         ;; CC is good at figuring out if we're in a class.
         (assq 'inclass syntax-guess))))))

(defadvice c-guess-basic-syntax (after dart-guess-basic-syntax activate)
  (when (c-major-mode-is 'dart-mode)
    (let* ((syntax (car (last ad-return-value)))
           (type (car syntax)))
      (save-excursion
        (back-to-indentation)

        (or
         ;; Handle indentation in a constructor with an initializer on a
         ;; separate line.
         (when (memq type '(defun-block-intro inline-close))
           (save-excursion
             (c-safe
               (goto-char (cadr syntax))
               (when (= (char-after) ?:)
                 (c-beginning-of-statement-1)
                 (setq ad-return-value `((,type ,(point))))
                 t))))

         ;; Handle array literal indentation
         (when (memq type
                     '(arglist-intro
                       arglist-cont
                       arglist-cont-nonempty
                       arglist-close))
           (save-excursion
             (c-safe
               (backward-up-list)
               (when (= (char-after) ?\[)
                 (setq ad-return-value
                       `((,(cl-case type
                             (arglist-intro 'brace-list-intro)
                             (arglist-cont 'brace-list-entry)
                             (arglist-cont-nonempty 'dart-brace-list-cont-nonempty)
                             (arglist-close 'brace-list-close))
                          ,(cadr syntax)))))
               t)))

         ;; Handle map literal indentation
         (when (and (memq type '(label statement-block-intro statement-cont statement
                                 block-close defun-block-intro defun-close))
                    (not (dart-in-block-p ad-return-value)))
           (save-excursion
             (c-safe
               (if (= (char-after) ?\})
                   (progn
                     (backward-up-list)
                     (when (= (char-after) ?\{)
                       (back-to-indentation)
                       (setq ad-return-value `((brace-list-close ,(point))))))
                 (c-backward-comments)
                 ;; Completely reset ad-return-value here because otherwise it
                 ;; gets super-screwy.
                 (if (= (char-before) ?\{)
                     (progn
                       (back-to-indentation)
                       (setq ad-return-value `((brace-list-intro ,(point))))
                       t)
                   (backward-up-list)
                   (when (= (char-after) ?\{)
                     (forward-char)
                     (let ((contp (not (looking-at "\\s-*$"))))
                       (c-forward-comments)
                       (back-to-indentation)
                       (setq ad-return-value
                             `((,(if contp 'dart-brace-list-cont-nonempty
                                   'brace-list-entry)
                                ,(point))))
                       t))))))))))))

(defadvice c-inside-bracelist-p (after dart-inside-bracelist-p activate)
  ;; This function is only called within c-guess-basic-syntax. Since we do all
  ;; out brace-list detection in our advice, we just never report being in a
  ;; bracelist there.
  (when (c-major-mode-is 'dart-mode)
    (setq ad-return-value nil)))

(defadvice c-search-decl-header-end (around dart-search-decl-header-end activate)
  (if (not (c-major-mode-is 'dart-mode)) ad-do-it
    (let ((base (point)))
      (while (and
              (c-syntactic-re-search-forward "[;{=:]" nil 'move t t)
              (c-end-of-current-token base))
        (setq base (point)))
      ;; If we hit :, we're in a member initialization list and we want to
      ;; ignore = signs.
      (when (= (char-before) ?:)
        (while (and
                (c-syntactic-re-search-forward "[;{]" nil 'move t t)
                (c-end-of-current-token base))
        (setq base (point)))))))

(if (fboundp 'c-parse-state-1)
  (defadvice c-parse-state (around dart-c-parse-state activate)
    (if (not (c-major-mode-is 'dart-mode)) ad-do-it
      ;; c-parse-state is a wrapper around c-parse-state-1 that does some tricks
      ;; to ensure that dangling brackets in preprocessor commands don't screw up
      ;; parse information for the real world. In Dart, all "preprocessor"
      ;; directives have matched braces, so we don't need to worry about that. The
      ;; wrapper was also screwing up indentation in weird ways, so we just ignore
      ;; it.
      (setq ad-return-value (c-parse-state-1)))))


;;; Additional fontification support

(defun dart-fontify-region (beg end)
  "Use fontify the region between BEG and END as Dart.

This will overwrite fontification due to strings and comments."
  (save-excursion
    (save-match-data
      (save-restriction
        (let ((font-lock-dont-widen t))
          (narrow-to-region (- beg 1) end)
          ;; font-lock-fontify-region apparently isn't inclusive,
          ;; so we have to move the beginning back one char
          (font-lock-fontify-region (point-min) (point-max)))))))

(defun dart-limited-forward-sexp (limit &optional arg)
  "Move forward using `forward-sexp' or to limit,
whichever comes first."
  (let (forward-sexp-function)
    (condition-case err
        (save-restriction
          (narrow-to-region (point) limit)
          (forward-sexp arg))
      (scan-error
       (unless (equal (nth 1 err) "Unbalanced parentheses")
         (signal 'scan-error (cdr err)))
       (goto-char limit)))))

(defun dart-highlight-interpolation (limit)
  "Highlight interpolation (${foo})."
  (let ((start (point)))
    (when (re-search-forward "\\(\\${\\)" limit t)
      (if (elt (parse-partial-sexp start (point)) 3) ; in a string
          (save-match-data
            (forward-char -1)
            (let ((beg (point)))
              (dart-limited-forward-sexp limit)
              (dart-fontify-region (+ 1 beg) (point)))

            ;; Highlight the end of the interpolation.
            (when (eq (char-before) ?})
              (put-text-property (- (point) 1) (point) 'face font-lock-variable-name-face))
            t)
        (looking-at "\\<\\>")))))


;;; Boilerplate font-lock piping

(defcustom dart-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in DART mode.
Each list item should be a regexp matching a single identifier."
  :group 'dart-mode)

(defconst dart-font-lock-keywords-1 (c-lang-const c-matchers-1 dart)
  "Minimal highlighting for Dart mode.")

(defconst dart-font-lock-keywords-2 (c-lang-const c-matchers-2 dart)
  "Fast normal highlighting for Dart mode.")

(defconst dart-font-lock-keywords-3
  (cons
   '(dart-highlight-interpolation 1 font-lock-variable-name-face prepend)
   (c-lang-const c-matchers-3 dart))
  "Accurate normal highlighting for Dart mode.")

(defvar dart-font-lock-keywords dart-font-lock-keywords-3
  "Default expressions to highlight in Dart mode.")

(defvar dart-mode-syntax-table nil
  "Syntax table used in dart-mode buffers.")
(unless dart-mode-syntax-table
  (setq dart-mode-syntax-table
        (funcall (c-lang-const c-make-mode-syntax-table dart))))


;;; Dart analysis server

(cl-defstruct
    (dart--analysis-server
     (:constructor dart--make-analysis-server))
  "Struct containing data for an instance of a Dart analysis server.

The slots are:
- `process': the process of the running server.
- `buffer': the buffer where responses from the server are written."
  process buffer)

(defgroup dart-mode nil
  "Major mode for editing Dart code."
  :group 'languages)

(defvar dart-debug nil
  "If non-nil, enables writing debug messages for dart-mode.")

(defcustom dart-enable-analysis-server nil
  "If non-nil, enables support for Dart analysis server.

The Dart analysis server adds support for error checking, code completion,
navigation, and more."
  :group 'dart-mode
  :type 'boolean
  :package-version '(dart-mode . "0.12"))

(defvar dart--analysis-server nil
  "The instance of the Dart analysis server we are communicating with.")

(defcustom dart-executable-path (executable-find "dart")
  "The absolute path to the 'dart' executable."
  :group 'dart-mode
  :type 'file
  :package-version '(dart-mode . "0.12"))

(defcustom dart-analysis-server-snapshot-path
  (when dart-executable-path
    (concat (file-name-directory dart-executable-path)
            (file-name-as-directory "snapshots")
            "analysis_server.dart.snapshot"))
  "The absolute path to the snapshot file that runs the Dart analysis server."
  :group 'dart-mode
  :type 'file
  :package-version '(dart-mode . "0.12"))

(defvar dart-analysis-roots nil
  "The list of analysis roots that are known to the analysis server.

All Dart files underneath the analysis roots are analyzed by the analysis
server.")

(defvar dart--analysis-server-next-id 0
  "The ID to use for the next request to the Dart analysis server.")

(defvar dart--analysis-server-callbacks nil
  "An alist of ID to callback to be called when the analysis server responds.

Each request to the analysis server has an associated ID.  When the analysis
server sends a response to a request, it tags the response with the ID of the
request.  We look up the callback for the request in this alist and run it with
the JSON decoded server response.")

(defun dart-info (msg)
  "Logs MSG to the dart log if `dart-debug' is non-nil."
  (when dart-debug (dart-log msg)))

(defun dart-log (msg)
  "Logs MSG to the dart log."
  (let* ((log-buffer (get-buffer-create "*dart-debug*"))
         (iso-format-string "%Y-%m-%dT%T%z")
         (timestamp-and-log-string
          (format-time-string iso-format-string (current-time))))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert "\n\n\n")
      (insert (concat timestamp-and-log-string
                      "\n"
                      msg))
      (insert "\n"))))

(defun dart--start-analysis-server-for-current-buffer ()
  "Initialize Dart analysis server for current buffer.

This starts Dart analysis server and adds either the pub root
directory or the current file directory to the analysis roots."
  (if (not dart--analysis-server) (dart-start-analysis-server))
  ;; TODO(hterkelsen): Add this file to the priority files.
  (dart-add-analysis-root-for-file)
  (add-hook 'first-change-hook 'dart-add-analysis-overlay t t)
  (add-hook 'after-change-functions 'dart-change-analysis-overlay t t)
  (add-hook 'after-save-hook 'dart-remove-analysis-overlay t t)
  (add-to-list 'flycheck-checkers 'dart-analysis-server))

(defun dart-start-analysis-server ()
  "Start the Dart analysis server."
  (when dart--analysis-server
    (kill-process
     (dart--analysis-server-process dart--analysis-server))
    (kill-buffer (dart--analysis-server-buffer dart--analysis-server)))
  (let* ((process-connection-type nil)
         (dart-process
          (start-process "dart-analysis-server"
                         "*dart-analysis-server*"
                         dart-executable-path
                         dart-analysis-server-snapshot-path
                         "--no-error-notification")))
    (set-process-query-on-exit-flag dart-process nil)
    (setq dart--analysis-server
          (dart--analysis-server-create dart-process))))

(defun dart--analysis-server-create (process)
  "Create a Dart analysis server from PROCESS."
  (let* ((buffer (generate-new-buffer (process-name process)))
                 (instance (dart--make-analysis-server
                            :process process
                            :buffer buffer)))
    (buffer-disable-undo (dart--analysis-server-buffer instance))
    (set-process-filter
     process
     (lambda (_ string)
       (dart--analysis-server-process-filter instance string)))
    instance))

(defun dart-add-analysis-overlay ()
  "Report to the Dart analysis server that it should overlay this buffer.

The Dart analysis server allows clients to 'overlay' file contents with
a client-supplied string.  This is needed because we want Emacs to report
errors for the current contents of the buffer, not whatever is saved to disk."
  (dart--analysis-server-send
   "analysis.updateContent"
   `((files .
            ((,buffer-file-name . ((type . "add")
                                   (content . ,(buffer-string)))))))))

(defun dart-change-analysis-overlay
    (change-begin change-end change-before-length)
  "Report to analysis server that it should change the overlay for this buffer.

The region that changed ranges from CHANGE-BEGIN to CHANGE-END, and the
length of the text before the change is CHANGE-BEFORE-LENGTH. See also
`dart-add-analysis-overlay'."
  (dart--analysis-server-send
   "analysis.updateContent"
   `((files
      . ((,buffer-file-name
          . ((type . "change")
             (edits
              . (((offset . ,(- change-begin 1))
                  (length . ,change-before-length)
                  (replacement
                   . ,(buffer-substring change-begin change-end))))))))))))

(defun dart-remove-analysis-overlay ()
  "Remove the overlay for the current buffer since it has been saved.

See also `dart-add-analysis-overlay'."
  (dart--analysis-server-send
   "analysis.updateContent"
   `((files . ((,buffer-file-name . ((type . "remove"))))))))

(defun dart-add-analysis-root-for-file (&optional file)
  "Add the given FILE's root to the analysis server's analysis roots.

A file's root is the pub root if it is in a pub package, or the file's directory
otherwise.  If no FILE is given, then this will default to the variable
`buffer-file-name'."
  (let* ((file-to-add (if file file buffer-file-name))
         (pub-root (locate-dominating-file file-to-add "pubspec.yaml"))
         (current-dir (file-name-directory file-to-add)))
    (if pub-root (dart-add-to-analysis-roots (expand-file-name pub-root))
      (dart-add-to-analysis-roots (expand-file-name current-dir)))))

(defun dart-add-to-analysis-roots (dir)
  "Add DIR to the analysis server's analysis roots.

The analysis roots are directories that contain Dart files. The analysis server
analyzes all Dart files under the analysis roots and provides information about
them when requested."
  (add-to-list 'dart-analysis-roots dir)
  (dart--send-analysis-roots))

(defun dart--send-analysis-roots ()
  "Send the current list of analysis roots to the analysis server."
  (dart--analysis-server-send
   "analysis.setAnalysisRoots"
   `(("included" . ,dart-analysis-roots)
     ("excluded" . nil))))

(defun dart--analysis-server-send (method &optional params callback)
  "Send the METHOD request to the server with optional PARAMS.

PARAMS should be JSON-encodable.  If you provide a CALLBACK, it will be called
with the JSON decoded response.  Otherwise, the output will just be checked."
  (let ((req-without-id (dart--analysis-server-make-request method params)))
    (dart--analysis-server-enqueue req-without-id callback)))

(defun dart--analysis-server-make-request (method &optional params)
  "Construct a request for the analysis server.

The constructed request will call METHOD with optional PARAMS."
  `((method . ,method) (params . ,params)))

(defun dart--analysis-server-on-error-callback (response)
  "If RESPONSE has an error, report it."
  (-when-let (resp-err (assoc-default 'error response))
    (error "Analysis server error: %s" (assoc-default 'message resp-err))))

(defun dart--analysis-server-enqueue (req-without-id callback)
  "Send REQ-WITHOUT-ID to the analysis server, call CALLBACK with the result."
  (setq dart--analysis-server-next-id (1+ dart--analysis-server-next-id))
  (let ((request
         (json-encode (push (cons 'id (format "%s" dart--analysis-server-next-id))
                            req-without-id))))

    ;; Enqueue the request so that we can be sure all requests are processed in
    ;; order.
    (push (cons dart--analysis-server-next-id
                (or callback #'dart--analysis-server-on-error-callback))
          dart--analysis-server-callbacks)

    (dart-info (concat "Sent: " request))
    (process-send-string (dart--analysis-server-process dart--analysis-server)
                         (concat request "\n"))))

(defun dart--analysis-server-process-filter (das string)
  "Handle the event or method response from the dart analysis server.

The server DAS has STRING added to the buffer associated with it.
Method responses are paired according to their pending request and
the callback for that request is given the json decoded response."
  (let ((buf (dart--analysis-server-buffer das)))
    ;; The buffer may have been killed if the server was restarted
    (when (buffer-live-p buf)
      ;; We use a buffer here because emacs might call the filter before the
      ;; entire line has been written out. In this case we store the
      ;; unterminated line in a buffer to be read when the rest of the line is
      ;; output.
      (with-current-buffer buf
        (goto-char (point-max))
        (insert string)
        (let ((buf-lines (split-string (buffer-string) "\n")))
          (delete-region (point-min) (point-max))
          (insert (-last-item buf-lines))
          (let ((json-lines
                 (-map (lambda (s)
                         (dart-info (concat "Received: " s))
                         (json-read-from-string s))
                       (-filter (lambda (s)
                                  (not (or (null s) (string= "" s))))
                                (-butlast buf-lines)))))
            (-each json-lines 'dart--analysis-server-handle-msg)))))))

(defun dart--analysis-server-handle-msg (msg)
  "Handle the parsed MSG from the analysis server."
  (-when-let* ((id-assoc (assoc 'id msg))
               (raw-id (cdr id-assoc))
               (id (string-to-number raw-id)))
    (-if-let (resp-closure (assoc id dart--analysis-server-callbacks))
        (progn
          (setq dart--analysis-server-callbacks
                (assq-delete-all id dart--analysis-server-callbacks))
          (funcall (cdr resp-closure) msg))
      (-if-let (err (assoc 'error msg))
          (dart--analysis-server-on-error-callback msg)
        (dart-info (format "No callback was associated with id %s" raw-id))))))

(defun dart--flycheck-start (_ callback)
  "Run the CHECKER and report the errors to the CALLBACK."
  (dart-info (format "Checking syntax for %s" (current-buffer)))
  (dart--analysis-server-send
   "analysis.getErrors"
   `((file . ,(buffer-file-name)))
   (let ((buffer (current-buffer)))
     (lambda (response)
       (dart--report-errors response buffer callback)))))

(flycheck-define-generic-checker 'dart-analysis-server
  "Checks Dart source code for errors using Dart analysis server."
  :start 'dart--flycheck-start
  :modes '(dart-mode))

(defun dart--report-errors (response buffer callback)
  "Report the errors returned from the analysis server.

The errors contained in RESPONSE from Dart analysis server run on BUFFER are
reported to CALLBACK."
  (dart-info (format "Reporting to flycheck: %s" response))
  (-when-let* ((resp-result (cdr (assoc 'result response)))
               (resp-errors (cdr (assoc 'errors resp-result))))
    (let ((fly-errors
           (-map (lambda (err) (dart--to-flycheck-err err buffer)) resp-errors)))
      (dart-info (format "Parsed errors: %s" fly-errors))
      (funcall callback 'finished fly-errors))))

(defun dart--to-flycheck-err (err buffer)
  "Create a flycheck error from a dart ERR in BUFFER."
  (-let* ((severity (cdr (assoc 'severity err)))
          (location (cdr (assoc 'location err)))
          (msg (cdr (assoc 'message err)))
          (level (dart--severity-to-level severity))
          (filename (cdr (assoc 'file location)))
          (line (cdr (assoc 'startLine location)))
          (column (cdr (assoc 'startColumn location))))
    (flycheck-error-new
     :buffer buffer
     :checker 'dart-analysis-server
     :filename filename
     :line line
     :column column
     :message msg
     :level level)))

(defun dart--severity-to-level (severity)
  "Convert SEVERITY to a flycheck level."
  (cond
   ((string= severity "INFO") 'info)
   ((string= severity "WARNING") 'warning)
   ((string= severity "ERROR") 'error)))


;;; Formatting

(defcustom dartfmt-command "dartfmt"
  "The 'dartfmt' command."
  :type 'string
  :group 'dart-mode)

(defcustom dartfmt-args nil
  "Additional arguments to pass to dartfmt."
  :type '(repeat string)
  :group 'dart-mode)

(defcustom dartfmt-show-errors 'buffer
  "Where to display dartfmt error output.
It can either be displayed in its own buffer, in the echo area, or not at all.

Please note that Emacs outputs to the echo area when writing
files and will overwrite dartfmt's echo output if used from inside
a `before-save-hook'."
  :type '(choice
          (const :tag "Own buffer" buffer)
          (const :tag "Echo area" echo)
          (const :tag "None" nil))
  :group 'dart-mode)

(defvar dartfmt-compilation-regexp
  '("^line \\([0-9]+\\), column \\([0-9]+\\) of \\([^ \n]+\\):" 3 1 2)
  "Specifications for matching errors in dartfmt's output.
See `compilation-error-regexp-alist' for help on their format.")

(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'dartfmt dartfmt-compilation-regexp))
(add-to-list 'compilation-error-regexp-alist 'dartfmt)

(defun dartfmt ()
  "Format the current buffer according to the dartfmt tool."
  (interactive)
  (let ((tmpfile (make-temp-file "dartfmt" nil ".dart"))
        (patchbuf (get-buffer-create "*Dartfmt patch*"))
        (errbuf (if dartfmt-show-errors (get-buffer-create "*Dartfmt Errors*")))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        our-dartfmt-args)
    (unwind-protect
        (save-restriction
          (widen)
          (if errbuf
              (with-current-buffer errbuf
                (setq buffer-read-only nil)
                (erase-buffer)))
          (with-current-buffer patchbuf
            (erase-buffer))
          (write-region nil nil tmpfile)
          (setq our-dartfmt-args (append our-dartfmt-args
                                       dartfmt-args
                                       (list "-w" tmpfile)))
          (message "Calling dartfmt: %s %s" dartfmt-command our-dartfmt-args)
          (if (zerop (apply #'call-process dartfmt-command nil errbuf nil our-dartfmt-args))
              (progn
                (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                    (message "Buffer is already dartmted")
                  (dart--apply-rcs-patch patchbuf)
                  (message "Applied dartfmt"))
                (if errbuf (dartfmt--kill-error-buffer errbuf)))
            (message "Could not apply dartfmt")
            (if errbuf (dartfmt--process-errors (buffer-file-name) tmpfile errbuf))))
      (kill-buffer patchbuf)
      (delete-file tmpfile))))

(defun dart--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in dart--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (dart--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (dart--delete-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in dart--apply-rcs-patch")))))))))

(defun dartfmt--kill-error-buffer (errbuf)
  "Kill the dartfmt error buffer."
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (kill-buffer errbuf))))

(defun dartfmt--process-errors (filename tmpfile errbuf)
  "Display the dartfmt errors."
  (message tmpfile)
  (with-current-buffer errbuf
    (if (eq dartfmt-show-errors 'echo)
        (progn
          (message "%s" (buffer-string))
          (dartfmt--kill-error-buffer errbuf))
      (goto-char (point-min))
      (insert "dartfmt errors:\n")
      (while (search-forward-regexp (concat "\\(" (regexp-quote tmpfile) "\\):") nil t)
        (replace-match (file-name-nondirectory filename) t t nil 1))
      (compilation-mode)
      (display-buffer errbuf))))

;;;###autoload
(defun dartfmt-before-save ()
  "Add this to .emacs to run dartfmt on the current buffer when saving:
 (add-hook 'before-save-hook 'dartfmt-before-save).

Note that this will cause dart-mode to get loaded the first time
you save any file, kind of defeating the point of autoloading."

  (interactive)
  (when (eq major-mode 'dart-mode) (dartfmt)))


;;; Utility functions

(defun dart-beginning-of-statement ()
  "Moves to the beginning of a Dart statement.

Unlike `c-beginning-of-statement', this handles maps correctly
and will move to the top level of a bracketed statement."
  (while
      (progn
        (back-to-indentation)
        (while (eq (char-after) ?})
          (forward-char)
          (forward-sexp -1)
          (back-to-indentation))
        (when (not (-dart-beginning-of-statement-p)) (forward-line -1)))))

(defun -dart-beginning-of-statement-p ()
  "Returns whether the point is at the beginning of a statement.

Statements are assumed to begin on their own lines. This returns
true for positions before the start of the statement, but on its line."
  (and
   (save-excursion
     (skip-syntax-forward " ")
     (not (or (bolp) (eq (char-after) ?}))))
   (save-excursion
     (skip-syntax-backward " ")
     (when (bolp)
       (cl-loop do (forward-char -1)
             while (looking-at "^ *$"))
       (skip-syntax-backward " ")
       (cl-case (char-before)
         ((?} ?\;) t)
         ((?{) (dart-in-block-p (c-guess-basic-syntax))))))))

(defun dart--goto-line (line)
  "Move to the specified line."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun dart--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))



;;; Initialization

;;;###autoload (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

;;;###autoload
(defun dart-mode ()
  "Major mode for editing Dart files.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `dart-mode-hook'.

Key bindings:
\\{dart-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table dart-mode-syntax-table)
  (setq major-mode 'dart-mode
        mode-name "Dart")
  (use-local-map dart-mode-map)
  (c-init-language-vars dart-mode)
  (c-common-init 'dart-mode)
  (c-set-style "dart")
  (when dart-enable-analysis-server
    (if (or (null dart-executable-path)
            (null dart-analysis-server-snapshot-path))
        (dart-log
         "Cannot find `dart' executable or Dart analysis server snapshot.")
      (dart--start-analysis-server-for-current-buffer)))
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'dart-mode-hook)
  (c-update-modeline))

(provide 'dart-mode)

;;; dart-mode.el ends here
