;;; gnu-apl-mode --- Emacs mode for GNU APL -*- lexical-binding: t -*-
;;;
;;; Copyright (C) 2013-2015 Elias Mårtenson
;;;
;;; Author: Elias Mårtenson <lokedhs@gmail.com>
;;; Version: 1.4.0
;;; Keywords: languages
;;; URL: http://www.gnu.org/software/apl/
;;;
;;; Commentary:
;;;
;;; Emacs mode for GNU APL
;;;
;;; This mode provides both normal editing facilities for APL code as
;;; well as an interactive mode. The interactive mode is started using
;;; the command ‘gnu-apl’.
;;;
;;; The mode provides two different ways to input APL symbols. The
;;; first method is enabled by default, and simply binds keys with the
;;; "super" modifier. The problem with this method is that the "super"
;;; modifier has to be enabled, and any shortcuts added by the
;;; operating system that uses this key has to be changed.
;;;
;;; The other method is a bit more cumbersome to use, but it's pretty
;;; much guaranteed to work everywhere. Simply enable the input mode
;;; using C-\ (‘toggle-input-method’) and choose APL-Z. Once this mode
;;; is enabled, press "." (period) followed by a letter to generate
;;; the corresponding symbol.
;;;
;;; Code:

(require 'cl)
(require 'comint)
(require 'etags)
(require 'gnu-apl-util)
(require 'gnu-apl-network)
(require 'gnu-apl-finnapl)

;;;###autoload
(defgroup gnu-apl nil
  "Major mode for interacting with the GNU APL interpreter."
  :prefix 'gnu-apl
  :group 'languages)

(defcustom gnu-apl-executable "apl"
  "Where the GNU APL implementaion is located."
  :type 'string
  :group 'gnu-apl)

(defcustom gnu-apl-auto-function-editor-popup t
  "Edit function definitions in an Emacs buffer.
If non-nil, the function editor will start automatically when
the function definition command is entered. If nil, the
function editor must be opened manually using the function
‘gnu-apl-edit-function’."
  :type 'boolean
  :group 'gnu-apl)

(defcustom gnu-apl-redefine-function-when-in-use-action 'ask
  "What action to take when trying to save a function that is on the )SI stack.
This parameter controls the behaviour when an attempt is made
to redefine a function which is already on the )SI stack.
Permitted values are:

    error - Signal an error message
    clear - Clear the )SI stack before editing
    ask - Ask the user what action to take"
  :type '(choice (const :tag "error" error)
                 (const :tag "clear" clear)
                 (const :tag "ask" ask))
  :group 'gnu-apl)

(defcustom gnu-apl-show-keymap-on-startup t
  "Choose if the keymap should be automatically displayed.
When non-nil, automatically display the keymap when activating
the GNU APL buffer using the command ‘gnu-apl’. The keyboard help
buffer can also be toggled using the command
‘gnu-apl-show-keyboard’."
  :type 'boolean
  :group 'gnu-apl)

(defcustom gnu-apl-show-apl-welcome t
  "Choose if the GNU APL welcome screen should be displayed.
When non-nil, display the GNU APL welcome screen. When this value
is nil, the apl binary is called with the --silent flag."
  :type 'boolean
  :group 'gnu-apl)

(defcustom gnu-apl-show-tips-on-start t
  "When non-nil, show some help when starting a new APL session."
  :type 'boolean
  :group 'gnu-apl)

(defcustom gnu-apl-native-listener-port 0
  "The port number that the native listener should listen to.
If zero, randomly choose an available port.
If -1, request the use of Unix domain sockets."
  :type 'integer
  :group 'gnu-apl)

(defcustom gnu-apl-gnuplot-program "gnuplot"
  "The name of the gnuplot executable."
  :type 'string
  :group 'gnu-apl)

;;; This parameter is not customisable since there are very few cases
;;; where it would need to be changed.
(defvar gnu-apl-native-communication t
  "Enable the use of the Emacs native library that is part of GNU APL.
This library provides a communications channel that
‘gnu-apl-mode’ can use to communicate with the APL interpreter.
Normally, this value should be set to t, as without it many
functions will not work. If this option is set to t, and the
library fails to load for some reason, the features will be
automatically disabled anyway.")

(defcustom gnu-apl-program-extra-args nil
  "List of strings containing extra commandline arguments to pass
to the apl binary."
  :type '(repeat string)
  :group 'gnu-apl)

(defvar gnu-apl-use-new-native-library t
  "If non-nil, use the new-style native library.
Enabling this option requires the use of at least GNU APL version 1.4
or the latest version from the subversion repository.")

(defcustom gnu-apl-indent-amounts '(0 2 0 2)
  "The amounts by which to indent lines within APL functions.
The ∇s are always flush-left, as are all lines outside of functions."
  :type '(list (integer :tag "Number of spaces after ∇ on header line")
               (integer :tag "Number of spaces before comment line   ")
               (integer :tag "Number of spaces before label          ")
               (integer :tag "Number of space before other lines     "))
  :safe '(lambda (v)
           (and (listp v)
                (= 4 (length v))
                (every #'integerp v)
                (every #'(lambda (n) (>= n 0)) v)))
  :group 'gnu-apl)

(defface gnu-apl-default
  ()
  "Face used for APL buffers"
  :group 'gnu-apl)

(defface gnu-apl-error
  '((((class color))
     :foreground "red"
     :inherit gnu-apl-default)
    (t
     :inherit gnu-apl-default))
  "Face used for error messages in the interactive APL buffer"
  :group 'gnu-apl)

(defface gnu-apl-user-status-text
  '((((class color))
     :foreground "#ff0080"
     :inherit gnu-apl-default)
    (t
     :inherit gnu-apl-default))
  "Face used for user diagnostic messages in the interactive APL buffer"
  :group 'gnu-apl)

(defface gnu-apl-help
  '((t
     :inherit gnu-apl-default))
  "Face used for displaying text in help buffers"
  :group 'gnu-apl)

(defface gnu-apl-kbd-help-screen
  '((t
     :inherit gnu-apl-default))
  "Face used to display the keyboard help popup"
  :group 'gnu-apl)

;;;
;;;  History variables
;;;

(defvar gnu-apl-variables-history nil)

;;;
;;;  Keymap functions
;;;

(require 'gnu-apl-symbols)

(defun gnu-apl--make-key-command-sym (n)
  (intern (concat "insert-sym-apl-" n)))

(macrolet ((make-insert-functions ()
             `(progn
                ,@(mapcar #'(lambda (command)
                              `(defun ,(gnu-apl--make-key-command-sym (car command)) ()
                                 (interactive)
                                 (insert ,(cadr command))))
                          gnu-apl--symbols))))
  (make-insert-functions))

(defun gnu-apl-insert-spc ()
  "Insert a space. This is needed so that one can type a space
character when using the super-prefixed characters."
  (interactive)
  (insert " "))

(defun gnu-apl--make-base-mode-map (prefix)
  (let ((map (make-sparse-keymap)))
    (dolist (command gnu-apl--symbols)
      (let ((key-sequence (caddr command)))
        (dolist (s (if (listp key-sequence) key-sequence (list key-sequence)))
          (define-key map (gnu-apl--kbd (concat prefix s)) (gnu-apl--make-key-command-sym (car command))))))
    (define-key map (kbd (concat prefix "SPC")) 'gnu-apl-insert-spc)
    (define-key map (kbd "C-c C-k") 'gnu-apl-show-keyboard)
    (define-key map (kbd "C-c C-h") 'gnu-apl-show-help-for-symbol)
    (define-key map (kbd "C-c C-a") 'gnu-apl-apropos-symbol)
    (define-key map (kbd "C-M-a") 'gnu-apl-beginning-of-defun)
    (define-key map (kbd "C-M-e") 'gnu-apl-end-of-defun)
    (define-key map (kbd "M-.") 'gnu-apl-find-function-at-point)
    (define-key map (kbd "C-c C-.") 'gnu-apl-trace)
    (define-key map (kbd "C-c C-i") 'gnu-apl-finnapl-list)
    (define-key map [menu-bar gnu-apl] (cons "APL" (make-sparse-keymap "APL")))
    (define-key map [menu-bar gnu-apl toggle-keyboard] '("Toggle keyboard" . gnu-apl-show-keyboard))
    (define-key map [menu-bar gnu-apl show-help-for-symbol] '("Documentation for symbol" . gnu-apl-show-help-for-symbol))
    (define-key map [menu-bar gnu-apl apropos-symbol] '("Search symbols" . gnu-apl-apropos-symbol))
    (define-key map [menu-bar gnu-apl find-symbol-at-point] '("Find symbol at point" . gnu-apl-find-function-at-point))
    (define-key map [menu-bar gnu-apl trace] '("Trace variable" . gnu-apl-trace))
    (define-key map [menu-bar gnu-apl finnapl-list] '("FinnAPL idioms list" . gnu-apl-finnapl-list))
    map))

(defun gnu-apl--make-apl-mode-map ()
  (let ((map (gnu-apl--make-base-mode-map gnu-apl-mode-map-prefix)))
    (define-key map (kbd "C-c C-s") 'gnu-apl-interactive-send-region)
    (define-key map (kbd "C-c C-c") 'gnu-apl-interactive-send-current-function)
    (define-key map (kbd "C-c C-l") 'gnu-apl-interactive-send-buffer)
    (define-key map (kbd "C-c C-z") 'gnu-apl-switch-to-interactive)
    map))

(defun gnu-apl--set-mode-map-prefix (symbol new)
  "Recreate the prefix and the keymap."
  (set-default symbol new)
  (setq gnu-apl-mode-map (gnu-apl--make-apl-mode-map)))

(defcustom gnu-apl-mode-map-prefix "s-"
  "The keymap prefix for ‘gnu-apl-mode-map’ used both to store the new value
using ‘set-create’ and to update ‘gnu-apl-mode-map’ using
  `gnu-apl--make-apl-mode-map'. Kill and re-start your APL buffers to reflect the change."
  :type 'string
  :group 'gnu-apl
  :set 'gnu-apl--set-mode-map-prefix)

(defvar gnu-apl-mode-map (gnu-apl--make-apl-mode-map)
  "The keymap for ‘gnu-apl-mode’.")

(defvar gnu-apl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (loop for s in gnu-apl--symbols
          for char = (second s)
          when char
          do (modify-syntax-entry (aref char 0) "." table))
    (modify-syntax-entry (aref "⍝" 0) "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry (aref "∆" 0) "w" table)
    (modify-syntax-entry (aref "⍙" 0) "w" table)
    (modify-syntax-entry ?\\ "." table)
    table)
  "Syntax table for ‘gnu-apl-mode’.")

(defun gnu-apl--init-mode-common ()
  "Generic initialisation code for all gnu-apl modes."
  (setq-local eldoc-documentation-function 'gnu-apl--eldoc-data)
  (setq-local completion-at-point-functions '(gnu-apl-expand-symbol))
  (setq-local comment-start "⍝")
  (setq-local comment-padding " ")
  (setq-local comment-end "")
  (when (featurep 'company)
    (add-to-list (make-local-variable 'company-backends) 'company-gnu-apl))
  ;; TODO: It's an open question as to whether the below is a good idea
  ;; or if a user should manually set this from the hook
  ;;(setq buffer-face-mode-face 'gnu-apl-default)
  ;;(buffer-face-mode)
  )

(defvar gnu-apl-font-lock-keywords1
  '("⎕[a-zA-Z0-9]+"))

(defvar gnu-apl--apl-symbol-pattern "[a-zA-Z_⍺⍵⍶⍹∆⍙λ][a-zA-Z0-9_⍺⍵⍶⍹∆⍙λ¯]*")

(defvar gnu-apl--function-declaration-patterns
  (let* ((s gnu-apl--apl-symbol-pattern)
         (f (format "\\(?: *\\[ *%s *\\]\\)?" s)))

    ;; Patterns that cover the following variations:
    ;;    FN
    ;;    FN R
    ;;    L FN R
    ;;    (LO FN)
    ;;    (LO FN) R
    ;;    (LO FN RO)
    ;;    (LO FN RO) R
    ;;    L (LO FN) R
    ;;    L (LO FN RO) R
    (cl-labels ((add-assignment-syntax (regexp) (concat (format "\\(?:%s *← *\\)?" s)
                                                        regexp
                                                        " *\\(?:;.*\\)?$")))
      (list (add-assignment-syntax (format "\\(%s\\)" s))
            (add-assignment-syntax (format "\\(?:%s +\\)?\\(%s\\)%s +%s" s s f s))
            (add-assignment-syntax (format "\\(?:%s +\\)?( *%s +\\(%s\\) *)%s *%s" s s s f s))
            (add-assignment-syntax (format "\\(?:%s +\\)?( *%s +\\(%s\\) +%s)%s *%s" s s s s f s)))))
  "List of regexps that matches a function declaration header.
The first parenthised substring is the name of the function.")

(defun gnu-apl--match-function-head (limit)
  (loop for pattern in gnu-apl--function-declaration-patterns
        for result = (search-forward-regexp (format "^∇ *%s" pattern) limit t)
        when result
        return t
        finally (return nil)))

(defun gnu-apl--parse-function-header (string)
  "Parse a function definition string.
Returns the name of the function or nil if the function could not be parsed."
  (let* ((line (gnu-apl--trim-spaces string)))
    (loop for pattern in gnu-apl--function-declaration-patterns
          when (string-match (concat "^" pattern) line)
          return (match-string 1 line))))

;;;###autoload
(define-derived-mode gnu-apl-mode prog-mode "GNU-APL"
  "Major mode for editing GNU APL files."
  :syntax-table gnu-apl-mode-syntax-table
  :group 'gnu-apl
  (use-local-map gnu-apl-mode-map)
  (gnu-apl--init-mode-common)
  (gnu-apl--set-imenu-pattern)
  (setq-local font-lock-defaults '((("⎕[a-zA-Z0-9]+" . font-lock-keyword-face)
                                    ("^[ \t]*[a-zA-Z_∆⍙λ⍺⍵][a-zA-Z0-9_∆⍙λ⍺⍵¯]+:" . font-lock-builtin-face)
                                    (gnu-apl--match-function-head . (1 font-lock-function-name-face)))
                                   nil nil nil))
  (setq-local indent-line-function 'gnu-apl-indent))

(defun gnu-apl--find-largest-backward-match (regex)
  (save-excursion
    (loop with old-pos = nil
          for pos = (save-excursion (search-backward-regexp regex nil t))
          while pos
          do (progn
               (backward-char 1)
               (setq old-pos pos))
          finally (return old-pos))))

;;;
;;;  Indentation support
;;;

(defun gnu-apl--full-function-definition-p (line &optional error-on-incorrect-format)
  (when (and (plusp (length line))
             (string= (subseq line 0 1) "∇"))
    (let ((parsed (gnu-apl--parse-function-header (subseq line 1))))
      (when (and error-on-incorrect-format
                 (null parsed))
        (user-error "Incorrectly formatted function header"))
      parsed)))

(defun gnu-apl-indent ()
  "Indent a function, controlled by ‘gnu-apl-indent-amounts’.
Anything outside a function definition is not indented."
  (let ((was-at-first-col (save-excursion
                            (let ((p (point)))
                              (beginning-of-line)
                              (let ((b (point)))
                                (re-search-forward "\\=[ \t]*" nil t)
                                (and (<= b p)
                                     (<= p (point))))))))
    (save-excursion
      (beginning-of-line)
      (re-search-forward "\\=[ \t]*" nil t)
      (let ((indent-amount (destructuring-bind (i-header i-comment i-label i-other)
                               gnu-apl-indent-amounts
                             (cond ((looking-at "∇")
                                    0)
                                   ((looking-at (format "%s:" gnu-apl--apl-symbol-pattern))
                                    i-label)
                                   (t
                                    (let ((function-start (save-excursion
                                                            (search-backward-regexp
                                                             "^[ \t]*∇[ \t]*[^ \t]" nil t)))
                                          (function-end (save-excursion
                                                          (or (search-forward-regexp "^[ \t]*∇[ \t]*$" nil t)
                                                              (point-max))))
                                          (prev-function-end (save-excursion
                                                               (search-backward-regexp
                                                                "^[ \t]*∇[ \t]*$" nil t))))
                                      (if (and function-start
                                               function-end
                                               (or (not prev-function-end)
                                                   (< prev-function-end function-start))
                                               (< function-start function-end))
                                          (if (looking-at "⍝")
                                              i-comment
                                            i-other)
                                        0)))))))
        (when indent-amount
          (save-excursion
            (beginning-of-line)
            (when (re-search-forward "\\=[ \t]*" nil t)
              (unless (eql (current-column) indent-amount)
                (replace-match "")
                (indent-to-column indent-amount)))))))
    (when was-at-first-col
      (re-search-forward "[ \t]*" (point-at-eol) t))
    nil))

;;;
;;;  Support for expansion
;;;

(defun gnu-apl--load-commands (prefix)
  "Return a list of all system commands that start with PREFIX."
  (let ((results (gnu-apl--send-network-command-and-read "systemcommands")))
    (cl-remove-if-not #'(lambda (v)
                          (gnu-apl--string-match-start v prefix))
                      results)))

(defun gnu-apl-expand-symbol ()
  "Implementation of expansion.
This function is designed to be used in ‘completion-at-point-functions’."
  (let* ((row (buffer-substring (save-excursion (beginning-of-line) (point)) (point))))
    ;; Check for system commands
    (if (string-match "^[ \t]*\\([])][a-zA-Z0-9]*\\)$" row)
        (let* ((cmdname (match-string 1 row))
               (command-start-index (- (point) (length cmdname))))
          (list command-start-index (point) (gnu-apl--load-commands cmdname)))

      ;; Check for quad-commands
      (let ((svar-pos (gnu-apl--find-largest-backward-match "⎕[a-zA-Z0-9]*\\=")))
        (if svar-pos
            (let* ((svar (buffer-substring svar-pos (point)))
                   (results (gnu-apl--send-network-command-and-read "systemvariables"))
                   (filtered-variables (cl-remove-if-not #'(lambda (v)
                                                             (gnu-apl--string-match-start v svar))
                                                         results)))
              (when filtered-variables
                (list svar-pos (point) filtered-variables)))

          ;; Check for user-defined symbols
          (let ((pos (gnu-apl--find-largest-backward-match "[a-zA-Z_∆⍙][a-zA-Z0-9_∆⍙¯]*\\=")))
            (when pos
              (let* ((s (buffer-substring pos (point)))
                     (results (gnu-apl--send-network-command-and-read "variables"))
                     (filtered-variables (cl-remove-if-not #'(lambda (v)
                                                               (gnu-apl--string-match-start v s))
                                                           results)))
                (when filtered-variables
                  (list pos (point) filtered-variables))))))))))

;;;
;;;  remote help command integration
;;;

(defun gnu-apl--load-help (&optional string)
  "Retrieve the help from GNU APL for a symbol STRING and
convert it to the format same as `gnu-apl--symbol-doc'.
If STRING is nil return help for all symbols"
  (let* ((results (gnu-apl--send-network-command-and-read
                   (if string (concat "help:" string) "help")))
         (entries (mapcar (lambda (x) (car (read-from-string x))) results))
         (uniq-symbols (mapcar #'second
                               (seq-uniq entries
                                         (lambda (x y)
                                           (string= (second x) (second y))))))
         (docs))
    (cl-flet ((cnv (entry)
                   (let ((arity (first entry)))
                     (list (case arity
                             (0 "Niladic function")
                             (1 "Monadic function")
                             (2 "Dyadic function")
                             (-1 "Monadic operator taking one argument")
                             (-2 "Monadic operator taking one or two arguments")
                             (-3 "Dyadic operator taking one argument")
                             (-4 "Dyadic operator taking two arguments")
                             (-5 "Quasi-dyadic operator (outer product)"))
                           (third entry)
                           (fourth entry)
                           (fifth entry)))))
      (dolist (symb uniq-symbols)
        (push
         (list symb
               (mapcar #'cnv
                       (cl-remove-if-not (lambda (x) (string= (second x) symb)) entries)))
         docs)))
    docs))

                               

;;;
;;;  imenu integration
;;;

(defun gnu-apl--set-imenu-pattern ()
  "Set up the imeny expression patterns."
  (setq imenu-generic-expression
        (mapcar #'(lambda (v) (list nil (concat "^∇ *" v) 1))
                gnu-apl--function-declaration-patterns)))

;;;
;;;  Movement
;;;

(defun gnu-apl-beginning-of-defun ()
  "Go beginning of function.
If point is not located whithin a function, go to ‘point-min’."
  (interactive)
  (let* ((positions (mapcan #'(lambda (pattern)
                                       (save-excursion
                                         (if (re-search-backward (concat "^∇ *" pattern) nil t)
                                             (list (point))
                                           nil)))
                                   gnu-apl--function-declaration-patterns))
         (pos (if positions (reduce #'max positions) nil)))
    (if pos
        (progn
          (goto-char pos)
          (beginning-of-line))
      (goto-char (point-min)))))

(defun gnu-apl-end-of-defun ()
  "Go to the end of the function.
If the cursor is not located within a function, go to ‘point-max’."
  (interactive)
  (beginning-of-line)
  (next-line)
  (if (re-search-forward "^[ \t]*∇[ \t]*$" nil t)
      (beginning-of-line)
    (goto-char (point-max))))

;;;
;;;  Company support
;;;

(defun company-gnu-apl (command &optional arg &rest ignored)
  "Backend for for ‘company-mode’ for GNU APL."
  (interactive (list 'interactive))
  (cond ((eq command 'interactive)
         (company-begin-backend 'company-gnu-apl))
        ((or (eq command 'prefix) (eq command 'candidates))
         (let ((result (gnu-apl-expand-symbol)))
           (case command
             (prefix (if result (buffer-substring (first result) (second result)) nil))
             (candidates (third result)))))
        ((eq command 'meta)
         nil)))

(with-eval-after-load 'company
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (or (eq major-mode 'gnu-apl-mode) (eq major-mode 'gnu-apl-interactive-mode))
        (add-to-list (make-local-variable 'company-backends) 'company-gnu-apl)))))

;;;###autoload
(defun gnu-apl (apl-executable)
  "Start the GNU APL interpreter in a buffer.
APL-EXECUTABLE is the path to the apl program (defaults
to ‘gnu-apl-executable’)."
  (interactive (list (when current-prefix-arg
                       (read-file-name "Location of GNU APL Executable: " nil nil t))))
  (let ((buffer (get-buffer-create "*gnu-apl*"))
        (resolved-binary (or apl-executable gnu-apl-executable)))
    (unless resolved-binary
      (user-error "GNU APL Executable was not set"))
    (pop-to-buffer-same-window buffer)
    (unless (comint-check-proc buffer)
      (gnu-apl--cleanup-trace-symbol buffer)
      (when gnu-apl-show-tips-on-start
        (gnu-apl--insert-tips))
      (apply #'make-comint-in-buffer
             "apl" buffer resolved-binary nil
             "--rawCIN" "--emacs" (append (if (and gnu-apl-native-communication gnu-apl-use-new-native-library)
                                              (list "--emacs_arg" (int-to-string gnu-apl-native-listener-port)))
                                          (if (not gnu-apl-show-apl-welcome)
                                              (list "--silent"))
                                          gnu-apl-program-extra-args))
      (setq gnu-apl-current-session buffer)

      (gnu-apl-interactive-mode)
      (set-buffer-process-coding-system 'utf-8 'utf-8)
      (when (and gnu-apl-native-communication (not gnu-apl-use-new-native-library))
        (gnu-apl--send buffer (concat "'" *gnu-apl-network-start* "'"))
        (gnu-apl--send buffer (concat "'" gnu-apl-libemacs-location "' ⎕FX "
                                      "'" *gnu-apl-native-lib* "'"))
        (gnu-apl--send buffer (format "%s[1] %d" *gnu-apl-native-lib* gnu-apl-native-listener-port))
        (gnu-apl--send buffer (concat "'" *gnu-apl-network-end* "'"))))
    (when gnu-apl-show-keymap-on-startup      (run-at-time "0 sec" nil #'(lambda () (gnu-apl-show-keyboard 1))))))

;;;
;;;  Load the other source files
;;;
 
(require 'gnu-apl-input) 
(require 'gnu-apl-interactive)
(require 'gnu-apl-editor)
(require 'gnu-apl-network)
(require 'gnu-apl-spreadsheet)
(require 'gnu-apl-plot)
(require 'gnu-apl-follow)
(require 'gnu-apl-refdocs-bsd-license)
(require 'gnu-apl-documentation)
(require 'gnu-apl-osx-workaround)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.apl\\'" . gnu-apl-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("apl" . gnu-apl-mode))

(with-eval-after-load 'speedbar
  (speedbar-add-supported-extension ".apl"))

(provide 'gnu-apl-mode)
