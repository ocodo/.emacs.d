;;; apples-mode.el --- Major mode for editing and executing AppleScript code

;; Copyright (C) 2011 tequilasunset

;; Author: tequilasunset <tequilasunset.mac@gmail.com>
;; Keywords: AppleScript, languages
(defconst apples-mode-version "0.0.2"
  "The Current version of `apples-mode'.")

;; This program is free software: you can redistribute it and/or modify
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

;; This program provides a major mode for AppleScript.

;; [INSTALL]
;;
;; Put files in your load-path and add the following to your init file.
;;
;;    (autoload 'apples-mode "apples-mode" "Happy AppleScripting!" t)
;;    (autoload 'apples-open-scratch "apples-mode" "Open scratch buffer for AppleScript." t)
;;    (add-to-list 'auto-mode-alist '("\\.\\(applescri\\|sc\\)pt\\'" . apples-mode))
;; or
;;    (require 'apples-mode)
;;    (add-to-list 'auto-mode-alist '("\\.\\(applescri\\|sc\\)pt\\'" . apples-mode))
;;
;; After that you should byte-compile apples-mode.el.
;;
;;    M-x byte-compile-file RET /path/to/apples-mode.el RET
;;
;; During the byte-compilation, you may get some warnings, but you should
;; ignore them.
;;
;; [FEATURES]
;;
;; Commands for the execution have the prefix `apples-run-'. You can see
;; the other features via menu.
;;
;; [CONFIGURATION]
;;
;; You can access to the customize group via menu or using the command
;; `apples-customize-group'.

;;; Tested:

;; GNU Emacs 23.2.1 on Mac OS X 10.6.6
;; AppleScript 2.1.2

;;; Known Bugs:


;;; TODO:


;;; Code:

(require 'cl)
(require 'easymenu)
(require 'newcomment)

(defgroup apples nil
  "Major mode for editing and executing AppleScript code."
  :group 'languages
  :prefix "apples-")


;;; Utilities for internal use

(defconst apples-identifier "\\(?:\\sw\\|\\s_\\)+") ; "[[:alnum:]_]+"

(defmacro apples-define-show-func (name var)
  "Define `apples-show-NAME', which is the command to display VAR."
  `(defun ,(intern (format "apples-show-%s" name)) ()
     (interactive)
     (message "%s" (or ,var ""))))

(apples-define-show-func mode-version apples-mode-version)

(defsubst apples-replace-re-comma->spaces (re)
  "Replace all `,'s with `\\\\s-+' in RE."
  (replace-regexp-in-string "," "\\\\s-+" re))

(defsubst apples-replace-re-space->spaces (re)
  "Replace all ` 's with `\\\\s-+' in RE."
  (replace-regexp-in-string " " "\\\\s-+" re))

;; DB
(defvar apples-plist nil)
(defsubst apples-plist-put (prop val)
  (setq apples-plist (plist-put apples-plist prop val))
  val)
(defsubst apples-plist-get (prop)
  (plist-get apples-plist prop))

;; temp files
(defcustom apples-tmp-dir nil
  "Specify the path of temp dir. If nil, temp dir will be created to
the same directory where apples-mode.el is located."
  :type '(choice directory (const nil))
  :group 'apples)

(defmacro apples-define-tmp-file (name)
  "Define `apples-tmp-NAME'."
  `(progn
     (defvar ,(intern (format "apples-tmp-%s" name)) nil)
     (let ((files (apples-plist-get :tmp-files))
           (file ',(intern (format "apples-tmp-%s" name))))
       (unless (memq file files)
         (apples-plist-put :tmp-files (cons file files)))
       file)))

(defun apples-tmp-files-setup ()
  "Make the apples-tmp-dir and temp files."
  (when (apples-plist-get :tmp-files)
    (let ((dir (expand-file-name
                (or apples-tmp-dir
                    (concat (file-name-directory (locate-library "apples-mode"))
                            "apples-tmp-dir")))))
      (unless (file-directory-p dir)
        (mkdir dir t))
      (loop for file in (apples-plist-get :tmp-files)
            for tmp = (format "%s/%s.applescript" dir file)
            do
            (set file tmp)
            (unless (file-exists-p tmp)
              (with-temp-file tmp))
            finally (apples-plist-put :tmp-files nil)))))


;;; User variables

(defcustom apples-follow-error-position t
  "If non-nil, automatically move to the beginning position
where error has occurred."
  :type 'boolean
  :group 'apples)

(defcustom apples-prefer-coding-system nil
  "Specify the coding-system used for the execution of script."
  :type '(choice symbol (const nil))
  :group 'apples)

(defcustom apples-compile-create-file-flag nil
  "If non-nil, create an output file of compilation without confirmations
if it doesn't exist."
  :type 'boolean
  :group 'apples)

(defcustom apples-decompile-callback 'apples-handle-decompile
  "Function to handle decompiled text via `apples-decompile'.
It is required two arguments SCRIPT and FILENAME.
SCRIPT is decompiled script. FILENAME is decompiled file name."
  :type 'function
  :group 'apples)

(defcustom apples-decompile-query nil
  "If valid char is specified, handle the output of decompiling
with specified one. Acceptable chars are as following:\n
?o - overwrite file by decompiled script
?i - insert decompiled script at point
?c - copy decompiled script in clipboard.\n
If you set `apples-decompile-callback' to other value, this is ignored.
Because this variable is used in `apples-handle-decompile'."
  :type '(choice character (const nil))
  :group 'apples)

(defcustom apples-continuation-char ?\x00AC
  "Continuation character (not sign in mathematics)."
  :type 'character
  :group 'apples)

(defcustom apples-indent-offset 4
  "Amount of offset per level of indentation for AppleScript."
  :type 'integer
  :group 'apples)

(defcustom apples-continuation-offset apples-indent-offset
  "Extra offset for the lines whose previous line is terminated with
the continuation character. See also `apples-continuation-char'."
  :type 'integer
  :group 'apples)

(defcustom apples-indenters
  '("considering" "else" "if" "ignoring" "on" "repeat" "tell" "try")
  "Leading words of previous line which invoke the indentation of
current line.\n
See also `apples-deindenters', `apples-indent-regexps' and
`apples-noindent-regexps'."
  :type '(repeat string)
  :group 'apples)

(defcustom apples-deindenters
  '("else" "end")
  "Leading words of current line which invoke the deindentation.\n
See also `apples-indenters', `apples-indent-regexps' and
 `apples-noindent-regexps'."
  :type '(repeat string)
  :group 'apples)

(defcustom apples-indent-regexps
  (mapcar
   (lambda (re) (concat "^" (apples-replace-re-comma->spaces re)))
   '(;; script foo
     "script,\\<"
     "using,terms,from"
     "with,timeout"
     "with,transaction"
     ))
  "Regexps match to previous line, which invoke the indentation of
current line. This variable has priority over `apples-indenters'.\n
See also `apples-deindenters' and `apples-noindent-regexps'."
  :type '(repeat regexp)
  :group 'apples)

(defcustom apples-noindent-regexps
  (mapcar
   (lambda (re) (concat "^" (apples-replace-re-comma->spaces re)))
   '(;; if foo then bar
     "if\\>.+\\<then,\\<"
     ;; tell foo to bar
     "tell\\>.+\\<to,\\<"
     ))
  "Regexps match to previous line, which invoke the no-indentation of
current line. It means that the indentation of current line will
be same as previous line's one. This variable has priority over
`apples-indent-regexps'.\n
See also `apples-indenters' and `apples-deindenters'."
  ;; It is also used in `apples-parse-statement'.
  :type '(repeat regexp)
  :group 'apples)

(defcustom apples-keymap
  '(("<S-tab>"   . apples-toggle-indent)
    ("C-c t r"   . apples-run-region/buffer)
    ("C-c t k"   . apples-compile)
    ("C-c t d"   . apples-decompile)
    ("C-c t 3"   . apples-show-last-result)
    ("C-c t l"   . apples-insert-continuation-char)
    ("C-c t RET" . apples-insert-continuation-char-and-newline)
    ("C-c t o"   . apples-open-dict-index)
    ("C-c t s"   . apples-send-to-applescript-editor)
    ("C-c t e"   . apples-end-completion)
    )
  "Alist of keybindings for `apples-mode'. Each element should be the form
\(KEY . COMMAND). KEY must be a string read by `kbd'.
If the value is nil, nothing will be set."
  :type '(repeat (cons string symbol))  ; Can't set to nil via customize group?
  :group 'apples)

(defcustom apples-underline-syntax-class nil
  "Specify syntax class of `_' (underline).
For example, \"w\" means a word, \"_\" means a symbol.
If nil, treated as a symbol."
  :type '(choice string (const nil))
  :link '(info-link "(elisp)Syntax Class Table")
  :group 'apples)

(defcustom apples-end-completion-hl 'words
  "Just after end completion, highlight 'region, 'words or
nothing (nil). See also `apples-end-completion-hl-duration'."
  :type '(choice symbol (const nil))
  :group 'apples)

(defcustom apples-end-completion-hl-duration 0.3
  "Highlight duration of end competion."
  :type 'float
  :group 'apples)

(defcustom apples-mode-hook nil
  "Hook executed just after called `apples-mode'."
  :type 'hook
  :group 'apples)

(defcustom ac-source-applescript
  '((candidates . (apples-keywords))
    (symbol . "d")
    (cache))
  "Source for keywords of AppleScript. This is an additional source for
`auto-complete-mode'."
  :type 'sexp
  :link '(url-link "http://github.com/m2ym/auto-complete")
  :group 'apples)

;; Faces
(macrolet ((face (name &rest attrs)
                 `(defface ,(intern (format "apples-%s" name))
                    '((t (,@attrs)))
                    ,(subst-char-in-string ?- ?  (format "Face for %s." name))
                    :group 'apples)))
  (face statements :inherit font-lock-keyword-face)
  (face commands :inherit font-lock-keyword-face :italic t)
  (face operators :inherit font-lock-type-face)
  (face labels :inherit font-lock-type-face :italic t)
  (face records :inherit font-lock-builtin-face)
  (face reserved-words :inherit font-lock-keyword-face :italic t)
  (face error :inherit font-lock-warning-face)
  (face standard-folders :inherit font-lock-constant-face)
  (face continuation-char :inherit escape-glyph)
  (face error-highlight :background "DeepPink3")
  (face result-prompt :inherit minibuffer-prompt)
  (face error-prompt :inherit font-lock-warning-face)
  (face end-completion :inherit apples-error-highlight)
  )


;;; Process

(apples-define-show-func last-result (apples-plist-get :last-result))
(apples-define-show-func last-raw-result (apples-plist-get :last-raw-result))

(defmacro apples-set-run-info (&optional pred beg buf)
  "If PRED returns non-nil, record BEG (or 1) and BUF (or current buffer).
Otherwise delete stored info."
  (declare (indent 0))
  `(progn
     (apples-delete-result)             ; Delete last display
     (apples-plist-put :run-info (if ,pred
                                     (cons (or ,beg 1) (or ,buf (current-buffer)))
                                   (cons nil nil)))))

(defun apples-delete-overlay (ov)
  (if (consp ov)
      (mapc #'apples-delete-overlay ov)
    (when (overlayp ov)
      (delete-overlay ov))))

(defun apples-error-overlay-setup ()
  (let ((ov (apples-plist-get :err-ov)))
    (if ov
        (move-overlay ov 1 1)
      (setq ov (make-overlay 1 1))
      (overlay-put ov 'face 'apples-error-highlight)
      (apples-plist-put :err-ov ov))))

(defun apples-delete-result (&rest _)
  (apples-delete-overlay (apples-plist-get :err-ov))
  (remove-hook 'pre-command-hook 'apples-display-result t)
  (remove-hook 'after-change-functions 'apples-delete-result t))

(defun apples-display-result (&optional result)
  (let (message-log-max)
    (message (cond
              ;; first time
              (result
               (add-hook 'pre-command-hook 'apples-display-result nil t)
               (add-hook 'after-change-functions 'apples-delete-result nil t)
               (apples-plist-put :last-result result))
              ;; called from hook
              ((and (eq major-mode 'apples-mode)
                    (eq (cdr (apples-plist-get :run-info))
                        (current-buffer)))
               (apples-plist-get :last-result))))))

(apples-define-tmp-file -1713)
(defun apples-error--1713-workaround (f/s)
  "Avoid AppleScript's error -1713.\n
   execution error: No user interaction allowed. (-1713)"
  ;; Ref: <http://macscripter.net/viewtopic.php?id=26334>
  (unless (file-exists-p f/s)
    (with-temp-file apples-tmp--1713
      (insert f/s))
    (setq f/s apples-tmp--1713))
  (apples-do-applescript
   (format
    "tell application \"AppleScript Runner\" to do script \"%s\"" f/s)))

(defun apples-parse-error (result)
  (destructuring-bind
      (err-ov (actual-beg . err-buf)
              &aux err-beg err-end err-type err-msg err-num unknown)
      (values (apples-plist-get :err-ov) (apples-plist-get :run-info))
    (if (string-match
         "\\([0-9]+\\):\\([0-9]+\\): \\([^:]+:\\) \\(.+\\) (\\(-?[0-9]+\\))$"
         result)
        (progn
          (setq err-beg (string-to-number (match-string 1 result))
                err-end (string-to-number (match-string 2 result))
                err-type (match-string 3 result)
                err-msg (match-string 4 result)
                err-num (string-to-number (match-string 5 result)))
          (when actual-beg
            (setq err-beg (+ err-beg actual-beg)
                  err-end (+ err-end actual-beg))))
      (setq unknown t))
    (values
     unknown
     ;; If get the unknown error, below five values return nil.
     err-beg err-end err-type err-msg err-num
     ;; If executed script is an unopened file or called from the minibuffer,
     ;; err-buf returns nil.
     err-buf err-ov)))

(defun apples-result (result status f/s)
  "Handle the result of AppleScript's execution.
If execution has done successfully, display the result.
If error has occurred, display the error.
In that case, if executed script is same as current buffer or in it,
also highlight the error region and go to the beginning of it if
`apples-follow-error-position' is non-nil."
  (block nil
    (apples-plist-put :last-raw-result result)
    (apples-display-result
     (replace-regexp-in-string
      "%" "%%"                          ; %-sequence => %
      (if (= status 1)
          ;; error
          (multiple-value-bind (unknown beg end type msg num buf ov)
              (apples-parse-error result)
            ;; -1713
            (when (eq num -1713)
              (return (apples-error--1713-workaround f/s)))
            (when (and beg buf)
              ;; highlight and move
              (when apples-follow-error-position
                (switch-to-buffer buf)
                (goto-char beg)
                (deactivate-mark))
              (move-overlay ov beg end buf))
            ;; res
            (if unknown
                result
              (format "%s%s [%s]"
                      (propertize (concat type " ") 'face 'apples-error-prompt)
                      msg
                      (propertize (int-to-string num)
                                  'face 'apples-error-prompt))))
        ;; no error
        (concat (propertize "Result: " 'face 'apples-result-prompt)
                (replace-regexp-in-string
                 "\\\\\"" "\"" (if (string-match "^\"\\(.*\\)\"$" result)
                                   (match-string-no-properties 1 result)
                                 result))))))))

(defsubst apples-proc-live-p (proc)
  "Return non-nil if PROC is still running."
  (and (processp proc)
       (not (eq (process-status proc) 'exit))
       (= (process-exit-status proc) 0)
       t))

(defsubst apples-buffer-string (&optional buffer-or-name)
  "Return contents of a currnet buffer or BUFFER-OR-NAME.
Also delete the entire contents of the buffer."
  (with-current-buffer (or buffer-or-name (current-buffer))
    (if (zerop (buffer-size))
        ""
      (prog1 (buffer-substring-no-properties
              (point-min) (1- (point-max)))
        (erase-buffer)))))

(defsubst apples-proc-failed-p (proc)
  "Return non-nil, if PROC has been failed."
  (= (process-exit-status proc) 1))

(defsubst apples-proc-failed (msg buf)
  "Display BUF's contents or MSG added 'missed message.
After that, delete BUF's contents."
  (let ((str (apples-buffer-string buf)))
    (message (or (and (not (string= str "")) str)
                 (concat msg "missed")))))

(defsubst apples-encode-string (str)
  "Encode STR to `apples-prefer-coding-system' if it is specified."
  (if apples-prefer-coding-system
      (encode-coding-string str apples-prefer-coding-system)
    str))

(defun apples-do-applescript (filename-or-script &optional callback)
  "Execute FILENAME-OR-SCRIPT as AppleScript. CALLBACK is required
three arguments RESULT, EXIT-PROC-STATUS and FILENAME-OR-SCRIPT.
If CALLBACK is omitted, call `apples-result'."
  (lexical-let* ((f/s filename-or-script)
                 (callback callback)
                 (buf (get-buffer-create " *apples-do-applescript*"))
                 (args (if (file-exists-p f/s)
                           `(,f/s)
                         `("-ss" "-e" ,(apples-encode-string f/s))))
                 (old-proc (get-buffer-process buf))
                 (enable (if (apples-proc-live-p old-proc)
                             (when (y-or-n-p "\
apples: Process is still running; kill it? ")
                               (progn
                                 (kill-process old-proc)
                                 t))
                           t)))
    (when enable
      (set-process-sentinel
       (apply #'start-process "apples-do-applescript" buf "osascript" args)
       (lambda (proc _)
         (funcall (or callback 'apples-result)
                  (apples-buffer-string buf)
                  (process-exit-status proc)
                  f/s))))))

;; Compile
(defun apples-compile (&optional filename output)
  "Compile FILENAME into OUTPUT."
  (interactive)
  (labels ((read (file prompt default)
                 (expand-file-name
                  (or file (read-file-name prompt default default)))))
    (lexical-let* ((filename (read filename "File: " buffer-file-name))
                   (output (let ((file (read output "Output: " filename)))
                             (if (file-exists-p file)
                                 file
                               (when (or apples-compile-create-file-flag
                                         (y-or-n-p
                                          (format "apples: %s doesn't exist; make it? "
                                                  file)))
                                 (let ((dir (file-name-directory file)))
                                   (unless (file-directory-p dir)
                                     (mkdir dir t))
                                   (with-temp-file file))
                                 file))))
                   (buf (get-buffer-create " *apples-compile*"))
                   (args `("-o" ,output ,filename))
                   msg)
      (when (every 'file-exists-p `(,filename ,output))
        (setq msg (message "Compiling..."))
        (set-process-sentinel
         (apply #'start-process "apples-compile" buf "osacompile" args)
         (lambda (proc _)
           (if (apples-proc-failed-p proc)
               (apples-proc-failed msg buf)
             (message "%sdone" msg))))))))

;; Decompile
(defun apples-handle-decompile (script filename)
  "Default function to handle decompiled script.
To specify the default query, set `apples-decompile-query'."
  (case (or apples-decompile-query
            (ignore-errors
              (read-char
               (apply #'format
                      "%s%sverwrite file %snsert script %sopy script"
                      (propertize "Select: " 'face 'apples-result-prompt)
                      (mapcar (lambda (s)
                                (propertize (concat "[" (upcase s) "]")
                                            'face '(:weight bold)))
                              '("o" "i" "c"))))))
    (?o (let ((buf (get-file-buffer filename)))
          (if buf
              (with-current-buffer buf
                (erase-buffer)
                (insert script))
            (with-temp-file filename
              (insert script)))))
    (?i (insert script))
    (?c (with-temp-buffer
          (insert script)
          (kill-ring-save (point-min) (point-max))))))

(defun apples-decompile (filename)
  "Decompile FILENAME. See also `apples-decompile-query' and
`apples-decompile-callback'."
  (interactive
   (list (read-file-name "File: " buffer-file-name buffer-file-name)))
  (lexical-let* ((filename (expand-file-name filename))
                 (buf (get-buffer-create " *apples-decompile*"))
                 msg)
    (when (file-exists-p filename)
      (setq msg (message "Decompiling..."))
      (set-process-sentinel
       (start-process "apples-decompile" buf "osadecompile" filename)
       (lambda (proc _)
         (if (apples-proc-failed-p proc)
             (apples-proc-failed msg buf)
           (funcall apples-decompile-callback
                    (apples-buffer-string buf)
                    filename)))))))

;; Send script to AppleScript Editor
(defun apples-quoted-string (str)
  "Convert `\"' to `\\\"', `\\' to `\\\\' in STR."
  (replace-regexp-in-string
   "\"" "\\\\\"" (replace-regexp-in-string "\\\\" "\\\\\\\\" str)))

(apples-define-tmp-file send)
(defun apples-send-to-applescript-editor ()
  "Send region or current buffer to AppleScript Editor and run it."
  (interactive)
  (ignore-errors
    (do-applescript
     (apples-encode-string
      (format
       (mapconcat
        'identity
        '("tell application \"AppleScript Editor\""
          "    activate"
          "    open \"%s\""
          "    tell document \"%s\""
          "        set contents to \"%s\""
          ;; "        save"
          "        execute"
          "    end tell"
          "end tell")
        "\n")
       (expand-file-name apples-tmp-send)
       (file-name-nondirectory apples-tmp-send)
       (apples-quoted-string
        (apply #'buffer-substring-no-properties
               (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))))))))


;;; Commands

;; Run
(defun apples-run-file (&optional filename)
  "Execute FILENAME as AppleScript."
  (interactive "fFile: ")
  (setq filename (expand-file-name filename))
  (when (file-exists-p filename)
    (apples-set-run-info
      (eq (get-file-buffer filename)
          (current-buffer)))
    (apples-do-applescript filename)))

(defun apples-run-buffer (&optional buffer-or-name)
  "Execute a current buffer or BUFFER-OR-NAME as AppleScript."
  (interactive)
  (or buffer-or-name (setq buffer-or-name (current-buffer)))
  (apples-set-run-info
    (eq (or (get-buffer buffer-or-name)
            buffer-or-name)
        (current-buffer)))
  (with-current-buffer buffer-or-name
    (apples-do-applescript (buffer-string))))

(defun apples-run-region (beg end)
  "Execute region as AppleScript."
  (interactive "r")
  (when (/= beg end)
    (apples-set-run-info t beg)
    (apples-do-applescript (buffer-substring beg end))))

(defun apples-run-region/buffer ()
  "Execute region or current buffer as AppleScript."
  (interactive)
  (if (use-region-p)
      (call-interactively 'apples-run-region)
    (apples-run-buffer)))

(defun apples-run-minibuf (script)
  "Read script from minibuffer and execute it as AppleScript."
  (interactive "sScript: ")
  (apples-set-run-info nil)
  (apples-do-applescript script))

;; Open Dictionary index
(defun apples-open-dict-index ()
  "Open dictionary index in AppleScript Editor."
  (interactive)
  (do-applescript
   (mapconcat
    'identity
    '("tell application \"AppleScript Editor\" to activate"
      "tell application \"System Events\""
      "    tell process \"AppleScript Editor\""
      "        key code 31 using {shift down, command down}" ; Command-Shift-O
      "    end tell"
      "end tell")
    "\n")))

;; Insert continuation character
(defsubst apples-continuation-char ()
  "Return the continuation character as a string."
  (char-to-string apples-continuation-char))

(defun apples-insert-continuation-char ()
  "Insert the continuation character."
  (interactive "^")
  (when (re-search-backward "\\s-+\\=" nil t)
    (delete-region (match-beginning 0) (match-end 0)))
  (insert ?  apples-continuation-char))

(defun apples-insert-continuation-char-and-newline ()
  "Insert the continuation character, then add newline."
  (interactive "^")
  (apples-insert-continuation-char)
  (call-interactively (key-binding (kbd "RET"))))

;; Scratch buffer for AppleScript
(apples-define-tmp-file scratch)
(defun apples-save-scratch ()
  "Write the contents of *apples-scratch* to temp file."
  (let ((buf (get-buffer "*apples-scratch*")))
    (when buf
      (with-current-buffer buf
        (write-region (point-min) (point-max) apples-tmp-scratch nil 'quiet)))))

;;;###autoload
(defun apples-open-scratch ()
  "Open scratch buffer for AppleScript."
  (interactive)
  (apples-tmp-files-setup)
  (let ((buf (get-buffer "*apples-scratch*")))
    (pop-to-buffer (get-buffer-create "*apples-scratch*"))
    (unless buf
      (add-hook 'kill-emacs-hook 'apples-save-scratch)
      (add-hook 'kill-buffer-hook 'apples-save-scratch nil t)
      (insert-file-contents apples-tmp-scratch)
      (goto-char (point-max))
      (apples-mode))))

;; Key code
(defconst apples-key-codes
  '((?a . 0) (?b . 11) (?c . 8) (?d . 2) (?e . 14) (?f . 3) (?g . 5) (?h . 4)
    (?i . 34) (?j . 38) (?k . 40) (?l . 37) (?m . 46) (?n . 45) (?o . 31)
    (?p . 35) (?q . 12) (?r . 15) (?s . 1) (?t . 17) (?u . 32) (?v . 9)
    (?w . 13) (?x . 7) (?y . 16) (?z . 6) (?0 . 29) (?1 . 18) (?2 . 19)
    (?3 . 20) (?4 . 21) (?5 . 23) (?6 . 22) (?7 . 26) (?8 . 28) (?9 . 25)
    (f1 . 122) (f2 . 120) (f3 . 99) (f4 . 118) (f5 . 96) (f6 . 97) (f7 . 98)
    (f8 . 100) (f9 . 101) (f10 . 109) (f11 . 103) (f12 . 111) (escape . 53)
    (tab . 48) (?  . 49) (return  . 36) (backspace . 51) (left . 123)
    (right . 124) (down . 125) (up . 126))
  "Index of key codes. Each element has the form (CHAR-OR-SYMBOL . KEY-CODE).")

(defun apples-lookup-key->key-code ()
  "Look up key code of AppleScript from key."
  (interactive)
  (clear-this-command-keys)
  (message (propertize "Key: " 'face 'minibuffer-prompt))
  (let* ((key (read-event))
         (key-code (cdr (assq key apples-key-codes)))
         (key (if (integerp key) (char-to-string key) key)))
    (if key-code
        (message (format "Key: %s  Key code: %d" key key-code))
      (message "Not found %s" key))))

(defun apples-lookup-key-code->key (key-code)
  "Look up key from key code of AppleScript."
  (interactive "nKey code: ")
  (let* ((key (car (rassq key-code apples-key-codes)))
         (key (if (integerp key) (char-to-string key) key)))
    (if key
        (message (format "Key: %s  Key code: %d" key key-code))
      (message "Not found %d" key-code))))

;; Comment
(defun apples-comment-or-uncomment-region (beg end &optional arg)
  "`comment-or-uncomment-region' for `apples-mode'."
  (interactive "*r\nP")
  (let ((comment-style 'indent))
    (comment-or-uncomment-region beg end arg)))

(defun apples-comment-dwim (arg)
  "`comment-dwim' for `apples-mode'."
  (interactive "*P")
  (let ((comment-style 'indent))
    (comment-dwim arg)))


;;; Indentation

(defsubst apples-in-string/comment-p (&optional pos)
  "Return non-nil, if POS is in string or in comment."
  (save-excursion
    (let ((ppss (syntax-ppss pos)))
      (or (nth 3 ppss) (nth 4 ppss)))))

(defsubst apples-in-string-p (&optional pos)
  "Return non-nil, if POS is in string."
  (save-excursion
    (nth 3 (syntax-ppss pos))))

(defsubst apples-ideal-prev-bol ()
  "Return the point of previous bol or nil. Lines like the followings
are skipped.\n
- Empty lines
- Lines filled by whitespaces
- Lines whose bol is in string or in comment
- Comments"
  (save-excursion
    (loop initially (beginning-of-line)
          while (not (bobp))
          do (forward-line -1)
          unless (or (looking-at "\\s-*$")
                     (apples-in-string/comment-p)
                     (let ((face-prop (save-excursion
                                        (skip-chars-forward " \t")
                                        (get-text-property (point) 'face))))
                       (some (lambda (face)
                               (if (listp face-prop)
                                   (memq face face-prop)
                                 (eq face face-prop)))
                             '(font-lock-comment-face
                               font-lock-comment-delimiter-face))))
          return (point))))

(defsubst apples-leading-word-of-line ()
  "Return the leading word of line as a string.
If leading char except whitespaces is not a word, return nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at (concat "\\s-*\\(" apples-identifier "\\)"))
      (buffer-substring-no-properties
       (match-beginning 1) (match-end 1)))))

(defsubst apples-line-string ()
  "Return the contents of current line as a string. Leading and trailing
whitespaces are deleted."
  (let (beg end)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq beg (point))
      (end-of-line)
      (skip-chars-backward " \t")
      (setq end (point)))
    (buffer-substring-no-properties beg end)))

(defsubst apples-string-match (regexps string)
  "Unlike `string-match', first argument has to be a list of REGEXPS."
  (some (lambda (re) (string-match re string)) regexps))

(defun apples-parse-lines ()
  "Parse current and previous lines then return the values."
  (let ((prev-bol (unless (= (point-at-bol) (point-min))
                    (apples-ideal-prev-bol)))
        (cchar-re (concat (apples-continuation-char) "$"))
        prev-indent prev-lword prev-lstr pprev-bol prev-cchar-p pprev-cchar-p)
    (flet ((cchar? (lstr) (string-match cchar-re lstr)))
      (when prev-bol
        (save-excursion
          (goto-char prev-bol)
          (setq prev-indent (current-indentation)
                prev-lstr (apples-line-string)
                prev-lword (when (string-match (concat "^" apples-identifier)
                                               prev-lstr)
                             (match-string-no-properties 0 prev-lstr))
                prev-cchar-p (cchar? prev-lstr))
          ;; Parse previous line again for continuation char.
          (when (setq pprev-bol (apples-ideal-prev-bol))
            (goto-char pprev-bol)
            (setq pprev-cchar-p (cchar? (apples-line-string))))))
      (values
       (current-column) (current-indentation) (apples-leading-word-of-line)
       ;; If prev-bol is nil, belows return nil.
       prev-bol prev-indent prev-lword prev-lstr prev-cchar-p pprev-cchar-p))))

(defun apples-indent-line ()
  "Indent current line according to AppleScript indentation rules."
  (interactive "^")
  (let* ((bol-ppss (save-excursion (syntax-ppss (point-at-bol))))
         (bol-is-in-string (nth 3 bol-ppss))
         (bol-is-in-comment (nth 4 bol-ppss))
         (pos (point))
         indent)
    (unless bol-is-in-string
      (multiple-value-bind
          (cur-col cur-indent cur-lword prev-bol prev-indent
                   prev-lword prev-lstr prev-cchar-p pprev-cchar-p)
          (apples-parse-lines)
        (if bol-is-in-comment
            (setq indent (or prev-indent 0))
          ;; bol is neither in string nor in comment
          (flet ((match? (regs str) (and regs str (apples-string-match regs str)))
                 (member? (str lst) (and str lst (member str lst))))
            (let* ((cchar-indent?   (and prev-cchar-p (not pprev-cchar-p)))
                   (prev-indent?    (match? apples-indent-regexps prev-lstr))
                   (prev-noindent?  (match? apples-noindent-regexps prev-lstr))
                   (cchar-deindent? (and (not prev-cchar-p) pprev-cchar-p))
                   (prev-indenter?  (member? prev-lword apples-indenters))
                   (cur-deindenter?
                    (or (member? cur-lword apples-deindenters)
                        (and (string= cur-lword "on")
                             (string-match "^on\\s-+error"
                                           (apples-line-string))))))
              ;; Calculating...
              (setq indent (- (cond ((and cchar-indent? (not prev-noindent?))
                                     ;; indent by cchar offset
                                     (+ prev-indent apples-continuation-offset))
                                    ((and (or prev-indent? prev-indenter?)
                                          (not prev-noindent?))
                                     ;; indent
                                     (+ prev-indent apples-indent-offset))
                                    (cchar-deindent?
                                     ;; deindent by cchar offset
                                     (- prev-indent apples-continuation-offset))
                                    ;; same as prev
                                    (prev-indent)
                                    ;; no prev
                                    (0))
                              (if cur-deindenter?
                                  ;; deindent
                                  apples-indent-offset
                                ;; noindent
                                0))))))
        ;; Now indent line.
        (indent-line-to (if (natnump indent) indent 0))
        (when (> (- cur-col cur-indent) 0)
          (goto-char (+ pos (- (current-indentation) cur-indent))))))))

(defun apples-toggle-indent ()
  "Toggle indentation."
  (interactive "^")
  (unless (apples-in-string-p (point-at-bol))
    (multiple-value-bind
        (cur-col cur-indent _1 prev? prev-indent _2 _3 prev-cchar-p pprev-cchar-p)
        (apples-parse-lines)
      (let* ((pos (point))
             (offset (if (or prev-cchar-p pprev-cchar-p)
                         apples-continuation-offset
                       apples-indent-offset))
             (indent (cond (prev?
                            (let ((diff (- prev-indent cur-indent)))
                              (cond ((> diff 0) prev-indent)
                                    ((= diff 0) (+ prev-indent offset))
                                    (t (- prev-indent offset)))))
                           ;; no prev
                           ((= cur-indent 0) offset)
                           (0))))
        ;; Now indent line.
        (indent-line-to (if (natnump indent) indent 0))
        (when (> (- cur-col cur-indent) 0)
          (goto-char (+ pos (- (current-indentation) cur-indent))))))))

;; end completion
(defconst apples-statements
  `(,@(loop for word in '("considering" "ignoring" "try" "if"
                          "repeat" "tell" "using terms from")
            collect (cons word word))
    ("with timeout"                  . "timeout"                    )
    ("with transaction"              . "transaction"                )
    ("on adding folder items to"     . "adding folder items to"     )
    ("on closing folder window for"  . "closing folder window for"  )
    ("on moving folder window for"   . "moving folder window for"   )
    ("on opening folder"             . "opening folder"             )
    ("on removing folder items from" . "removing folder items from" )
    )
  "Alist of (BEG-WORD-OF-STATEMENT . END-WORD-OF-STATEMENT)s.")

(defun apples-parse-statement ()
  "Parse the current statement block and return the values
\(BOL-WHERE-STATEMENT-STARTS BEG-WORD-OF-STATEMENT END-WORD-OF-STATEMENT)."
  (destructuring-bind (min count nils &aux bol lstr)
      (values (point-min) 1 (values nil nil nil))
    (if (apples-in-string/comment-p)
        nils
      (catch 'val
        (save-excursion
          (while (/= (point) min)
            (catch 'loop
              (if (null (setq bol (apples-ideal-prev-bol)))
                  (throw 'val nils)
                (goto-char bol)
                (setq lstr (apples-line-string))
                (if (string-match "^end\\>" lstr)
                    (incf count)
                  (loop for (beg . end) in apples-statements
                        when (and (string-match (concat "^" beg "\\>") lstr)
                                  (not (apples-string-match apples-noindent-regexps
                                                            lstr)))
                        do (if (zerop (decf count))
                               (throw 'val (values bol beg end))
                             (throw 'loop nil))
                        finally
                        ;; in case of `on'
                        (when (and (string-match (concat "^on \\("
                                                         apples-identifier
                                                         "\\)")
                                                 lstr)
                                   (setq end (match-string-no-properties 1 lstr))
                                   (not (string= end "error"))
                                   (zerop (decf count)))
                          (throw 'val (values bol
                                              (concat "on " end)
                                              end)))))))))))))

(defun apples-end-completion ()
  "Insert `end + current-statement-name'. If `apples-end-completion-hl' is
specified, also highlight the matching statement."
  (interactive "^")
  (multiple-value-bind (bol bword eword)
      (apples-parse-statement)
    (when eword
      (insert "end " eword)
      (when apples-end-completion-hl
        (apples-end-completion-hl bol bword eword)))))

(defun apples-end-completion-hl (bol bword eword)
  (destructuring-bind ((bov . eov) beg pos)
      (values (apples-plist-get :end-ovs)
              (save-excursion
                (goto-char bol)
                (skip-chars-forward " \t")
                (point))
              (point))
    (case apples-end-completion-hl
      (region (move-overlay bov beg pos))
      (words  (move-overlay bov beg (+ beg (length bword)))
              (move-overlay eov (- pos (length eword) 4) pos)))
    (run-at-time apples-end-completion-hl-duration nil
                 'apples-delete-overlay (list bov eov))))

(defun apples-end-completion-hl-setup ()
  (unless (apples-plist-get :end-ovs)
    (let ((bov (make-overlay 1 1))
          (eov (make-overlay 1 1)))
      (overlay-put bov 'face 'apples-end-completion)
      (overlay-put eov 'face 'apples-end-completion)
      (apples-plist-put :end-ovs (cons bov eov)))))


;;; Font lock

(defconst apples-keywords
  `((reserved-words
     . ("about" "above" "after" "against" "and" "apart from" "around" "as"
        "aside from" "at" "back" "before" "beginning" "behind" "below" "beneath"
        "beside" "between" "but" "by" "considering" "contain" "contains"
        "contains" "continue" "copy" "div" "does" "eighth" "else" "end" "equal"
        "equals" "error" "every" "exit" "false" "fifth" "first" "for" "fourth"
        "from" "front" "get" "given" "global" "if" "ignoring" "in" "instead of"
        "into" "is" "it" "its" "last" "local" "me" "middle" "mod" "my" "ninth"
        "not" "of" "on" "onto" "or" "out of" "over" "prop" "property" "put"
        "ref" "reference" "repeat" "return" "returning" "script" "second" "set"
        "seventh" "since" "sixth" "some" "tell" "tenth" "that" "the" "then"
        "third" "through" "thru" "timeout" "times" "to" "transaction" "true"
        "try" "until" "where" "while" "whose" "with" "without"))
    (statements
     . (,@(mapcar 'car apples-statements)
        "application" "considering application responses" "continue" "else"
        "end" "error" "exit" "ignoring application responses" "on"
        "repeat until" "repeat while" "repeat with" "return" "then"))
    (commands
     . ("ASCII character" "ASCII number" "activate" "AGStart" "beep" "copy"
        "count" "choose application" "choose color" "choose file"
        "choose file name" "choose folder" "choose from list"
        "choose remote application" "choose URL" "clipboard info" "close access"
        "current date" "delay" "display alert" "display dialog"
        "do shell script" "get" "get eof" "get volume settings" "info for"
        "launch" "list disks" "list folder" "load script" "localized string"
        "log" "monitor depth" "max monitor depth" "min monitor depth"
        "mount volume" "new file" "offset" "open for access" "open location"
        "path to" "path to application" "path to folder" "path to resource"
        "random number" "read" "round" "run" "run script" "say"
        "scripting component" "set" "set eof" "set monitor depth"
        "set the clipboard to" "set volume" "start log" "stop log"
        "store script" "system attribute" "system info" "time to GMT"
        "the clipboard" "write"))
    (operators
     . ("&" "*" "+" "-" "/" "<" "<=" "=" ">" ">=" "^" "a ref to" "a ref"
        "a reference to" "a reference" "and" "as" "begin with" "begins with"
        "comes after" "comes before" "contain" "contains" "div"
        "does not come after" "does not come before" "does not contain"
        "does not equal" "doesn't come after" "doesn't come before"
        "doesn't contain" "doesn't equal" "end with" "ends with" "equal to"
        "equals" "greater than or equal to" "greater than or equal"
        "greater than" "in not contained by" "is contained by" "is equal to"
        "is equal" "is greater than or equal to" "is greater than or equal"
        "is greater than" "is in" "is less than or equal to"
        "is less than or equal" "is less than" "is not equal to" "is not equal"
        "is not greater than or equal to" "is not greater than or equal"
        "is not greater than" "is not in" "is not less than or equal to"
        "is not less than or equal" "is not less than" "is not"
        "isn't contained by" "isn't equal to" "isn't equal"
        "isn't greater than or equal to" "isn't greater than or equal"
        "isn't greater than" "isn't less than or equal to"
        "isn't less than or equal" "isn't less than" "isn't"
        "less than or equal to" "less than or equal" "less than" "mod" "not"
        "or" "ref to" "ref" "reference to" "reference" "start with"
        "starts with"))
    (handler-parameter-labels
     . ("about" "above" "against" "apart from" "around" "aside from" "at"
        "below" "beneath" "beside" "between" "by" "for" "from" "given"
        "instead of" "into" "on" "onto" "out of" "over" "since" "thru" "through"
        "under"))

    (standard-folders
     . ,(let ((lst
               ;; Ref: <http://macwiki.sourceforge.jp/wiki/index.php/AppleScript>
               ;;      <http://docs.info.apple.com/jarticle.html?path=AppleScript/2.1/en/as189>
               '(("application support"
                  "Macintosh HD:Library:Application Support:"
                  "/Library/Application Support/")
                 ("applications folder"
                  "Macintosh HD:Applications:"
                  "/Applications/")
                 ("current application"
                  "Macintosh HD:System:Library:CoreServices:AppleScript Runner.app:"
                  "/System/Library/CoreServices/AppleScript Runner.app/")
                 ("current user folder"
                  "Macintosh HD:Users:username:"
                  "/Users/username/")
                 ("desktop"
                  "Macintosh HD:Users:username:Desktop:"
                  "/Users/username/Desktop/")
                 ("desktop pictures folder"
                  "Macintosh HD:Library:Desktop Pictures:"
                  "/Library/Desktop Pictures/")
                 ("documents folder"
                  "Macintosh HD:Users:username:Documents:"
                  "/Users/username/Documents/")
                 ("downloads folder"
                  "Macintosh HD:Users:username:Downloads:"
                  "/Users/username/Downloads/")
                 ("favorites folder"
                  "Macintosh HD:Users:username:Library:Favorites:"
                  "/Users/username/Library/Favorites/")
                 ("Folder Action scripts"
                  "Macintosh HD:Users:username:Library:Scripts:Folder Action Scripts:"
                  "/Users/username/Library/Scripts/Folder Action Scripts/")
                 ("fonts"
                  "Macintosh HD:System:Library:Fonts:"
                  "/System/Library/Fonts/")
                 ("frontmost application"
                  "Macintosh HD:System:Library:CoreServices:Finder.app:"
                  "/System/Library/CoreServices/Finder.app/")
                 ("help folder"
                  "Macintosh HD:Library:Documentation:Help:"
                  "/Library/Documentation/Help/")
                 ("home folder"
                  "Macintosh HD:Users:username:"
                  "/Users/username/")
                 ("keychain folder"
                  "Macintosh HD:Users:username:Library:Keychains:"
                  "/Users/username/Library/Keychains/")
                 ("library folder"
                  "Macintosh HD:Library:"
                  "/Library/")
                 ("modem scripts"
                  "Macintosh HD:System:Library:Modem Scripts:"
                  "/System/Library/Modem Scripts/")
                 ("movies folder"
                  "Macintosh HD:Users:username:Movies:"
                  "/Users/username/Movies/")
                 ("music folder"
                  "Macintosh HD:Users:username:Music:"
                  "/Users/username/Music/")
                 ("pictures folder"
                  "Macintosh HD:Users:username:Pictures:"
                  "/Users/username/Pictures/")
                 ("preferences"
                  "Macintosh HD:Users:username:Library:Preferences:"
                  "/Users/username/Library/Preferences/")
                 ("printer descriptions"
                  "Macintosh HD:System:Library:Printers:PPDs:"
                  "/System/Library/Printers/PPDs/")
                 ("public folder"
                  "Macintosh HD:Users:username:Public:"
                  "/Users/username/Public/")
                 ("scripting additions"
                  "Macintosh HD:System:Library:ScriptingAdditions:"
                  "/System/Library/ScriptingAdditions/")
                 ("scripts folder"
                  "Macintosh HD:Users:username:Library:Scripts:"
                  "/Users/username/Library/Scripts/")
                 ("shared documents folder"
                  "Macintosh HD:Users:Shared:"
                  "/Users/Shared/")
                 ("shared libraries"
                  "Macintosh HD:System:Library:CFMSupport:"
                  "/System/Library/CFMSupport/")
                 ("sites folder"
                  "Macintosh HD:Users:username:Sites:"
                  "/Users/username/Sites/")
                 ("startup disk"
                  "Macintosh HD:"
                  "/")
                 ("system folder"
                  "Macintosh HD:System:"
                  "/System/")
                 ("system preferences"
                  "Macintosh HD:System:Library:PreferencePanes:"
                  "/System/Library/PreferencePanes/")
                 ("temporary items"
                  "Macintosh HD:private:var:folders:foobar:TemporaryItems:"
                  "/private/var/folders/foobar/TemporaryItems/")
                 ("trash"
                  "Macintosh HD:Users:username:.Trash:"
                  "/Users/username/.Trash/")
                 ("users folder"
                  "Macintosh HD:Users:"
                  "/Users/")
                 ("utilities folder"
                  "Macintosh HD:Applications:Utilities:"
                  "/Applications/Utilities/")
                 ("voices"
                  "Macintosh HD:System:Library:Speech:Voices:"
                  "/System/Library/Speech/Voices/")
                 )))
          (loop for (folder path posix) in (nreverse lst)
                collect (propertize folder 'path path 'posix posix))))
    )
  "Keywords of AppleScript. Each element has the form (TYPE . KEYWORDS).")


(defun apples-keywords (&optional type)
  "Return keywords of TYPE. If it is omitted, return all keywords."
  (if type
      (cdr (assq type apples-keywords))
    (apply 'append (mapcar 'cdr apples-keywords))))

(defvar apples-font-lock-keywords
  (let ((i apples-identifier))
    (flet ((kws (type) (apples-replace-re-space->spaces
                        (regexp-opt (apples-keywords type) 'words)))
           (cat (&rest s) (apples-replace-re-comma->spaces (apply #'concat s))))
      `(
        ("\\<error\\>"                          0 'apples-error                )
        (,(kws 'statements)                     1 'apples-statements           )
        (,(cat "^\\s-*\\(?:on\\|to\\),\\(" i "\\)")
         1 font-lock-function-name-face )
        (,(cat "\\<set,\\(" i "\\),to\\>")      1 font-lock-variable-name-face )
        (,(apples-continuation-char)            0 'apples-continuation-char    )
        (,(kws 'standard-folders)               1 'apples-standard-folders     )
        (,(kws 'commands)                       1 'apples-commands             )
        (,(kws 'operators)                      1 'apples-operators            )
        (,(cat ",\\([-&*+/<=>^]\\|<=\\|>=\\),") 0 'apples-operators            )
        (,(cat "\\<" i ":")                     0 'apples-records              )
        (,(kws 'handler-parameter-labels)       1 'apples-labels               )
        (,(kws 'reserved-words)                 1 'apples-reserved-words       )
        ("\\('s\\)\\s-+"                        1 'apples-reserved-words       )
        )))
  "Font lock keywords for `apples-mode'.
See also `font-lock-defaults' and `font-lock-keywords'.")


;;; Misc

(defvar apples-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Define menu.
    (easy-menu-define nil map
      "Menu for `apples-mode'."
      `("AppleScript"
        ["Open Scratch" apples-open-scratch]
        ["Open Dictionary..." apples-open-dict-index]
        ("Edit" :visible (not buffer-read-only)
         ["(Un)comment Region" apples-comment-or-uncomment-region]
         ["Comment Dwim" apples-comment-dwim]
         "---"
         ["Insert Continuation Char" apples-insert-continuation-char]
         ["Insert Continuation Char and Newline"
          apples-insert-continuation-char-and-newline]
         "---"
         ["End Completion" apples-end-completion]
         )
        ("Execution"
         ["Run File" apples-run-file]
         ["Run Buffer" apples-run-buffer]
         ["Run Region" apples-run-region]
         ["Run from Minibuffer" apples-run-minibuf]
         ["Run Region or Buffer" apples-run-region/buffer]
         ["Run Region or Buffer in ASE" apples-send-to-applescript-editor]
         "---"
         ["Compile" apples-compile]
         ["Decompile" apples-decompile]
         "---"
         ["Last Result" apples-show-last-result]
         ["Last Raw Result" apples-show-last-raw-result]
         )
        ("Misc"
         ("Key <=> Key Code"
          ["Key => Key Code" apples-lookup-key->key-code]
          ["Key Code => Key" apples-lookup-key-code->key])
         ("path to..."
          ,@(loop for folder in (nreverse (apples-keywords 'standard-folders))
                  collect (multiple-value-bind (path posix)
                              (with-temp-buffer
                                (insert folder)
                                (let ((pos (point-min)))
                                  (values (get-text-property pos 'path)
                                          (get-text-property pos 'posix))))
                            `(,(capitalize folder)
                              [,folder
                               (insert ,folder)
                               (not buffer-read-only)]
                              [,(concat "PATH: " path)
                               (prin1 ,path (current-buffer))
                               (not buffer-read-only)]
                              [,(concat "POSIX: " posix)
                               (prin1 ,posix (current-buffer))
                               (not buffer-read-only)]))))
         )
        "---"
        ["Customizations" apples-customize-group]
        ["Mode Version" apples-show-mode-version]
        ["AppleScript Version" apples-show-applescript-version]
        ["Visit apples-mode Project" apples-visit-project]
        ))
    map)
  "Keymap used in `apples-mode'.")

(defun apples-keymap-setup ()
  "Set up keybindings for `apples-mode' according to `apples-keymap'."
  (when (and apples-keymap
             (not (apples-plist-get :keybinded?)))
    (loop for (key . cmd) in apples-keymap
          do (define-key apples-mode-map (read-kbd-macro key) cmd)
          finally (apples-plist-put :keybinded? t))))

(defvar apples-mode-syntax-table
  (let ((st (make-syntax-table))
        (lst
         '((?\" "\"")
           (?\\ "\\")
           (?|  "w")
           (?:  "_")
           (?_  "_")
           (?#  "<")
           (?-  ". 12")
           (?\t "    ")
           (?\f "    ")
           (?\n ">    ")
           (?\{ "(}")
           (?\} "){")
           (?\( "() 1b")
           (?\) ")( 4b")
           (?*  ". 23b")
           )))
    (loop for (char entry) in lst
          do (modify-syntax-entry char entry st))
    st)
  "Syntax table used in `apples-mode'.")

(defun apples-applescript-version ()
  "Return AppleScript's version."
  (unless (apples-plist-get :AS-version)
    (let ((ver (with-temp-buffer
                 (when (= (call-process "osascript" nil (current-buffer) t
                                        "-e" "AppleScript's version")
                          0)
                   (apples-buffer-string)))))
      (apples-plist-put :AS-version ver)
      ver)))
(apples-define-show-func applescript-version (apples-plist-get :AS-version))

(defun apples-customize-group ()
  (interactive)
  (customize-group "apples"))

(defun apples-visit-project ()
  (interactive)
  (browse-url "http://github.com/tequilasunset/apples-mode"))

(defvar apples-imenu-generic-expression
  (nreverse
   (mapcar (lambda (pair)
             `(,(car pair)
               ,(concat "^\\s-*" (apples-replace-re-comma->spaces (cdr pair)))
               1))
           '(("Handlers"  . "\\(?:on\\|to\\),\\(.+\\)$" )
             ("Tells"     . "tell,\\(.+\\)$"            )
             ("Variables" . "set,\\(.+\\),to"           )
             )))
  "Imenu index pattern for AppleScript. See also `imenu-generic-expression'.")


;;; Major mode

;;;###autoload
(defun apples-mode ()
  "Happy AppleScripting!"
  (interactive)
  (kill-all-local-variables)
  (apples-applescript-version)
  (apples-tmp-files-setup)
  (apples-error-overlay-setup)
  (apples-end-completion-hl-setup)
  ;; map and table
  (apples-keymap-setup)
  (use-local-map apples-mode-map)
  (set-syntax-table apples-mode-syntax-table)
  (when apples-underline-syntax-class
    (modify-syntax-entry ?_ apples-underline-syntax-class apples-mode-syntax-table))
  ;; local variables
  (setq major-mode 'apples-mode
        mode-name "AppleScript"
        font-lock-defaults '(apples-font-lock-keywords)
        paragraph-separate "[ \t\n\f]*$"
        paragraph-start    "[ \t\n\f]*$"
        comment-start "-- "
        comment-end   ""
        comment-start-skip "\\(?:#\\|---*\\|(\\*\\)+[ \t]*"
        comment-column 40
        indent-line-function 'apples-indent-line
        imenu-generic-expression apples-imenu-generic-expression
        )
  (run-mode-hooks 'apples-mode-hook))

(provide 'apples-mode)
;;; apples-mode.el ends here
