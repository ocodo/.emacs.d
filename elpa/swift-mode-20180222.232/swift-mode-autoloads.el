;;; swift-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "swift-mode" "swift-mode.el" (23278 48026 0
;;;;;;  0))
;;; Generated autoloads from swift-mode.el

(let ((loads (get 'swift 'custom-loads))) (if (member '"swift-mode" loads) nil (put 'swift 'custom-loads (cons '"swift-mode" loads))))

(autoload 'swift-mode "swift-mode" "\
Major mode for editing Swift code.

\\{swift-mode-map}

\(fn)" t nil)
 (add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))

;;;***

;;;### (autoloads nil "swift-mode-beginning-of-defun" "swift-mode-beginning-of-defun.el"
;;;;;;  (23278 48026 0 0))
;;; Generated autoloads from swift-mode-beginning-of-defun.el

(defvar swift-mode:mark-defun-preference 'containing "\
Preference for `swift-mode:mark-defun' for nested declarations.

Suppose the following code with the point located at A:

    func outer() {
      func inner1() {
      }

      // A

      func inner2() {
      }
    }

If `swift-mode:mark-defun-preference' is `containing', `swift-mode:mark-defun'
marks the `outer' function.  Likewise, it marks `inner1' if the preference is
`preceding' and `inner2' if the preference is `following'.")

(custom-autoload 'swift-mode:mark-defun-preference "swift-mode-beginning-of-defun" t)

;;;***

;;;### (autoloads nil "swift-mode-indent" "swift-mode-indent.el"
;;;;;;  (23278 48026 0 0))
;;; Generated autoloads from swift-mode-indent.el

(defvar swift-mode:basic-offset 4 "\
Amount of indentation for block contents.")

(custom-autoload 'swift-mode:basic-offset "swift-mode-indent" t)

(defvar swift-mode:parenthesized-expression-offset 2 "\
Amount of indentation inside parentheses and square brackets.")

(custom-autoload 'swift-mode:parenthesized-expression-offset "swift-mode-indent" t)

(defvar swift-mode:multiline-statement-offset 2 "\
Amount of indentation for continuations of expressions.")

(custom-autoload 'swift-mode:multiline-statement-offset "swift-mode-indent" t)

(defvar swift-mode:switch-case-offset 0 "\
Amount of indentation for case labels in switch statements.")

(custom-autoload 'swift-mode:switch-case-offset "swift-mode-indent" t)

(defvar swift-mode:prepend-asterisk-to-comment-line nil "\
Automatically insert a asterisk to each comment line if non-nil.")

(custom-autoload 'swift-mode:prepend-asterisk-to-comment-line "swift-mode-indent" t)

(defvar swift-mode:insert-space-after-asterisk-in-comment t "\
Automatically insert a space after asterisk in comment if non-nil.")

(custom-autoload 'swift-mode:insert-space-after-asterisk-in-comment "swift-mode-indent" t)

(defvar swift-mode:auto-close-multiline-comment t "\
If non-nil, `indent-new-comment-line' automatically close multiline comment.")

(custom-autoload 'swift-mode:auto-close-multiline-comment "swift-mode-indent" t)

(defvar swift-mode:fix-comment-close t "\
Fix \"* /\" in incomplete multiline comment to \"*/\" if non-nil.")

(custom-autoload 'swift-mode:fix-comment-close "swift-mode-indent" t)

(defvar swift-mode:highlight-anchor nil "\
Highlight anchor point for indentation if non-nil.

Intended for debugging.")

(custom-autoload 'swift-mode:highlight-anchor "swift-mode-indent" t)

;;;***

;;;### (autoloads nil "swift-mode-repl" "swift-mode-repl.el" (23278
;;;;;;  48026 0 0))
;;; Generated autoloads from swift-mode-repl.el

(autoload 'swift-mode:run-repl "swift-mode-repl" "\
Run a Swift REPL process.
This function input and output via buffer `*CMD*' where CMD is replaced with
the CMD given.
If there is a process already running in `*CMD*', switch to that buffer.
CMD is a string or a list, interpreted as a command line. The default value is
`swift-mode:repl-executable'. This function updates the buffer local variable
`swift-mode:repl-executable' with the given CMD unless KEEP-DEFAULT is non-nil,
so it will be used as the default value for the next invocatoin in the current
buffer.
If DONT-SWITCH is non-nil, cursor will stay in current buffer.
If KEEP-DEFAULT is non-nil, the `swift-mode:repl-executable' and the global
variable `swift-mode:repl-buffer' are not updated. The buffer local variable
`swift-mode:repl-buffer' is always updated.
Runs the hook `swift-repl-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)

\(fn CMD &optional DONT-SWITCH KEEP-DEFAULT)" t nil)

(defalias 'run-swift 'swift-mode:run-repl)

(autoload 'swift-mode:send-region "swift-mode-repl" "\
Send the current region to the inferior swift process.
START and END define region within current buffer

\(fn START END)" t nil)

(autoload 'swift-mode:send-buffer "swift-mode-repl" "\
Send the buffer to the Swift REPL process.

\(fn)" t nil)

(autoload 'swift-mode:build-swift-module "swift-mode-repl" "\
Build a Swift module in the PROJECT-DIRECTORY.
If PROJECT-DIRECTORY is nil or omited, it is searched from `default-directory'
or its ancestors.
An list ARGS are appended for builder command line arguments.

\(fn &optional PROJECT-DIRECTORY ARGS)" t nil)

(autoload 'swift-mode:build-ios-app "swift-mode-repl" "\
Build a iOS app in the PROJECT-DIRECTORY.
Build it for iOS simulator device DEVICE-IDENTIFIER.
If PROJECT-DIRECTORY is nil or omited, it is searched from `default-directory'
or its ancestors.
If DEVICE-IDENTIFIER is nil or omited,
the value of `swift-mode:ios-simulator-device-identifier' is used.

\(fn &optional PROJECT-DIRECTORY DEVICE-IDENTIFIER)" t nil)

(autoload 'swift-mode:debug-swift-module "swift-mode-repl" "\
Run debugger on a Swift module in the PROJECT-DIRECTORY.
If PROJECT-DIRECTORY is nil or omited, it is searched from `default-directory'
or its ancestors.

\(fn &optional PROJECT-DIRECTORY)" t nil)

(autoload 'swift-mode:debug-ios-app "swift-mode-repl" "\
Run debugger on an iOS app in the PROJECT-DIRECTORY.
If PROJECT-DIRECTORY is nil or omited, it is searched from `default-directory'
or its ancestors.
DEVICE-IDENTIFIER is the device identifier of the iOS simulator.

\(fn &optional PROJECT-DIRECTORY DEVICE-IDENTIFIER)" t nil)

;;;***

;;;### (autoloads nil nil ("swift-mode-font-lock.el" "swift-mode-lexer.el"
;;;;;;  "swift-mode-pkg.el") (23278 48026 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; swift-mode-autoloads.el ends here
