;;; shut-up-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "shut-up" "shut-up.el" (0 0 0 0))
;;; Generated autoloads from shut-up.el

(autoload 'shut-up "shut-up" "\
Evaluate BODY with silenced output.

While BODY is evaluated, all output is redirected to a buffer,
unless `shut-up-ignore' is non-nil.  This affects:

- `message'
- All functions using `standard-output' (e.g. `print', `princ', etc.)

Inside BODY, the buffer is bound to the lexical variable
`shut-up-sink'.  Additionally provide a lexical function
`shut-up-current-output', which returns the current contents of
`shut-up-sink' when called with no arguments.

Changes to the variable `shut-up-ignore' inside BODY does not
have any affect.

\(fn &rest BODY)" nil t)

(function-put 'shut-up 'lisp-indent-function '0)

(autoload 'shut-up-silence-emacs "shut-up" "\
Silence Emacs.

Change Emacs settings to reduce the output.

WARNING: This function has GLOBAL SIDE-EFFECTS.  You should only
call this function in `noninteractive' sessions." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "shut-up" '("inhibit-message" "shut-up-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; shut-up-autoloads.el ends here
