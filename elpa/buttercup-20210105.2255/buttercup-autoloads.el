;;; buttercup-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "buttercup" "buttercup.el" (0 0 0 0))
;;; Generated autoloads from buttercup.el

(autoload 'buttercup-run-at-point "buttercup" "\
Run the buttercup suite at point." t nil)

(autoload 'buttercup-run-discover "buttercup" "\
Discover and load test files, then run all defined suites.

Takes directories as command line arguments, defaulting to the
current directory." nil nil)

(autoload 'buttercup-run-markdown-buffer "buttercup" "\
Run all test suites defined in MARKDOWN-BUFFERS.
A suite must be defined within a Markdown \"lisp\" code block.
If MARKDOWN-BUFFERS is empty (nil), use the current buffer.

\(fn &rest MARKDOWN-BUFFERS)" t nil)

(autoload 'buttercup-run-markdown "buttercup" "\
Run all test suites defined in Markdown files passed as arguments.
A suite must be defined within a Markdown \"lisp\" code block." nil nil)

(autoload 'buttercup-run-markdown-file "buttercup" "\
Run all test suites defined in Markdown FILE.
A suite must be defined within a Markdown \"lisp\" code block.

\(fn FILE)" t nil)

(autoload 'buttercup-minor-mode "buttercup" "\
Activate buttercup minor mode.

If called interactively, enable Buttercup minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

With buttercup minor mode active the following is activated:

- `describe' and `it' forms are fontified with
  `font-lock-keyword-face'.
- `describe' and `it' forms are available from `imenu' for
  quicker access.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "buttercup" '("after-" "assume" "before-" "buttercup-" "describe" "expect" "spy-" "xdescribe" "xit")))

;;;***

;;;### (autoloads nil nil ("buttercup-compat.el" "buttercup-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; buttercup-autoloads.el ends here
