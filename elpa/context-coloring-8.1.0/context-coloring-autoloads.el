;;; context-coloring-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "context-coloring" "context-coloring.el" (22624
;;;;;;  31801 492032 156000))
;;; Generated autoloads from context-coloring.el

(defvar context-coloring-dispatch-hash-table (make-hash-table :test #'eq) "\
Map dispatch strategy names to their property lists.

A \"dispatch\" is a property list describing a strategy for
coloring a buffer.

Its properties must include one of `:modes' or `:predicate', and
a `:colorizer'.

`:modes' - List of major modes this dispatch is valid for.

`:predicate' - Function that determines if the dispatch is valid
for any given state.

`:colorizer' - Function that parses and colors the buffer.

`:delay' - Delay between buffer update and colorization, to
override `context-coloring-default-delay'.

`:setup' - Arbitrary code to set up this dispatch when
`context-coloring-mode' is enabled.

`:teardown' - Arbitrary code to tear down this dispatch when
`context-coloring-mode' is disabled.

`:async-p' - Hint that code will be colorized asynchronously.
Please call `context-coloring-after-colorize' when colorization
completes.")

(autoload 'context-coloring-mode "context-coloring" "\
Toggle contextual code coloring.
With a prefix argument ARG, enable Context Coloring mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

Context Coloring mode is a buffer-local minor mode.  When
enabled, code is colored by scope.  Scopes are colored
hierarchically.  Variables referenced from nested scopes retain
the color of their defining scopes.  Certain syntax, like
comments and strings, is still colored with `font-lock'.

The entire buffer is colored initially.  Changes to the buffer
trigger recoloring.

Define your own colors by customizing faces like
`context-coloring-level-N-face', where N is a number starting
from 0.  If no face is found on a custom theme nor the `user'
theme, the defaults are used.

New language / major mode support can be added with
`context-coloring-define-dispatch', which see.

Feature inspired by Douglas Crockford.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "context-coloring-emacs-lisp" "context-coloring-emacs-lisp.el"
;;;;;;  (22624 31801 504032 156000))
;;; Generated autoloads from context-coloring-emacs-lisp.el

(autoload 'context-coloring-elisp-colorize "context-coloring-emacs-lisp" "\
Color the current Emacs Lisp buffer.

\(fn)" t nil)

(puthash 'emacs-lisp (list :modes '(emacs-lisp-mode lisp-interaction-mode) :colorizer #'context-coloring-elisp-colorize :setup #'context-coloring-setup-idle-change-detection :teardown #'context-coloring-teardown-idle-change-detection) context-coloring-dispatch-hash-table)

(autoload 'context-coloring-eval-expression-colorize "context-coloring-emacs-lisp" "\
Color the `eval-expression' minibuffer prompt as elisp.

\(fn)" t nil)

(autoload 'context-coloring-eval-expression-predicate "context-coloring-emacs-lisp" "\
Non-nil if the minibuffer is for `eval-expression'.

\(fn)" nil nil)

(puthash 'eval-expression (list :predicate #'context-coloring-eval-expression-predicate :colorizer #'context-coloring-eval-expression-colorize :setup #'context-coloring-setup-idle-change-detection :teardown #'context-coloring-teardown-idle-change-detection) context-coloring-dispatch-hash-table)

;;;***

;;;### (autoloads nil "context-coloring-javascript" "context-coloring-javascript.el"
;;;;;;  (22624 31801 532032 157000))
;;; Generated autoloads from context-coloring-javascript.el

(autoload 'context-coloring-js2-colorize "context-coloring-javascript" "\
Color the buffer using the `js2-mode'.

\(fn)" nil nil)

(puthash 'javascript (list :modes '(js2-mode js2-jsx-mode) :colorizer #'context-coloring-js2-colorize :setup (lambda nil (add-hook 'js2-post-parse-callbacks #'context-coloring-colorize nil t)) :teardown (lambda nil (remove-hook 'js2-post-parse-callbacks #'context-coloring-colorize t))) context-coloring-dispatch-hash-table)

;;;***

;;;### (autoloads nil nil ("context-coloring-pkg.el") (22624 31801
;;;;;;  508032 156000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; context-coloring-autoloads.el ends here
