;;; erefactor-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "erefactor" "erefactor.el" (0 0 0 0))
;;; Generated autoloads from erefactor.el

(autoload 'erefactor-lazy-highlight-turn-on "erefactor" nil nil nil)

(autoload 'erefactor-check-eval-mode "erefactor" "\
Display compiling warnings when \\[eval-last-sexp], \\[eval-defun]


If called interactively, enable Erefactor-Check-Eval mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'erefactor-lint "erefactor" "\
Execuet Elint in new Emacs process." t nil)

(autoload 'erefactor-lint-by-emacsen "erefactor" "\
Execute Elint in new Emacs processes.
See variable `erefactor-lint-emacsen'." t nil)

(autoload 'erefactor-rename-symbol-in-package "erefactor" "\
Rename symbol at point with queries.
This affect to current buffer and requiring modules.

Please remember, this function only works well if
the module have observance of `require'/`provide' system.

\(fn OLD-NAME NEW-NAME)" t nil)

(autoload 'erefactor-rename-symbol-in-buffer "erefactor" "\
Rename symbol at point resolving reference local variable
as long as i can with queries. This affect to current buffer.

\(fn OLD-NAME NEW-NAME)" t nil)

(autoload 'erefactor-change-prefix-in-buffer "erefactor" "\
Rename symbol prefix with queries.

OLD-PREFIX: `foo-' -> NEW-PREFIX: `baz-'
`foo-function1' -> `baz-function1'
`foo-variable1' -> `baz-variable1'

\(fn OLD-PREFIX NEW-PREFIX)" t nil)

(autoload 'erefactor-add-current-defun "erefactor" "\
Add current defun form to `load-history'
This is usefull when creating new definition." t nil)

(autoload 'erefactor-eval-current-defun "erefactor" "\
Evaluate current defun and add definition to `load-history'

\(fn &optional EDEBUG-IT)" t nil)

(autoload 'erefactor-highlight-current-symbol "erefactor" "\
Highlight current symbol in this buffer.
Force to dehighlight \\[erefactor-dehighlight-all-symbol]" t nil)

(defvar erefactor-map (let ((map (make-sparse-keymap))) (define-key map "L" 'erefactor-lint-by-emacsen) (define-key map "R" 'erefactor-rename-symbol-in-package) (define-key map "A" 'erefactor-add-current-defun) (define-key map "c" 'erefactor-change-prefix-in-buffer) (define-key map "d" 'erefactor-dehighlight-all-symbol) (define-key map "h" 'erefactor-highlight-current-symbol) (define-key map "l" 'erefactor-lint) (define-key map "r" 'erefactor-rename-symbol-in-buffer) (define-key map "x" 'erefactor-eval-current-defun) (define-key map "?" 'erefactor-flymake-display-errors) map))
(add-hook 'emacs-lisp-mode-hook 'erefactor-lazy-highlight-turn-on)
(add-hook 'lisp-interaction-mode-hook 'erefactor-lazy-highlight-turn-on)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "erefactor" '("erefactor-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; erefactor-autoloads.el ends here
