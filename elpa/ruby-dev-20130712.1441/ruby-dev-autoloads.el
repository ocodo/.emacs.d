;;; ruby-dev-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ruby-dev-stop-process ruby-dev-restart-process
;;;;;;  ruby-dev-start-maybe ruby-dev-connect ruby-dev ruby-dev-faces
;;;;;;  ruby-dev) "ruby-dev-core" "ruby-dev-core.el" (20964 56621
;;;;;;  0 0))
;;; Generated autoloads from ruby-dev-core.el

(let ((loads (get 'ruby-dev 'custom-loads))) (if (member '"ruby-dev-core" loads) nil (put 'ruby-dev 'custom-loads (cons '"ruby-dev-core" loads))))

(let ((loads (get 'ruby-dev-faces 'custom-loads))) (if (member '"ruby-dev-core" loads) nil (put 'ruby-dev-faces 'custom-loads (cons '"ruby-dev-core" loads))))

(autoload 'ruby-dev "ruby-dev-core" "\
Starts the shell used for Ruby development

If the process is already running, the user is given the choice to restart it
or to cancel this operation.

\(fn)" t nil)

(autoload 'ruby-dev-connect "ruby-dev-core" "\
Starts the shell used for Ruby development, through TCP.

The advantage of this is that you can run ruby-dev on a remote computer by
running `ruby-dev.rb' with the '--server' argument on the server, setting the
RUBY_DEV_HOST and RUBY_DEV_PORT environment variables to bind at the correct
address.

Another thing thats's made possible is to modify an actual program as it is
running in a main loop, by having the ruby-dev server run in another thread.
Conveniently, you can do that by just loading `ruby-dev.rb' before the actual
script (e.g. with the '-r' command line argument).

If the process is already running, the user is given the choice to restart it
or to cancel this operation.

\(fn HOST PORT)" t nil)

(autoload 'ruby-dev-start-maybe "ruby-dev-core" "\
Like `ruby-dev', but doesn't do anything if the process is running already.

\(fn)" t nil)

(autoload 'ruby-dev-restart-process "ruby-dev-core" "\
Restarts the ruby-dev process.

\(fn)" t nil)

(autoload 'ruby-dev-stop-process "ruby-dev-core" "\
Kills the ruby-dev process (or connection).

\(fn)" t nil)

;;;***

;;;### (autoloads (ruby-dev-doc-mode ruby-dev-show-doc ruby-dev-doc)
;;;;;;  "ruby-dev-doc" "ruby-dev-doc.el" (20964 56621 0 0))
;;; Generated autoloads from ruby-dev-doc.el

(let ((loads (get 'ruby-dev-doc 'custom-loads))) (if (member '"ruby-dev-doc" loads) nil (put 'ruby-dev-doc 'custom-loads (cons '"ruby-dev-doc" loads))))

(autoload 'ruby-dev-show-doc "ruby-dev-doc" "\
Shows the documentation for a given symbol.

If the symbol is not found, an error message is shown instead.

\(fn SYMBOL)" t nil)

(defvar ruby-dev-doc-mode-map (let ((map (make-sparse-keymap))) (define-key map (kbd "TAB") 'forward-button) (define-key map (kbd "e") 'ruby-dev-doc-visit-source) (define-key map (kbd "i") 'ruby-dev-doc-goto-instance-methods) (define-key map (kbd "c") 'ruby-dev-doc-goto-class-methods) (define-key map (kbd "s") 'ruby-dev-doc-goto-source) (define-key map (kbd "m") 'ruby-dev-doc-goto-included-modules) (define-key map (kbd "/") 'ruby-dev-show-doc) map) "\
Key bindings for the `ruby-dev-doc-mode'.")

(autoload 'ruby-dev-doc-mode "ruby-dev-doc" "\
Major mode for viewing Ruby documentation for classes, modules or methods,
as shown by `ruby-dev-show-doc'.

\\{ruby-dev-doc-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (ruby-dev-error-mode) "ruby-dev-error" "ruby-dev-error.el"
;;;;;;  (20964 56621 0 0))
;;; Generated autoloads from ruby-dev-error.el

(defvar ruby-dev-error-mode-map (let ((map (make-sparse-keymap))) (define-key map "TAB" 'forward-button) (define-key map "c" 'delete-window) map) "\
Keymap for `ruby-dev-error-mode'.")

(autoload 'ruby-dev-error-mode "ruby-dev-error" "\
Major mode for viewing Ruby exceptions and jumping through their backtrace, in
the buffer shown by `ruby-dev-show-error'.

Commands:
\\{ruby-dev-error-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (ruby-dev-eval-buffer-and-kill ruby-dev-eval-buffer
;;;;;;  ruby-dev-eval-defun-and-kill ruby-dev-eval-defun ruby-dev-eval-last-sexp-and-kill
;;;;;;  ruby-dev-eval-last-sexp ruby-dev-eval-region-and-kill ruby-dev-eval-region
;;;;;;  ruby-dev-eval-string-and-kill ruby-dev-eval-string) "ruby-dev-eval"
;;;;;;  "ruby-dev-eval.el" (20964 56621 0 0))
;;; Generated autoloads from ruby-dev-eval.el

(autoload 'ruby-dev-eval-string "ruby-dev-eval" "\
Evaluates an arbitrary string of ruby code and writes to messages.

Optionally, you can specify a FILENAME (__eval__ by default) and a LINE number
 (0 by default).

\(fn CODE &optional FILENAME LINE)" t nil)

(autoload 'ruby-dev-eval-string-and-kill "ruby-dev-eval" "\
Evaluates an arbitrary string of ruby code and adds it to the kill ring.

Optionally, you can specify a FILENAME (__eval__ by default) and a LINE number
 (0 by default).

\(fn CODE &optional FILENAME LINE)" t nil)

(autoload 'ruby-dev-eval-region "ruby-dev-eval" "\
Tries to evaluate a region of code.

FILENAME and LINE are normally guessed from the buffer and the location of START,
but they can be specified explicitly.

\(fn START END &optional FILENAME LINE)" t nil)

(autoload 'ruby-dev-eval-region-and-kill "ruby-dev-eval" "\
Tries to evaluate a region of code and adds the result to the kill ring.

FILENAME and LINE are normally guessed from the buffer and the location of START,
but they can be specified explicitly.

\(fn START END &optional FILENAME LINE)" t nil)

(autoload 'ruby-dev-eval-last-sexp "ruby-dev-eval" "\
Evaluates the last 'sexp' in code.

Sexps are found using movement functions from `ruby-mode'.

\(fn &optional FILENAME LINE)" t nil)

(autoload 'ruby-dev-eval-last-sexp-and-kill "ruby-dev-eval" "\
Evaluates the last 'sexp' in code and adds it to the kill ring.

Sexps are found using movement functions from `ruby-mode'.

\(fn &optional FILENAME LINE)" t nil)

(autoload 'ruby-dev-eval-defun "ruby-dev-eval" "\
Evaluates the current top-level expression at point.

This is done using `ruby-beginnning-of-defun' and `ruby-end-of-defun'.

\(fn &optional FILENAME LINE)" t nil)

(autoload 'ruby-dev-eval-defun-and-kill "ruby-dev-eval" "\
Evaluates the current top-level expression at point and adds it to the kill ring.

This is done using `ruby-beginnning-of-defun' and `ruby-end-of-defun'.

\(fn &optional FILENAME LINE)" t nil)

(autoload 'ruby-dev-eval-buffer "ruby-dev-eval" "\
Evaluates the whole buffer.

An explicit FILENAME can be specified, otherwise __eval__ is used.

\(fn &optional FILENAME)" t nil)

(autoload 'ruby-dev-eval-buffer-and-kill "ruby-dev-eval" "\
Evaluates the whole buffer.

An explicit FILENAME can be specified, otherwise __eval__ is used.

\(fn &optional FILENAME)" t nil)

;;;***

;;;### (autoloads (turn-off-ruby-dev turn-on-ruby-dev ruby-dev-mode)
;;;;;;  "ruby-dev-mode" "ruby-dev-mode.el" (20964 56621 0 0))
;;; Generated autoloads from ruby-dev-mode.el

(defvar ruby-dev-mode-map (let ((map (make-sparse-keymap))) (define-key map (kbd "C-x C-e") 'ruby-dev-eval-last-sexp) (define-key map (kbd "C-u C-x C-e") 'ruby-dev-eval-last-sexp-and-kill) (define-key map (kbd "C-c C-e") 'ruby-dev-eval-last-sexp) (define-key map (kbd "C-u C-x C-e") 'ruby-dev-eval-last-sexp-and-kill) (define-key map (kbd "C-c C-c") 'ruby-dev-eval-defun) (define-key map (kbd "C-u C-c C-c") 'ruby-dev-eval-defun-and-kill) (define-key map (kbd "C-M-x") 'ruby-dev-eval-defun) (define-key map (kbd "C-u C-M-x") 'ruby-dev-eval-defun-and-kill) (define-key map (kbd "C-c C-b") 'ruby-dev-eval-buffer) (define-key map (kbd "C-u C-c C-b") 'ruby-dev-eval-buffer-and-kill) (define-key map (kbd "C-c C-r") 'ruby-dev-eval-region) (define-key map (kbd "C-u C-c C-r") 'ruby-dev-eval-region-and-kill) (define-key map (kbd "C-c C-s") 'ruby-dev-eval-string) (define-key map (kbd "C-u C-c C-s") 'ruby-dev-eval-string-and-kill) (define-key map (kbd "C-c C-d") 'ruby-dev-show-doc) (define-key map (kbd "C-c TAB") 'ruby-dev-start-main-repl) (define-key map (kbd "C-c S-TAB") 'ruby-dev-start-repl) map) "\
Keybindings for `ruby-dev-mode'.")

(autoload 'ruby-dev-mode "ruby-dev-mode" "\
Minor mode for live features in ruby-mode.

\\{ruby-dev-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'turn-on-ruby-dev "ruby-dev-mode" "\


\(fn)" t nil)

(autoload 'turn-off-ruby-dev "ruby-dev-mode" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (ruby-dev-repl-mode ruby-dev-start-main-repl ruby-dev-start-repl
;;;;;;  ruby-dev-repl) "ruby-dev-repl" "ruby-dev-repl.el" (20964
;;;;;;  56621 0 0))
;;; Generated autoloads from ruby-dev-repl.el

(let ((loads (get 'ruby-dev-repl 'custom-loads))) (if (member '"ruby-dev-repl" loads) nil (put 'ruby-dev-repl 'custom-loads (cons '"ruby-dev-repl" loads))))

(autoload 'ruby-dev-start-repl "ruby-dev-repl" "\
Starts the REPL.

ID is a unique identifier used for communication with the server.
OBJECT is a ruby expression, used to start pry into.

\(fn ID OBJECT)" t nil)

(autoload 'ruby-dev-start-main-repl "ruby-dev-repl" "\
Starts a top-level REPL with main as its identifier.

If there already is such a REPL, just switch buffer

\(fn)" t nil)

(defvar ruby-dev-repl-mode-map (let ((map (make-sparse-keymap))) (define-key map "" 'ruby-dev-repl-send-input) (define-key map "p" 'ruby-dev-repl-previous) (define-key map "n" 'ruby-dev-repl-next) (define-key map (kbd "<C-up>") 'ruby-dev-repl-previous) (define-key map (kbd "<C-down>") 'ruby-dev-repl-next) map) "\
Key bindings for `ruby-dev-repl-mode'.")

(autoload 'ruby-dev-repl-mode "ruby-dev-repl" "\
Major mode for interacting with a Ruby REPL.

\\{ruby-dev-repl-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("ruby-dev-pkg.el" "ruby-dev-utils.el"
;;;;;;  "ruby-dev.el") (20964 56621 987143 0))

;;;***

(provide 'ruby-dev-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ruby-dev-autoloads.el ends here
