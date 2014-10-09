;;; elixir-mix.el --- Emacs integration for Elixir's mix
;;
;; Filename: elixir-mix.el
;; Description: Integration of Elixir's building and deployment tool: mix into Emacs.
;; Author: Samuel Tonini
;; Maintainer: Samuel Tonini
;; Created: So Jun  9 10:01:02 2013 (+0200)
;; Version: 20141003.632
;; X-Original-Version: 1.0.0
;; URL: http://github.com/tonini/elixir-mix.el
;; Keywords: elixir, mix, elixir-mix

;; The MIT License (MIT)
;;
;; Copyright (c) Samuel Tonini
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;;   Manual Installation:
;;
;;    (add-to-list 'load-path "~/path/to/elixir-mix.el/")
;;    (require 'elixir-mix)
;;    (global-elixir-mix-mode)
;;
;;   Interesting variables are:
;;
;;       `elixir-mix-command`
;;
;;            Path to the executable <mix> command
;;
;;       `elixir-mix-buffer-name`
;;
;;            Name for the buffer used for mix shell output.
;;
;;   Major commands are:
;;
;;        M-x elixir-mix-new
;;
;;            Create a new Elixir application.
;;
;;        M-x elixir-mix-test
;;
;;            Run the whole Elixir application test suite.
;;
;;        M-x elixir-mix-test-this-buffer
;;
;;            Run the current buffer through <mix test> command.
;;
;;        M-x elixir-mix-test-file
;;
;;            Run a file through <mix test> command.
;;
;;        M-x elixir-mix-test-at-point
;;
;;            Run the test at point.
;;
;;        M-x elixir-mix-compile
;;
;;            Compile the whole Elixir application.
;;
;;        M-x elixir-mix-run
;;
;;            Runs the given expression in the Elixir application context.
;;
;;        M-x elixir-mix-deps-with-prompt
;;
;;            Prompt for mix deps commands.
;;
;;        M-x elixir-mix-local-with-prompt
;;
;;            Prompt for mix local commands.
;;
;;        M-x elixir-mix-local-install
;;
;;            Prompt for mix local.install <path> or <url>.
;;
;;        M-x elixir-mix-local-install-with-path
;;
;;            Runs local.install and prompt for a <path> as argument.
;;
;;        M-x elixir-mix-local-install-with-url
;;
;;            Runs local.install and prompt for a <url> as argument.
;;
;;        M-x elixir-mix-help
;;
;;            Show help output for a specific mix command.
;;
;;        M-x elixir-mix-execute
;;
;;            Run any command in the context of the application.
;;            Just run any command as you like, including arguments
;;            for the specific command.  (example: test --quick)
;;

;;; Code:

(require 'compile)
(require 'ansi-color)

(defcustom elixir-mix-command "mix"
  "The shell command for mix."
  :type 'string
  :group 'elixir-mix)

(defvar elixir-mix-buffer-name "*mix*"
  "Name of the mix output buffer.")

(defvar elixir-mix--elixir-project-root-indicator
  "mix.exs"
  "The file which indicate an elixir project root.")

(defvar elixir-mix--deps-commands
  '("deps" "deps.clean" "deps.compile" "deps.get" "deps.unlock" "deps.unlock")
  "List of all deps.* available commands.")

(defvar elixir-mix--local-commands
  '("local" "local.install" "local.rebar" "local.uninstall")
  "List of all local.* available commands.")

(defvar elixir-mix--compilation-buffer-name nil
  "Used to store compilation name so recompilation works as expected.")
(make-variable-buffer-local 'elixir-mix--compilation-buffer-name)

(defvar elixir-mix--local-install-option-types '("path" "url")
  "List of local.install option types.")

(defvar elixir-mix--compilation-error-link-options
  '(elixir "\\([a-z./_]+\\):\\([0-9]+\\)\\(: warning\\)?" 1 2 nil (3) 1)
  "File link matcher for `compilation-error-regexp-alist-alist' (matches path/to/file:line).")

(defun elixir-mix--compilation-kill-any-orphan-proc ()
  "Ensure any dangling buffer process is killed."
  (let ((orphan-proc (get-buffer-process (buffer-name))))
    (when orphan-proc
      (kill-process orphan-proc))))

(define-compilation-mode elixir-mix-compilation-mode "ElixirMix"
  "Mix compilation mode."
  (progn
    (font-lock-add-keywords nil
                            '(("^Finished in .*$" . font-lock-string-face)
                              ("^ElixirMix.*$" . font-lock-string-face)))
    ;; Set any bound buffer name buffer-locally
    (setq elixir-mix--compilation-buffer-name elixir-mix--compilation-buffer-name)
    (set (make-local-variable 'kill-buffer-hook)
         'elixir-mix--compilation-kill-any-orphan-proc)))

(defun elixir-mix--elixir-project-root ()
  "Finds the root directory of the project.
It walking the directory tree until it finds a elixir project root indicator."
  (let* ((file (file-name-as-directory (expand-file-name default-directory))))
    (locate-dominating-file file elixir-mix--elixir-project-root-indicator)))

(defun elixir-mix--completing-read (prompt cmdlist)
  (completing-read prompt cmdlist nil t nil nil (car cmdlist)))

(defun elixir-mix--build-runner-cmdlist (command)
  "Build the commands list for the runner."
  (remove "" (elixir-mix-flatten
              (list (if (stringp command)
                        (split-string command)
                      command)))))

(defvar elixir-mix--save-buffers-predicate
  (lambda ()
    (not (string= (substring (buffer-name) 0 1) "*"))))

(defun elixir-mix--handle-compilation-once ()
  (remove-hook 'compilation-filter-hook 'elixir-mix--handle-compilation-once t)
  (delete-matching-lines "\\(elixir-mix-compilation\\|ElixirMix started\\|\n\\)" (point-min) (point)))

(defun elixir-mix--handle-compilation ()
  (ansi-color-apply-on-region compilation-filter-start (point)))

(defun elixir-mix-task-runner (name cmdlist)
  "In a buffer identified by NAME, run CMDLIST in `elixir-mix-compilation-mode'.
Returns the compilation buffer."
  (save-some-buffers (not compilation-ask-about-save) elixir-mix--save-buffers-predicate)

  (let* ((elixir-mix--compilation-buffer-name name)
         (compilation-filter-start (point-min)))
    (with-current-buffer
        (compilation-start
         (mapconcat 'shell-quote-argument
                    (append (list elixir-mix-command) cmdlist)
                    " ")
         'elixir-mix-compilation-mode
         (lambda (b) elixir-mix--compilation-buffer-name))
      (setq-local compilation-error-regexp-alist-alist
                  (cons elixir-mix--compilation-error-link-options compilation-error-regexp-alist-alist))
      (setq-local compilation-error-regexp-alist (cons 'elixir compilation-error-regexp-alist))
      (add-hook 'compilation-filter-hook 'elixir-mix--handle-compilation nil t)
      (add-hook 'compilation-filter-hook 'elixir-mix--handle-compilation-once nil t))))

(defun elixir-mix-flatten (alist)
  (cond ((null alist) nil)
        ((atom alist) (list alist))
        (t (append (elixir-mix-flatten (car alist))
                   (elixir-mix-flatten (cdr alist))))))

(defun elixir-mix-new (name)
  "Create a new elixir project with mix."
  (interactive "Gmix new: ")
  (elixir-mix-execute (list "new" (expand-file-name name))))

(defun elixir-mix-test ()
  "Run the whole elixir test suite."
  (interactive)
  (elixir-mix-execute (list "test")))

(defun elixir-mix-test-this-buffer ()
  "Run the current buffer through mix test."
  (interactive)
  (elixir-mix--test-file buffer-file-name))

(defun elixir-mix-test-file (filename)
  "Run <mix test> with the given `filename`"
  (interactive "Fmix test: ")
  (elixir-mix--test-file (expand-file-name filename)))

(defun elixir-mix--test-file (filename)
  (when (not (file-exists-p filename))
    (error "The given file doesn't exists"))
  (elixir-mix-execute (list "test" (expand-file-name filename))))

(defun elixir-mix-test-at-point ()
  "Run the test at point."
  (interactive)
  (let* ((line (line-number-at-pos (point)))
         (file-and-line (format "%s:%s" buffer-file-name line)))
    (elixir-mix-execute (list "test" file-and-line))))

(defun elixir-mix-compile (command)
  "Compile the whole elixir project."
  (interactive "Mmix compile: ")
  (elixir-mix-execute (list "compile" command)))

(defun elixir-mix-run (command)
  "Runs the given file or expression in the context of the application."
  (interactive "Mmix run: ")
  (elixir-mix-execute (list "run" command)))

(defun elixir-mix-deps-with-prompt (command)
  "Prompt for mix deps commands."
  (interactive
   (list (elixir-mix--completing-read "mix deps: " elixir-mix--deps-commands)))
  (elixir-mix-execute (list command)))

(defun elixir-mix-local-with-prompt (command)
  "Prompt for mix local commands."
  (interactive
   (list (elixir-mix--completing-read "mix local: " elixir-mix--local-commands)))
  (if (string= command "local.install")
      (call-interactively 'elixir-mix-local-install)
    (elixir-mix-execute (list command))))

(defun elixir-mix-local-install (path-or-url)
  "Prompt for mix local.install <path> or <url>."
  (interactive
   (list (completing-read "mix local.install FORMAT: "
                          elixir-mix--local-install-option-types
                          nil t nil nil (car elixir-mix--local-install-option-types))))
  (if (string= path-or-url (car elixir-mix--local-install-option-types))
      (call-interactively 'elixir-mix-local-install-with-path)
    (call-interactively 'elixir-mix-local-install-with-url)))

(defun elixir-mix-local-install-with-path (path)
  "Runs local.install and prompt for a <path> as argument."
  (interactive "fmix local.install PATH: ")
  (elixir-mix-execute (list "local.install" path)))

(defun elixir-mix-local-install-with-url (url)
  "Runs local.install and prompt for a <url> as argument."
  (interactive "Mmix local.install URL: ")
  (elixir-mix-execute (list "local.install" url)))

(defun elixir-mix-help (command)
  "Show help output for a specific mix command."
  (interactive "Mmix help: ")
  (elixir-mix-execute (list "help" command)))

(defun elixir-mix--establish-project-root-directory ()
  "Set the default-directory to the Elixir project root."
  (let ((project-root (elixir-mix--elixir-project-root)))
    (if (not project-root)
        (error "Couldn't find any elixir project root")
      (setq default-directory project-root))))

(defun elixir-mix-execute (command)
  "Run a mix command."
  (interactive "Mmix: ")
  (let ((old-directory default-directory))
    (unless (string= (car command) "new")
      (elixir-mix--establish-project-root-directory))
    (elixir-mix-task-runner elixir-mix-buffer-name
                            (elixir-mix--build-runner-cmdlist command))
    (cd old-directory)))

;;;###autoload
(define-minor-mode global-elixir-mix-mode
  "Toggle global-elixir-mix-mode to use elixir's mix build tool within emacs."
  :global t)

(provide 'elixir-mix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elixir-mix.el ends here
