;;; phpunit.el --- Launch PHP unit tests using phpunit

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;         Eric Hansen <hansen.c.eric@gmail.com>
;;
;; URL: https://github.com/nlamirault/phpunit.el
;; Version: 0.17.1
;; Keywords: tools, php, tests, phpunit

;; Package-Requires: ((s "1.12.0") (f "0.19.0") (pkg-info "0.6") (cl-lib "0.5") (emacs "24.3"))

;;; License:

;; Copyright (C) 2014-2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; Thanks to tox.el(https://github.com/chmouel/tox.el) from Chmouel Boudjnah.

;; To use this code, bind the functions `phpunit-current-test', `phpunit-current-class',
;; and `phpunit-current-project' to convenient keys with something like :

;; (define-key web-mode-map (kbd "C-x t") 'phpunit-current-test)
;; (define-key web-mode-map (kbd "C-x c") 'phpunit-current-class)
;; (define-key web-mode-map (kbd "C-x p") 'phpunit-current-project)

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'tramp)
(require 's)
(require 'f)
(eval-when-compile
  (require 'rx))

(defgroup phpunit nil
  "PHPUnit utility"
  :tag "PHPUnit"
  :prefix "phpunit-"
  :group 'tools
  :group 'php)

(defcustom phpunit-default-program nil
  "PHPUnit command or path to executable file or a function that returns these string."
  :type '(choice (file     :tag "Path to PHPUnit executable file.")
                 (function :tag "A function return PHPUnit executable file path.")
                 (string   :tag "PHPUnit command name. (require command in PATH)")))

(defvar phpunit-program)
(make-obsolete-variable 'phpunit-program 'phpunit-default-program "0.18.0")

(defvar phpunit-arg)
(make-obsolete-variable 'phpunit-arg 'phpunit-args "0.18.0")

(defcustom phpunit-stop-on-error nil
  "Stop execution upon first error."
  :type 'boolean)

(defcustom phpunit-stop-on-failure nil
  "Stop execution upon first error or failure."
  :type 'boolean)

(defcustom phpunit-stop-on-skipped nil
  "Stop execution upon first skipped test."
  :type 'boolean)

(defcustom phpunit-verbose-mode nil
  "Display debugging information during test execution."
  :type 'boolean)

(defcustom phpunit-configuration-file nil
  "The PHPUnit configuration file."
  :type '(choice (file  :tag "Path to phpunit.xml[.dist]")
                 (const :tag "Automatically detect the path of phpunit.xml" nil)))

(defcustom phpunit-bootstrap-file nil
  "The PHPUnit bootstrap file."
  :type '(choice (file  :tag "Path to PHPUnit bootstrap script")
                 (const :tag "Not specify boostrap script" nil)))

(defcustom phpunit-colorize nil
  "Colorize PHPUnit compilation output buffer."
  :type '(choice (const :tag "Do not specific --color argument" nil)
                 (const :tag "--color=auto" "auto")
                 (const :tag "--color=never" "never")
                 (const :tag "--color=always" "always")))

(defcustom phpunit-hide-compilation-buffer-if-all-tests-pass nil
  "Hide the compilation buffer if all tests pass."
  :type 'boolean
  :group 'phpunit)

(defconst php-beginning-of-defun-regexp
  (eval-when-compile
    (rx line-start
        (* (syntax whitespace))
        (* (or "abstract" "final" "private" "protected" "public" "static") (+ (syntax whitespace)))
        (* (syntax whitespace))
        "function"
        (+ (syntax whitespace))
        (? "&")
        (group (+ (or (syntax word) (syntax symbol))))
        (* (syntax whitespace))
        "("))
  "Regular expression for a PHP function.")

(defconst php-beginning-of-class
  (rx line-start
        (* (syntax whitespace))
        (? "final" (syntax whitespace))
        (* (syntax whitespace))
        "class"
        (+ (syntax whitespace))
        (group (+ (or (syntax word) (syntax symbol))))
        (* (syntax whitespace)))
  "Regular expression for a PHP class.")

(defconst php-labelchar-regexp
  "[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]"
  "Valid syntax for a character in a PHP label.")

;;;###autoload
(progn
  (defvar-local phpunit-root-directory nil
    "Directory path to execute PHPUnit.")
  (put 'phpunit-root-directory 'safe-local-variable #'stringp)
  (defvar-local phpunit-args nil
    "Argument to pass to phpunit command.")
  (put 'phpunit-args 'safe-local-variable #'(lambda (v) (or (stringp v) (listp v))))
  (defvar-local phpunit-executable nil
    "PHPUnit command or path to executable file.")
  (put 'phpunit-executable 'safe-local-variable
       #'(lambda (v) (or (null v) (stringp v) (functionp v)))))

(when phpunit-hide-compilation-buffer-if-all-tests-pass
  (add-hook 'compilation-finish-functions 'phpunit--hide-compilation-buffer-if-all-tests-pass))

(defvar phpunit-last-group-cache nil)

;; Commands
;; -----------

(defun phpunit--find-executable (directory)
  "Get PHPUnit executable command in `DIRECTORY'."
  (let ((executable (or phpunit-executable phpunit-default-program)))
    (cond ((stringp executable) executable)
          ((functionp executable) (funcall executable))
          ((and directory
                (file-exists-p (concat directory "vendor/bin/phpunit")))
           (expand-file-name (concat directory "vendor/bin/phpunit")))
          ((executable-find "phpunit") "phpunit")
          (t (error "PHPUnit command/package is not installed")))))

(defun phpunit-get-program (args)
  "Return the command to launch unit test.
`ARGS' corresponds to phpunit command line arguments."
  (let* ((vendor-dir (locate-dominating-file default-directory "vendor"))
         (executable (phpunit--find-executable vendor-dir)))
    (when (file-remote-p default-directory)
      (setq executable
            (tramp-file-name-localname (tramp-dissect-file-name executable))))
    (s-concat (shell-quote-argument executable)
              (when phpunit-args
                (s-concat " " (if (stringp phpunit-args) phpunit-args
                                (s-join " " (mapcar 'shell-quote-argument phpunit-args)))))
              (if phpunit-configuration-file
                  (s-concat " -c " (shell-quote-argument (expand-file-name phpunit-configuration-file)))
                "")
              (if phpunit-bootstrap-file
                  (s-concat " --bootstrap " (shell-quote-argument (expand-file-name phpunit-bootstrap-file)))
                "")
              (when phpunit-colorize
                (format " --colors=%s" phpunit-colorize))
              " "
              args)))

(defun phpunit-get-root-directory (&optional force-real-path)
  "Return the root directory to run tests."
  ;; The function doesn't detect the root directory when used with
  ;; tramp mode. In that case, the phpunit-root-directory variable can
  ;; be set which takes precedence
  (or (and (not force-real-path) phpunit-root-directory)
      (let ((filename (buffer-file-name)) path)
        (cond
         ((null filename) default-directory)
         (phpunit-configuration-file
          (file-truename (locate-dominating-file filename phpunit-configuration-file)))
         (:else
          (cl-loop for file in '("phpunit.xml" "phpunit.xml.dist" ".git" "composer.json")
                   do (setq path (locate-dominating-file filename file))
                   if path return (file-truename path)
                   finally return (file-truename "./")))))))

(defun phpunit-get-current-class ()
  "Return the canonical unit test class name associated with the current class or buffer."
  (save-excursion
    (when (re-search-backward php-beginning-of-class nil t)
      (match-string-no-properties 1))))

(defun phpunit-get-current-test ()
  "Get the name of the current test function"
  (save-excursion
    (when (re-search-backward php-beginning-of-defun-regexp nil t)
      (match-string-no-properties 1))))

(defun phpunit--listing-groups ()
  "Return list of @group.

https://phpunit.de/manual/current/en/appendixes.annotations.html#appendixes.annotations.group"
  (let ((phpunit-output (phpunit--execute "--list-groups")))
    (with-temp-buffer
      (insert phpunit-output)
      (goto-char (point-min))
      (search-forward "Available test group")
      (move-beginning-of-line 1)
      (forward-line)
      (cl-loop
       for line in (s-split "\n" (buffer-substring-no-properties (point) (point-max)))
       if (s-starts-with? " - " line)
       collect (s-chop-prefix " - " line)))))

(defun phpunit--get-last-group (path)
  "Get last group cache by `PATH'."
  (if (null phpunit-last-group-cache)
      nil
    (gethash path phpunit-last-group-cache nil)))

(defun phpunit--put-last-group (group path)
  "Put last group `GROUP' cache by `PATH'."
  (unless phpunit-last-group-cache
    (setq phpunit-last-group-cache (make-hash-table :test 'equal)))
  (puthash path group phpunit-last-group-cache))

(defun phpunit-arguments (args)
  "Append options to `ARGS' by variables."
  (when phpunit-stop-on-error
    (setq args (s-concat args " --stop-on-error")))
  (when phpunit-stop-on-failure
    (setq args (s-concat args " --stop-on-failure")))
  (when phpunit-stop-on-skipped
    (setq args (s-concat args " --stop-on-skipped")))
  (when phpunit-verbose-mode
    (setq args (s-concat args " --verbose")))
  args)

(defun phpunit-get-compile-command (args)
  "Return command string to execute PHPUnit from `ARGS'."
  (if (memq system-type '(windows-nt ms-dos))
      (phpunit-get-program (phpunit-arguments args))
    (let ((column-setting-command (format "stty cols %d" (frame-width)))
	  (command-separator "; ")
	  (phpunit-command (phpunit-get-program (phpunit-arguments args))))
      (concat column-setting-command command-separator phpunit-command))))

(defun phpunit--colorize-compilation-buffer ()
  "Colorize PHPUnit compilation buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(defun phpunit--setup-compilation-buffer ()
  "Setup hooks for PHPUnit compilation buffer."
  (add-hook 'compilation-finish-functions #'phpunit--finish-compilation-buffer)
  (add-hook 'compilation-filter-hook #'phpunit--colorize-compilation-buffer))

(defun phpunit--finish-compilation-buffer (&optional cur-buffer msg)
  "Setup hooks for PHPUnit compilation buffer.
`CUR-BUFFER' and `MSG' are ignore."
  (remove-hook 'compilation-finish-functions #'phpunit--finish-compilation-buffer)
  (remove-hook 'compilation-filter-hook #'phpunit--colorize-compilation-buffer))

(defun phpunit--execute (args)
  "Execute phpunit command with `ARGS'."
  (let ((default-directory (phpunit-get-root-directory)))
    (shell-command-to-string (phpunit-get-program (phpunit-arguments args)))))

(defun phpunit-run (args)
  "Execute phpunit command with `ARGS'."
  (add-to-list 'compilation-error-regexp-alist '("^\\(.+\\.php\\):\\([0-9]+\\)$" 1 2))
  (let ((default-directory (phpunit-get-root-directory))
        (compilation-process-setup-function #'phpunit--setup-compilation-buffer))
    (compile (phpunit-get-compile-command args))))

(defun phpunit--hide-compilation-buffer-if-all-tests-pass (buffer status)
  "Hide the compilation BUFFER if all tests pass.
The STATUS describes how the compilation process finished."
  (with-current-buffer buffer
    (let* ((buffer-string (buffer-substring-no-properties
                           (point-min) (point-max)))
           (buffer-lines (s-lines buffer-string))
           (ok-msg (car (cl-remove-if-not
                         (lambda (x)
                           (and (s-contains? "OK" x)
                                (s-contains? "test" x)
                                (s-contains? "assertion" x)))
                         buffer-lines)))
           (time-msg (car (cl-remove-if-not
                           (lambda (x)
                             (and (s-contains? "Time" x)
                                  (s-contains? "Memory" x)))
                           buffer-lines))))
      (when ok-msg
        (delete-windows-on buffer)
        (message "%s %s" ok-msg time-msg)))))

;; API
;; ----

;;;###autoload
(defun phpunit-set-dir-local-variable (variable)
  "Create project file `.dir-locals.el' and set `VARIABLE' for `phpunit.el'."
  (interactive
   (list (intern (completing-read "Select variable name: "
                                  '(phpunit-executable phpunit-directory)))))
  (add-dir-local-variable nil variable
                          (read-string
                           (cl-case variable
                             ('phpunit-executable "Input a command to run PHPUnit: ")
                             ('phpunit-root-directory "Input a directory path to execute PHPUnit: ")))))

;;;###autoload
(defun phpunit-current-test ()
  "Launch PHPUnit on curent test."
  (interactive)
  (let ((args (s-concat " --filter '"
			(phpunit-get-current-class)
			"::"
			(phpunit-get-current-test) "'"
                        " "
                        (s-chop-prefix (phpunit-get-root-directory) buffer-file-name))))
    (phpunit-run args)))


;;;###autoload
(defun phpunit-current-class ()
  "Launch PHPUnit on current class."
  (interactive)
  (phpunit-run (s-chop-prefix (phpunit-get-root-directory t) buffer-file-name)))

;;;###autoload
(defun phpunit-current-project ()
  "Launch PHPUnit on current project."
  (interactive)
  (phpunit-run ""))

;;;###autoload
(defun phpunit-group (use-last-group &optional group)
  "Launch PHPUnit for group."
  (interactive "p")
  (let* ((current-root-directory (phpunit-get-root-directory))
         (last-group (phpunit--get-last-group current-root-directory)))
    (when (called-interactively-p 'interactive)
      (setq use-last-group (eq use-last-group 1))
      (setq group (if (and use-last-group last-group)
                      last-group
                    (completing-read "PHPUnit @group: " (phpunit--listing-groups)))))
    (phpunit-run (format "--group %s" group))
    (phpunit--put-last-group group current-root-directory)))

(provide 'phpunit)
;;; phpunit.el ends here
