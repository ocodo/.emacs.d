;;; heroku.el --- Interface to Heroku apps.

;; Copyright © 2012 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: https://github.com/technomancy/heroku.el
;; Package-Version: 20120629.1813
;; Version: 1.0.0
;; Keywords: convenience, api, database

;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; Enhance Heroku `run' and `pg:psql' commands with Emacs equivalents.

;; Requires `heroku' command-line client to be installed; this simply
;; shells out and wraps it in a nicer interface. See
;; https://toolbelt.herokuapp.com for installation instructions.

;; Install this via M-x package-install-file or from Marmalade:
;; http://marmalade-repo.org

;;; Code:

(require 'sql)
(require 'shell)

;;; heroku pg:psql

(defcustom heroku-sql-program "heroku"
  "Command to start pg:psql by Heroku.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom heroku-sql-login-params `(database)
  "Login parameters needed to connect to Heroku PostgreSQL."
  :type 'sql-login-params
  :group 'SQL)

(defcustom heroku-sql-options '("pg:psql")
  "List of additional options for `heroku-sql-program'."
  :type '(repeat string)
  :group 'SQL)

;;;###autoload
(defun heroku-sql (&optional buffer)
  "Run heroku pg:psql as an inferior process in an SQL buffer.

Enter app name when prompted for `database'."
  (interactive "P")
  (sql-product-interactive 'heroku buffer))

(defun heroku-sql-comint (product options)
  (let ((params options))
    (sql-comint product (if (string= "" sql-database)
                            options
                          (append options (list "-a" sql-database))))))

(add-to-list 'sql-product-alist
             '(heroku :name "Heroku"
                      :sqli-program heroku-sql-program
                      :sqli-login heroku-sql-login-params
                      :sqli-options heroku-sql-options
                      :sqli-comint-func heroku-sql-comint
                      :font-lock sql-mode-postgres-font-lock-keywords
                      :list-all ("\\d+" . "\\dS+")
                      :list-table ("\\d+ %s" . "\\dS+ %s")
                      :completion-object sql-postgres-completion-object
                      :prompt-regexp "^\\w*=[#>] "
                      :prompt-length 5
                      :prompt-cont-regexp "^\\w*[-(][#>] "
                      :input-filter sql-remove-tabs-filter
                      :terminator ("\\(^\\s-*\\\\g$\\|;\\)" . "\\g")))

;;; heroku run

(defvar heroku-app-hist nil
  "History of apps for `heroku-run'.")

(defvar heroku-command-hist '("bash")
  "History of commands for `heroku-run'.")

;;;###autoload
(defun heroku-run (&optional prompt-app)
  "Run a remote command on a given app using `shell'."
  (interactive "P")
  (let* ((app (if (or prompt-app (not (locate-dominating-file "." "Procfile")))
                  (read-from-minibuffer "App: " (car heroku-app-hist)
                                        nil nil 'heroku-app-hist)))
         (cmd (read-from-minibuffer "Command: " (car heroku-command-hist)
                                    nil nil 'heroku-command-hist))
         (explicit-shell-file-name "heroku")
         (explicit-heroku-args (append (list "run" cmd)
                                       (if app (list "-a" app)))))
    (shell (format "*heroku bash: %s*" app))))

(provide 'heroku)
;;; heroku.el ends here
