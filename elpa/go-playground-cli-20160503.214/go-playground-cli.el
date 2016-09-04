;;; go-playground-cli.el --- Go Playground client tool  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016 KOBAYASHI Shigeru

;; Author: KOBAYASHI Shigeru (kosh) <shigeru.kb@gmail.com>
;; Version: 1.2
;; Package-Version: 20160503.214
;; Package-Requires: ((emacs "24") (request "0.2.0") (deferred "0.3.2") (names "20151201.404") (s "1.10.0") (f "0.17.2") (let-alist "1.0.4") (cl-lib "0.5"))
;; Keyword: extensions, tools
;; Created: 2015-10-19
;; License: MIT
;; URL: https://github.com/kosh04/go-playground-cli

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; `go-playground-cli.el' is Go Playground (https://play.golang.org) client tool.
;; You can compile and run a Go program like `go run prog.go`.

;;; Change Log:

;; v1.2  2016-05-03  change compile url to HTTPS
;; v1.1  2015-12-03  rename to go-playground-cli.el
;; v1.0  2015-10-19  initial release

;;; Code:

;; basic
(require 'json)
(require 'url-util)
;; elpa
(eval-when-compile
  (require 'names)
  (require 'let-alist))
(require 'cl-lib)
(require 'request)
(require 'deferred)
(require 's)
(require 'f)
(require 'go-mode nil t)                ; optional

;;;###autoload
(define-namespace go-playground-cli-

(defgroup go-playground-cli ()
  "Go Playground client tool."
  :prefix "go-playground-cli-"
  :group 'tools)

(defcustom compile-url "https://play.golang.org/compile"
  "Endpoint URL for Go Playground compile."
  :group 'go-playground-cli
  :type 'string)

(defvar -buffer-name "*Go Playground*"
  "Output buffer for go playground client.")

(defun -run (code)
  "Send request CODE and display output."
  (request compile-url
           :type "POST"
           :data (url-build-query-string
                  `(("version" 2)
                    ("body" ,code)))
           :parser (lambda ()
                     (decode-coding-region (point-min) (point-max) 'utf-8-unix)
                     (json-read))
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (let-alist data
                         (when (s-present? .Errors)
                           (message "%s" (s-chomp .Errors)) ; `error' cannot handler message
                           (cl-return))
                         (-playback (get-buffer-create -buffer-name) .Events))))))

(defun -playback (output events)
  (cl-labels ((clear (output)
                (with-current-buffer output (erase-buffer))))
    (clear output)
    (display-buffer output)
    (deferred:$
      (deferred:loop events
        (lambda (event)
          (let-alist event
            ;; ^L clears the screen
            (when (s-starts-with? "\x0c" .Message)
              (setf (substring .Message 0 1) "")
              (clear output))
            (princ .Message output)
            (sleep-for 0 (/ .Delay 1000000)))))
      (deferred:nextc it
        (lambda ()
          (with-current-buffer output
            (insert (propertize "Program exited." 'face '(:foreground "red")))))))))

:autoload
(defun run (path)
  "Compile and run go program from PATH."
  (interactive "fGo run (playground): ")
  (cl-assert (f-ext? path "go"))
  (-run (f-read path)))

:autoload
(defun run-current-file ()
  "Compile and run go program from current file."
  (interactive)
  (run buffer-file-name))

:autoload
(with-eval-after-load 'go-mode
  ;; register menu
  (define-key (lookup-key go-mode-map [menu-bar Go Playground]) [Run]
    `("Run" . ,#'run-current-file)))

) ;; end namespace

(provide 'go-playground-cli)

;;; go-playground-cli.el ends here
