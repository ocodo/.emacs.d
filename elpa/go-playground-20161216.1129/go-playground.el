;;; go-playground.el --- Local Golang playground for short snippets.

;; Copyright (C) 2015-2016  Alexander I.Grafov (axel)

;; Author: Alexander I.Grafov (axel) <grafov@gmail.com>
;; URL: https://github.com/grafov/go-playground
;; Package-Version: 20161216.1129
;; Keywords: tools, golang
;; Package-Requires: ((emacs "24") (go-mode "1.0.0") (gotest "0.40.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Local playground for the Go programs similar to play.golang.org.
;; `M-x go-playground` and type you golang code then make&run it with `C-Return`.

;; Playground works around `go-mode` and requires preconfigured environment
;; for Go language.

;; I recommend you to use `goimports` instead of `gofmt` for automatically make
;; import clauses. It very comfortable especially for experimenting with code
;; in playground.

;; You may push code to play.golang.org with go-mode' function `go-play-buffer`.

;;

;;; Code:

(require 'go-mode)
(require 'gotest)
(require 'compile)
(require 'time-stamp)

(defgroup go-playground nil
  "Options specific to Go Playground."
  :group 'go)

(defcustom go-playground-ask-file-name nil
  "Non-nil means we ask for a name for the snippet.

By default it will be created as snippet.go"
  :type 'boolean
  :group 'go-playground)

(defcustom go-playground-confirm-deletion t
  "Non-nil means you will be asked for confirmation on the snippet deletion with `go-playground-rm'.

By default confirmation required."
  :type 'boolean
  :group 'go-playground)

(defcustom go-playground-basedir "~/go/src/playground"
  "Base directory for playground snippets.  Better to set it under GOPATH."
  :group 'go-playground)

(define-minor-mode go-playground-mode
  "A place for playing with golang code and export it in short snippets."
  :init-value nil
  :lighter ""
  :keymap '(([C-return] . go-playground-exec))
  (setq mode-name "Play(Go)"))

(defun go-playground-snippet-file-name(&optional snippet-name)
  (let ((file-name (cond (snippet-name)
                         (go-playground-ask-file-name
                          (read-string "Go Playground filename: "))
                         ("snippet"))))
    (concat (go-playground-snippet-unique-dir file-name) "/" file-name ".go")))

; obsoleted by go-playground-exec
(defun go-playground-save-and-run ()
  (interactive)  
  (go-playground-exec))
  
(defun go-playground-exec ()
  "Save the buffer then runs Go compiler for executing the code."
  (interactive)
  (save-buffer t)
  (make-local-variable 'compile-command)
  (compile (concat go-command " run *.go")))

;; draft
;; (defun go-playground-print-unused ()
;;   "Uncompleted function in development.  Don't use it."
;;   (interactive)
;;   (save-buffer t)
;;   (let ((snippet-buf (current-buffer)) (compile-buf (compile (go-run-get-program (go-run-arguments)))))
;;     (set-buffer compile-buf)
;;     (looking-at "^.*:[0-9]+: \\([_.a-zA-Z0-9]+\\) declared and not used")
;;     (let ((not-used-var (match-string 0)))
;;       (set-buffer snippet-buf)
;;       (insert not-used-var))))

;;;###autoload
(defun go-playground ()
  "Run playground for Go language in a new buffer."
  (interactive)
  (let ((snippet-file-name (go-playground-snippet-file-name)))
    (switch-to-buffer (create-file-buffer snippet-file-name))
	(go-playground-insert-template-head "snippet of code")
(insert "package main

import (
    \"fmt\"
)

func main() {
    fmt.Println(\"Results:\")

}
")
    (backward-char 3)
    (go-mode)
    (go-playground-mode)
    (set-visited-file-name snippet-file-name t)))

(defun go-playground-insert-template-head (description)
  (insert "// -*- mode:go;mode:go-playground -*-
// " description " @ " (time-stamp-string "%:y-%02m-%02d %02H:%02M:%02S") "

// === Go Playground ===
// Execute the snippet with Ctl-Return
// Remove the snippet completely with its dir and all files M-x `go-playground-rm`

"))

;;;###autoload
(defun go-playground-rm ()  
  "Remove files of the current snippet together with directory of this snippet."
  (interactive)
  (if (string-match-p (file-truename go-playground-basedir) (file-truename (buffer-file-name)))
      (if (or (not go-playground-confirm-deletion)
	       (y-or-n-p (format "Do you want delete whole snippet dir %s? "
				 (file-name-directory (buffer-file-name)))))
		  (progn
			(save-buffer)
			(delete-directory (file-name-directory (buffer-file-name)) t t)
			(kill-buffer)))
	(message "Won't delete this! Because %s is not under the path %s. Remove the snippet manually!"
			 (buffer-file-name) go-playground-basedir)))

;;;###autoload
(defun go-playground-remove-current-snippet ()
    "Obsoleted by `go-playground-rm'."
  (interactive)
  (go-playground-rm))

;;;###autoload
(defun go-playground-download (url)
  "Download a paste from the play.golang.org and insert it in a new local playground buffer.
Tries to look for a URL at point."
  (interactive (list (read-from-minibuffer "Playground URL: " (ffap-url-p (ffap-string-at-point 'url)))))
  (with-current-buffer
      (let ((url-request-method "GET") url-request-data url-request-extra-headers)
        (url-retrieve-synchronously (concat url ".go")))
    (let* ((snippet-file-name (go-playground-snippet-file-name)) (buffer (create-file-buffer snippet-file-name)))
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (copy-to-buffer buffer (point) (point-max))
      (kill-buffer)
      (with-current-buffer buffer
		(goto-char (point-min))
		(go-playground-insert-template-head (concat url " imported"))
		(go-mode)
		(go-playground-mode)
		(set-visited-file-name snippet-file-name t)
        (switch-to-buffer buffer)))))

(defun go-playground-upload ()
  "Upload the current buffer to play.golang.org and return the short URL of the playground."
  (interactive)
  (goto-char (point-min))
  (forward-line)
  (insert (go-play-buffer)))

(defun go-playground-snippet-unique-dir (prefix)
  "Get unique directory under GOPATH/`go-playground-basedir`."
  (let ((dir-name (concat go-playground-basedir "/"
                          (if (and prefix go-playground-ask-file-name) (concat prefix "-"))
                          (time-stamp-string "at-%:y-%02m-%02d-%02H%02M%02S"))))
    (make-directory dir-name t)
    dir-name))

(provide 'go-playground)
;;; go-playground.el ends here
