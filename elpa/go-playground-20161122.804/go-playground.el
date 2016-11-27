;;; go-playground.el --- Local Golang playground for short snippets.

;; Copyright (C) 2015  Alexander I.Grafov (axel)

;; Author: Alexander I.Grafov (axel) <grafov@gmail.com>
;; URL: https://github.com/grafov/go-playground
;; Package-Version: 20161122.804
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

(defcustom go-playground-ask-for-file-name nil
  "Non-nil means we ask for a name for the snippet.

By default it will be created as snippet.go"
  :type 'boolean
  :group 'go-playground)


(define-minor-mode go-playground-mode
  "A place for playing with golang code and export it in short snippets."
  :init-value nil
  :lighter ""
  :keymap '(([C-return] . go-playground-save-and-run))
  (setq mode-name "Play(Go)"))

(defun go-playground-snippet-file-name(&optional snippet-name)
  (let ((file-name (cond (snippet-name)
                         (go-playground-ask-for-file-name
                          (read-string "Go Playground filename: "))
                         ("snippet"))))
    (concat (go-playground-snippet-unique-dir file-name) "/" file-name ".go")))

(defun go-playground-save-and-run ()
  "Run go compiler on a current buffer."
  (interactive)
  (save-buffer t)
  (make-local-variable 'compile-command)
  (compile (concat go-command " run *.go")))

;; draft
(defun go-playground-print-unused ()
  "Uncompleted function in development.  Don't use it."
  (interactive)
  (save-buffer t)
  (let ((snippet-buf (current-buffer)) (compile-buf (compile (go-run-get-program (go-run-arguments)))))
    (set-buffer compile-buf)
    (looking-at "^.*:[0-9]+: \\([_.a-zA-Z0-9]+\\) declared and not used")
    (let ((not-used-var (match-string 0)))
      (set-buffer snippet-buf)
      (insert not-used-var))))

(defun go-playground-send-to-play.golang.org ()
  (interactive)
  (goto-char (point-min))
  (forward-line)
  (insert (go-play-buffer)))

(defgroup go-playground nil
  "Options specific to `go-playground`."
  :group 'go)

(defcustom go-playground-basedir "~/go/src/playground"
  "Base directory for playground snippets.  Better to set it under GOPATH."
  :group 'go-playground)

(defun go-playground ()
  "Run playground for Go language in a new buffer."
  (interactive)
  (let ((snippet-file-name (go-playground-snippet-file-name)))
    (switch-to-buffer (create-file-buffer snippet-file-name))
    (insert "// -*- mode:go;mode:go-playground -*-
// snippet of code @ " (time-stamp-string "%:y-%02m-%02d %02H:%02M:%02S") "
package main

// === Go Playground ===
// Execute the snippet with Ctl-Return
// Remove this snippet completely with M-x `go-playground-rm`

import ()

func main() {

}
")
    (backward-char 3)
    (go-mode)
    (go-playground-mode)
    (set-visited-file-name snippet-file-name t)))

;;;###autoload
(defun go-playground-rm ()  
  "Remove files of the current snippet together with directory of this snippet."
  (interactive)
  (save-buffer)
  (delete-directory (file-name-directory (buffer-file-name)) t t)
  (kill-buffer))

;;;###autoload
(defun go-playground-remove-current-snippet ()
    "Obsoleted by `go-playground-rm'."
  (interactive)
  (go-playground-rm))

(defun go-playground-snippet-unique-dir (prefix)
  "Get unique directory under GOPATH/`go-playground-basedir`."
  (let ((dir-name (concat go-playground-basedir "/"
                          (if (and prefix go-playground-ask-for-file-name) (concat prefix "-"))
                          (time-stamp-string "%:y-%02m-%02d-%02H:%02M:%02S"))))
    (make-directory dir-name t)
    dir-name))

(provide 'go-playground)
;;; go-playground.el ends here
