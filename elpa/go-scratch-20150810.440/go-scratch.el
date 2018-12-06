;;; go-scratch.el --- *scratch* buffer for Go -*- lexical-binding: t -*-

;; Copyright © 2015 Emanuel Evans
;;
;; Author: Emanuel Evans <mail@emanuel.industries>
;; Version: 0.0.1
;; Package-Version: 20150810.440
;; Package-Requires: ((go-mode "1.3.1") (emacs "24"))
;; Keywords: languages go

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A mixture between the *scratch* buffer and the Go playground.

;;; Code:

(require 'go-mode)

(defgroup go-scratch nil
  "Scratch buffer for go."
  :prefix "go-scratch"
  :group 'languages)

(defcustom go-scratch-timeout 3
  "Timeout length for scratch processes, in seconds."
  :type 'number
  :group 'go-scratch)

(defvar go-scratch-buffer-name "*go-scratch*"
  "The buffer name for the Go scratch buffer.")

(defconst go-scratch-initial-message "// This buffer is for experimenting with Go code.
// Press C-c C-c to format and evaluate the buffer,
// or C-c C-p to send the buffer to the Go playground.
package main

import \"fmt\"

func main() {
	fmt.Println(\"Hello, playground\")
}
")

(defconst go-scratch-outbuf "*go-scratch-out*")

;;;###autoload
(defun go-scratch ()
  "Go to the Go scratch buffer."
  (interactive)
  (pop-to-buffer (go-scratch-find-or-create-buffer)))

(defun go-scratch-find-or-create-buffer ()
  "Find or create the scratch buffer."
  (interactive)
  (or (get-buffer go-scratch-buffer-name)
      (go-scratch-create-buffer)))

(defun go-scratch-create-buffer ()
  "Create a new Go scratch bufer."
  (with-current-buffer (get-buffer-create go-scratch-buffer-name)
    (go-scratch-mode)
    (insert go-scratch-initial-message)
    (current-buffer)))

(defun go-scratch-eval-buffer ()
  "Compile and evaluate the current buffer.

Program stdout will be printed to the message output."
  (interactive)
  (let ((gofmt-show-errors nil))
    (gofmt))

  (let ((tmpfile (make-temp-file "go-scratch" nil ".go"))
        (outbuf (get-buffer-create go-scratch-outbuf)))
    (write-region nil nil tmpfile nil 'quiet)
    (with-current-buffer outbuf
      (erase-buffer))

    (let ((proc (start-process "go-scratch" outbuf go-command "run" tmpfile)))
      (set-process-sentinel proc #'go-scratch--run-sentinal)
      (run-at-time go-scratch-timeout nil
                   (lambda ()
                     (when (eq (process-status proc) 'run)
                       (kill-process proc)
                       (message "Go scratch process timed out.")))))))

(defun go-scratch--run-sentinal (proc _)
  "Handle process change for go run process PROC."
  (when (eq (process-status proc) 'exit)
    (let ((success (zerop (process-exit-status proc))))
      (with-current-buffer (get-buffer go-scratch-outbuf)
        ;; Trim extra newline
        (goto-char (- (point-max) 1))
        (when (looking-at-p "\n")
          (delete-char 1))

        (if success
            (message "%s" (buffer-string))
          (message "Compilation failed: %s" (buffer-string)))))))

(defvar go-scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map go-mode-map)
    (define-key map (kbd "C-c C-c") #'go-scratch-eval-buffer)
    (define-key map (kbd "C-c C-p") #'go-play-buffer)
    map))

(define-derived-mode go-scratch-mode go-mode "Go scratch interaction"
  "Major mode for interacting with the Go scratch buffer.
Like go-mode except that \\[go-scratch-eval-buffer] formats and
evals the buffer, printing the results.

\\{go-scratch-mode-map}")

(provide 'go-scratch)

;;; go-scratch.el ends here
