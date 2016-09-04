;;; go-stacktracer.el --- parse Go stack traces

;; Copyright (C) 2014 Samer Masterson

;; Author: Samer Masterson <samer@samertm.com>
;; Keywords: tools
;; Package-Version: 20150430.1442
;; URL: https://github.com/samertm/go-stacktracer.el

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

;; Jump through Go stacktraces as easily as grep-mode.

;; Usage:

;; 1. Hit a panic (`debug.PrintStack()' doesn't work perfectly, see
;; "BUGS" below)

;; 2. Highlight/mark region (you can be sloppy because go-stacktracer is
;; smart, just be sure to capture the entire line).

;; 3. M-x `go-stacktracer-region'

;; 4. Use `n' and `p' to move up and down, RET to jump to a file (just
;; like grep-mode).

;; 5. Find bugs and submit patches! I happily accept all issues/pull
;; requests.

;; Description:

;; When you hit a stacktrace, mark the portion that you want to jump
;; through and then call `go-stacktracer-region' with M-x. The
;; *go-stacktracer* buffer will look something like this:

;; main.go:20: main.AFunc()
;; main.go:15: main.AnotherFunc()
;; main.go:7: main.main()

;; Use "n" and "p" to go down and up (just like grep-mode,
;; *go-stacktracer* is literally a grep-mode buffer).

;; go-stacktracer uses a regexp to capture the file path and line
;; number, so you don't need to be super precise with the lines you
;; call `go-stacktracer-region' on (though you should be sure to
;; capture the entire line).

;; BUGS:

;; `debug.PrintStack()' prints the function name and file path in the
;; opposite order that `panic' does. I don't handle that case right
;; now, so the function names are off-by-one in the view for
;; `debug.PrintStack()' traces.

;;; Code:

(defun go-stacktracer--get-buffer ()
  "Get an empty buffer for src's output."
  (let* ((buffer-name "*go-stacktracer*")
         (buffer (get-buffer buffer-name)))
    ;; Kill the existing buffer if it already exists.
    (when buffer (kill-buffer buffer))
    (get-buffer-create buffer-name)))

(defconst go-stacktracer-re "^\\s-*\\([^ ]*\\.go\\):\\([[:digit:]]+\\)")

;;;###autoload
(defun go-stacktracer-region (start end)
  "Parse a Go stacktrace from START to END."
  (interactive "r")
  (let ((trace (split-string (buffer-substring start end) "\n" t))
        (buf (go-stacktracer--get-buffer))
        (last-line ""))
    (with-current-buffer buf
      (insert "go-stacktracer results:\n\n"))
    (while trace
      (let ((line (car trace)))
        (if (not (eq (string-match go-stacktracer-re line) nil))
            (let ((file-path (substring line (match-beginning 1) (match-end 1)))
                  (line-num  (substring line (match-beginning 2) (match-end 2))))
              ;; strip default-directory from file-path.
              (if (not (eq (string-match (concat "^" default-directory) file-path) nil))
                  (setq file-path (substring file-path (match-end 0) (length file-path))))
              (with-current-buffer buf
                (insert file-path ":" line-num ": " last-line "\n"))))
        (setq last-line line))
      (setq trace (cdr trace)))
    (with-current-buffer buf
      (grep-mode)
      (goto-char (point-min)))
    (display-buffer buf)))

(provide 'go-stacktracer)
;;; go-stacktracer.el ends here
