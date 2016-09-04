;;; go-complete.el --- Native code completion for Go
;; Copyright (C) 2015 Vibhav Pant

;; Author: Vibhav Pant <vibhavp@gmail.com>
;; Version: 1.0
;; Package-Version: 20151015.228
;; Package-Requires: ((go-mode "0") (cl-lib "0.5"))
;; Keywords: go, golang, completion
;; URL: https://github.com/vibhavp/go-complete

;;; Commentary:
;; This package provides native code completion for the Go Programming
;; Language.
;; To enable, ad the following code into your .emacs:
;; (require 'go-complete)
;; (add-hook 'completion-at-point-functions 'go-complete-at-point)
;; You need to have gocode (https://github.com/nsf/gocode) installed to use
;; this package.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)

(defcustom go-complete-gocode-command "gocode"
  "The command to invoke `gocode'."
  :group 'go-completion
  :type 'string)

(defun go-complete-run-gocode ()
  "Run gocode on the current point, return a buffer containing the output."
  (let ((temp-buffer (generate-new-buffer "*gocode*")))
    (if (buffer-modified-p)
	(call-process-region (point-min)
			     (point-max)
			     go-complete-gocode-command
			     nil
			     temp-buffer
			     nil
			     "-f=emacs"
			     "autocomplete"
			     (concat "c" (int-to-string (-  (point) 1))))
      (call-process-region
       (point-min)
       (point-min)
       "gocode"
       nil
       temp-buffer
       nil
       "-f=emacs"
       (format "--in=%s" buffer-file-name)
       "autocomplete"
       (concat "c" (int-to-string (- (point) 1)))))
    temp-buffer))

(defun go-complete-char-at (string index)
  "Return the character in STRING at index INDEX."
  (substring string index (+ index 1)))

(defun go-complete-args-commas (string)
  "STRING is a function completion returned by gocode.
Return a string in the format foo(,,), where foo is the function
name, and the number of commas = number of arguments taken by the function"
  (let ((index (string-match ",,func(" string))
	(args 0))
    (unless (or (eq index nil) (string= (go-complete-char-at string (+ index 1)) ")"))
      (cl-incf index 2)
      (while (and (not (eq index (- (length string) 1))) (not (string= (go-complete-char-at string index) ")")))
	(when (string= (go-complete-char-at string index) ",")
	  (cl-incf args))
	(cl-incf index))
      (format "(%s)" (make-string args ?,)))))

(defun go-complete-make-completion (string)
  "Take a completion STRING from gocode, return a completion string.
If STRING is a function completion, return foo(,,), where foo is the function
name, and the number of commas = number of arguments taken by the function"
  (format "%s%s"
	  (substring string 0 (string-match "," string))
	  (if (string-match ",,func(" string) (go-complete-args-commas string) "")))

(defun go-complete-make-completion-list (buffer)
  "Take a BUFFER containing gocode output, return a list of completions.
The list returned is compatible with `completion-at-point-functions'"
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((completion-list '()))
      (while (not (eq (point) (point-max)))
	(setq completion-list
	      (append completion-list (list (go-complete-make-completion
					     (buffer-substring
					      (line-beginning-position)
					      (line-end-position))))))
	(forward-line))
      (kill-buffer buffer)
      completion-list)))

;;;###autoload
(defun go-complete-at-point ()
  "Complete go expression at point."
  (interactive)
  (when (derived-mode-p 'go-mode)
    (let ((token (current-word t))
	  (completing-field (string= "." (buffer-substring
					  (- (point) 1) (point)))))
      (when (or token completing-field)
	(list
	 (if completing-field
	     (point)
	   (save-excursion (left-word) (point)))
	 (point)
	 (go-complete-make-completion-list (go-complete-run-gocode))
	 .
	 nil)))))

(provide 'go-complete)
;;; go-complete.el ends here
