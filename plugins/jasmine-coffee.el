;;; jasmine-coffee --- Helpers for Jasmine (in coffeescript).
;;; Author: Jason Milkins
;;; Version: 20140730
;;; Commentary:
;;
;;  This file is not a part of Emacs
;;
;;  Launch the first Jasmine coffeescript spec above the current point.
;;
;;    M-x jasmine-coffee/verify-it
;;    M-x jasmine-coffee/verify-single
;;
;;  Launch the first Jasmine coffeescript describe group above the current point.
;;
;;    M-x jasmine-coffee/verify-describe
;;    M-x jasmine-coffee/verify-group
;;
;;  Convert a local var assignment to a jlet
;;
;;    M-x jasmine-coffee/var-to-jlet
;;
;;  Convert a local var assignment to a jset
;;
;;    M-x jasmine-coffee/var-to-jset
;;
;;  Convert a local var assignment to a lazy
;;
;;    M-x jasmine-coffee/var-to-lazy
;;
;;  Move the current line or region to the previous describe body
;;
;;    M-x jasmine-coffee/move-to-previous-describe
;;
;;  Move the current line or region to the previous beforeEach body
;;
;;    M-x jasmine-coffee/move-to-previous-before-each
;;
;;; Licence: GPL3
;;
;;; Requires: ((coffee-mode))
;;; Code:

(require 'coffee-mode)

(defvar jasmine-coffee/base-url
  "http://localhost:3000/jasmine?spec="
  "Base URL for our Jasmine spec runner.")

(defvar jasmine-coffee/it-regexp
  (rx "it" (any " " "(") (zero-or-more " ")
      (any "'" "\"")
      (group (zero-or-more not-newline))
      (any "'" "\"") (? ")")
      (zero-or-more " ") ",")
  "Regexp to find a jasmine coffee-mode `it'.")

(defvar jasmine-coffee/describe-regexp
  (rx "describe" (any " " "(") (zero-or-more " ")
      (any "'" "\"")
      (group (zero-or-more not-newline))
      (any "'" "\"") (? ")")
      (zero-or-more " ") ",")
  "Regexp to find a jasmine coffee-mode `describe'.")

(defvar jasmine-coffee/before-each-regexp
  (rx "beforeEach" (? "(") (? " ") "->")
  "Regexp to find a jasmine coffee-mode `beforeEach'.")

(defun jasmine-coffee/kill-line-or-region ()
  "Utility function to kill whole line or region."
  (let (region)
    (setq region (if (use-region-p)
                     (list (region-beginning) (region-end))
                     (list (line-beginning-position) (line-beginning-position 2))))
    (kill-region (car region) (car (cdr region)))))

(defun jasmine-coffee/var-to-function-form (function-call-prefix)
  "Non-interactive convert variable to FUNCTION-CALL-PREFIX form."
  ;; NOTE: This is a very simplistic macro-like implementation, I
  ;; should improve it.
  (with-demoted-errors
    (beginning-of-line)
    (re-search-forward "=")
    (kill-backward-chars 1)
    (insert ",")
    (beginning-of-line)
    (search-forward-regexp "[^\s]")
    (backward-char)
    (insert (format "%s '" function-call-prefix))
    (search-forward-regexp "[\s]")
    (backward-char)
    (insert "'")
    (delete-horizontal-space)
    (forward-char)
    (delete-horizontal-space)
    (insert " -> ")))

(defun jasmine-coffee/move-to-indentedation ()
  "Move to the indentation of the current line."
  (move-end-of-line 1)
  (back-to-indentation))

(defun jasmine-coffee/get-region-indent-list ()
  "Collect list of indent columns from region."
  (let (columns-list
        (end (region-end)))
    (save-excursion
      (when (= (point) end) (exchange-point-and-mark))
      (while (< (point) end)
        (jasmine-coffee/move-to-indentedation)
        (push (current-column) columns-list)
        (forward-line 1)))
    (reverse columns-list)))

(defun jasmine-coffee/get-current-line-indent-as-list ()
  "Get a list containing the current line's indentation column."
  (jasmine-coffee/move-to-indentedation)
  (list (current-column)))

(defun jasmine-coffee/get-indent-list ()
  "Get a list of indent positions from either the region or the current line."
  (if (use-region-p)
      (jasmine-coffee/get-region-indent-list)
    (jasmine-coffee/get-current-line-indent-as-list)))

(defun jasmine-coffee/reset-indentation (indent-list)
  "From the current posion reset indentation using the supplied INDENT-LIST."
  (let* ((current (current-column))
         (first (pop indent-list))
         (difference (- current first)))
    (while indent-list
      (forward-line)
      (indent-line-to (+ difference (pop indent-list))))))

(defun jasmine-coffee/move-to-previous-thing (pattern)
  "Move the current line or region to the previous thing defined by PATTERN."
  (save-excursion
    (let* ((indent-list (jasmine-coffee/get-indent-list)))
      (jasmine-coffee/kill-line-or-region)
      (re-search-backward pattern)
      (end-of-line)
      (save-excursion
        (yank))
      (delete-horizontal-space)
      (coffee-newline-and-indent)
      (jasmine-coffee/reset-indentation indent-list))))

(defun jasmine-coffee/verify-thing (pattern)
  "Compose and launch a jasmine spec URL for the thing defined by PATTERN."
  (let* ((start-column 0) (spec-string ""))
    (save-excursion
      (move-end-of-line 1)
      (re-search-backward pattern)
      (setq start-column (current-column))
      (setq spec-string (match-string-no-properties 1))
      (while
          (re-search-backward jasmine-coffee/describe-regexp 0 t)
        (when (< (current-column) start-column)
          (setq start-column (current-column))
          (setq spec-string (format "%s %s" (match-string 1) spec-string)))))
    (setq spec-string (replace-regexp-in-string "#" "%23" spec-string))
    (save-current-buffer)
    (browse-url (url-encode-url (concat jasmine-coffee/base-url spec-string)))))

(defun jasmine-coffee/var-to-lazy ()
  "Convert local var on the current line to a lazy.
See Jasmine-let github.com:xaethos/jasmine-let.git"
  (interactive)
  (save-excursion
    (jasmine-coffee/var-to-function-form "lazy")))

(defun jasmine-coffee/var-to-jlet ()
  "Convert local var on the current line to a jlet.

jlet is a lazy evaluation variable form for jasmine, similar to
jasmine-let.  It is not a part of jasmine."
  (interactive)
  (save-excursion
    (jasmine-coffee/var-to-function-form "jlet")))

(defun jasmine-coffee/var-to-jset ()
  "Convert local var on the current line to a jset.

jset is a variable evaluation form similar to rspec's let!.
It is not a part of jasmine."
  (interactive)
  (save-excursion
    (jasmine-coffee/var-to-function-form "jset")))

(defun jasmine-coffee/move-to-previous-describe ()
  "Move the current line or region to the previous describe body."
  (interactive)
  (jasmine-coffee/move-to-previous-thing jasmine-coffee/describe-regexp))

(defun jasmine-coffee/move-to-previous-before-each ()
  "Move the current line or region to the previous beforeEach body."
  (interactive)
  (jasmine-coffee/move-to-previous-thing jasmine-coffee/before-each-regexp))

(defun jasmine-coffee/verify-describe ()
  "Compose and launch Spec URL for the current describe block."
  (interactive)
  (jasmine-coffee/verify-thing jasmine-coffee/describe-regexp))

(defun jasmine-coffee/verify-group ()
  "Alias for verify describe.
Compose and launch Spec URL for the current describe block."
  (interactive)
  (jasmine-coffee/verify-describe))

(defun jasmine-coffee/verify-it ()
  "Compose and launch spec URL for the current spec."
  (interactive)
  (jasmine-coffee/verify-thing jasmine-coffee/it-regexp))

(defun jasmine-coffee/verify-single ()
  "Alias for verify it.
Compose and launch spec URL for the current spec."
  (interactive)
  (jasmine-coffee/verify-it))

(provide 'jasmine-coffee)
;;; jasmine-coffee.el ends here
