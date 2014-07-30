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

(defun jasmine-coffee/verify-single ()
  "Compose the Spec URL launch a browser and run the single spec above the cursor point."
  (interactive)
  (jasmine-coffee/verify-it))

(defun jasmine-coffee/verify-it ()
  "Compose the Spec URL launch a browser and run the single spec above the cursor point."
  (interactive)
  (let* ((start-column 0) (spec-string ""))
    (save-excursion
      (re-search-backward jasmine-coffee/it-regexp)
      (setq start-column (current-column))
      (setq spec-string (match-string-no-properties 1))
      (while
          (re-search-backward jasmine-coffee/describe-regexp 0 t)
        (when (< (current-column) start-column)
          (setq start-column (current-column))
          (setq spec-string (format "%s %s" (match-string 1) spec-string)))))
    (setq spec-string (replace-regexp-in-string "#" "%23" spec-string))
    (browse-url (url-encode-url (concat jasmine-coffee/base-url spec-string)))))

(defun jasmine-coffee/verify-group ()
  "Compose the Spec URL launch a browser and run the spec at the cursor point."
  (interactive)
  (jasmine-coffee/verify-describe))

(defun jasmine-coffee/verify-describe ()
  "Compose the Spec URL launch a browser and run the spec at the cursor point."
  (interactive)
  (let* ((start-column 0) (spec-string ""))
    (save-excursion
      (re-search-backward jasmine-coffee/describe-regexp)
      (setq start-column (current-column))
      (setq spec-string (match-string-no-properties 1))
      (while
          (re-search-backward jasmine-coffee/describe-regexp 0 t)
        (when (< (current-column) start-column)
          (setq start-column (current-column))
          (setq spec-string (format "%s %s" (match-string 1) spec-string)))))
    (setq spec-string (replace-regexp-in-string "#" "%23" spec-string))
    (browse-url (url-encode-url (concat jasmine-coffee/base-url spec-string)))))

(defun jasmine-coffee/var-to-jlet ()
  "Convert local var on current line to a jlet.
Don't move it, in case it's a multiline expression."
  (interactive)
  (save-excursion
    (jasmine-coffee/var-to-function-call-form "jlet")))

(defun jasmine-coffee/var-to-jset ()
  "Convert local var on current line to a jset.
Don't move it, in case it's a multiline expression."
  (interactive)
  (save-excursion
    (jasmine-coffee/var-to-function-call-form "jset")))

(defun jasmine-coffee/var-to-function-call-form (function-call-prefix)
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

(defun jasmine-coffee/move-to-previous-before-each ()
  "Move the current line or region to the previous beforeEach body."
  (interactive)
  (save-excursion
    (jasmine-coffee/kill-line-or-region)
    (re-search-backward jasmine-coffee/before-each-regexp)
    (end-of-line)
    (coffee-newline-and-indent)
    (yank)))

(defun jasmine-coffee/move-to-previous-describe ()
  "Move the current line or region to the previous describe body."
  (interactive)
  (save-excursion
    (jasmine-coffee/kill-line-or-region)
    (re-search-backward jasmine-coffee/describe-regexp)
    (end-of-line)
    (coffee-newline-and-indent)
    (yank)))

(defun jasmine-coffee/kill-line-or-region ()
  "Utility function to kill whole line or region."
  (let* (region (list ))
    (setq region (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (progn
                     (beginning-of-line)
                     (delete-horizontal-space)
                     (list (line-beginning-position) (line-beginning-position 2)))))
    (kill-region (car region) (car (cdr region)))))

(provide 'jasmine-coffee)
;;; jasmine-coffee.el ends here
