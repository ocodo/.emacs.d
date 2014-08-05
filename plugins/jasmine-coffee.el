;;; jasmine-coffee --- Helpers for Jasmine (in coffeescript).
;;; Author: Jason Milkins
;;; Version: 20140730
;;; Commentary:
;;
;;  This file is not a part of Emacs
;;
;; ## Installation
;;
;; This script is still very much a work in-progress, so it's not
;; available via MELPA or any other package repository yet.
;;
;; Installation is manual only:
;;
;; Place the `jasmine-coffee.el` script in your Emacs `load-path` and add:
;;
;;     (eval-after-load "coffee-mode"
;;         (require 'jasmine-coffee))
;;
;; To your `~/.emacs` (or ~/.emacs.d/init.el).
;;
;; ### Spec launchers
;;
;; Launch the first Jasmine coffee-script `it` spec found at or above the
;; current point.
;;
;;     M-x jasmine-coffee/verify-it
;;     M-x jasmine-coffee/verify-single
;;
;; Launch the first Jasmine coffee-script `describe` group found at or
;; above the current point.
;;
;;     M-x jasmine-coffee/verify-describe
;;     M-x jasmine-coffee/verify-group
;;
;; Set the variable `jasmine-coffee/base-url` to set your jasmine spec runner base URL.
;;
;; e.g.
;;
;;     (setq jasmine-coffee/base-url "http://localhost:3000/jasmine?spec=")
;;
;; The helper commands above will compose the URL for you.  Note I've
;; only tested this with Jasmine-rice, so please post an issue if you use
;; the Karma runner or another Jasmine runner, and you find this
;; incompatible.
;;
;; ### Moving to outer blocks
;;
;; These commands move the current line or region into enclosing `describe`
;; block (useful for lazy/jlet) or into the previous `beforeEach`.
;;
;; Move the current line or region to the previous `describe` body
;;
;;     M-x jasmine-coffee/move-to-previous-describe
;;
;; Move the current line or region to the previous `beforeEach` body
;;
;;     M-x jasmine-coffee/move-to-previous-before-each
;;
;; ### Lazily evaluated vars
;;
;; Jasmine-Let by Diego Garcia (github @xaethos) allows you to use a lazy
;; variable which is evaluated when your `it` spec is run.  For more info
;; see: https://github.com/xaethos/jasmine-let
;;
;; Convert a local var assignment to a `lazy`
;;
;;     M-x jasmine-coffee/var-to-lazy
;;
;; In my day-to-day we assign `jasmineLet` to an alias `jlet` (instead of
;; `lazy`) so this command is really just for me and my team.
;;
;; Convert a local var assignment to a `jlet`
;;
;;     M-x jasmine-coffee/var-to-jlet
;;
;; We also use `jset` which works like `let!` in rspec, so:
;;
;; Convert a local var assignment to a `jset`
;;
;;     M-x jasmine-coffee/var-to-jset
;;
;; ### More
;;
;; I have a collection of coffee-mode yasnippets for jasmine, which I'll
;; migrate to this package soon.
;;
;; If you're impatient to grab them, you can get them directly from my
;; `.emacs.d`.  See the coffee-mode snippets folder
;; https://github.com/ocodo/emacs.d/tree/master/snippets/coffee-mode
;;
;; I'll also be adding navigation and further editing helpers (feature
;; matching
;; [buster-mode](https://gitorious.org/buster/buster-mode/source/c9d4b6b6f85283e18363c8236620905f58110831:buster-mode.el))
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
  (insert " -> "))

(defun jasmine-coffee/move-to-indentation ()
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
        (jasmine-coffee/move-to-indentation)
        (push (current-column) columns-list)
        (forward-line 1)))
    (reverse columns-list)))

(defun jasmine-coffee/get-current-line-indent-as-list ()
  "Get a list containing the current line's indentation column."
  (jasmine-coffee/move-to-indentation)
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
  "Convert local var on the current line to a `lazy'.
See Jasmine-let github.com:xaethos/jasmine-let.git"
  (interactive)
  (save-excursion
    (jasmine-coffee/var-to-function-form "lazy")))

(defun jasmine-coffee/var-to-jlet ()
  "Convert local var on the current line to a `jlet'.

`jlet' is a lazy evaluation variable form for jasmine, similar to
rspec's `let'.  It is not a part of jasmine."
  (interactive)
  (save-excursion
    (jasmine-coffee/var-to-function-form "jlet")))

(defun jasmine-coffee/var-to-jset ()
  "Convert local var on the current line to a `jset'.

`jset' is a variable evaluation form similar to rspec's `let!'.
It is not a part of jasmine."
  (interactive)
  (save-excursion
    (jasmine-coffee/var-to-function-form "jset")))

(defun jasmine-coffee/move-to-previous-describe ()
  "Move the current line or region to the previous `describe' body."
  (interactive)
  (jasmine-coffee/move-to-previous-thing jasmine-coffee/describe-regexp))

(defun jasmine-coffee/move-to-previous-before-each ()
  "Move the current line or region to the previous `beforeEach' body."
  (interactive)
  (jasmine-coffee/move-to-previous-thing jasmine-coffee/before-each-regexp))

(defun jasmine-coffee/verify-describe ()
  "Compose and launch Spec URL for the current `describe' block."
  (interactive)
  (jasmine-coffee/verify-thing jasmine-coffee/describe-regexp))

(defun jasmine-coffee/verify-group ()
  "Alias for verify describe.
Compose and launch Spec URL for the current `describe' block."
  (interactive)
  (jasmine-coffee/verify-describe))

(defun jasmine-coffee/verify-it ()
  "Compose and launch spec URL for the current `it' spec."
  (interactive)
  (jasmine-coffee/verify-thing jasmine-coffee/it-regexp))

(defun jasmine-coffee/verify-single ()
  "Alias for verify-it.
Compose and launch spec URL for the current `it' spec."
  (interactive)
  (jasmine-coffee/verify-it))

(defun jasmine-coffee/navigate-to-next-thing (regexp)
  "Navigate cursor to the body of the next matching REGEXP."
  (with-demoted-errors
    (re-search-forward regexp))
  (forward-line 1)
  (jc/move-to-indentation))

(defun jasmine-coffee/navigate-to-previous-thing (regexp)
  "Navigate cursor to the body of the previous REGEXP."
  (with-demoted-errors
    (re-search-backward regexp))
  (forward-line 1)
  (jc/move-to-indentation))

(defun jasmine-coffee/navigate-to-next-it ()
  "Navigate cursor to the body of the next `it' spec."
  (interactive)
  (jasmine-coffee/navigate-to-next-thing jasmine-coffee/it-regexp))

(defun jasmine-coffee/navigate-to-previous-it ()
  "Navigate cursor to the body of the previous `it' spec."
  (interactive)
  (jasmine-coffee/navigate-to-previous-thing jasmine-coffee/it-regexp))

(defun jasmine-coffee/navigate-to-next-describe ()
  "Navigate cursor to the body of the next `describe' block."
  (interactive)
  (jasmine-coffee/navigate-to-next-thing jasmine-coffee/describe-regexp))

(defun jasmine-coffee/navigate-to-previous-describe ()
  "Navigate cursor to the body of the previous `describe' block."
  (interactive)
  (jasmine-coffee/navigate-to-previous-thing jasmine-coffee/describe-regexp))

(defun jasmine-coffee/navigate-to-next-before-each ()
  "Navigate cursor to the body of the next `beforeEach' block."
  (interactive)
  (jasmine-coffee/navigate-to-previous-thing jasmine-coffee/before-each-regexp))

(defun jasmine-coffee/navigate-to-previous-before-each ()
  "Navigate cursor to the body of the previous `beforeEach' block."
  (interactive)
  (jasmine-coffee/navigate-to-previous-thing jasmine-coffee/before-each-regexp))

(defun jasmine-coffee/toggle-spec-enabled ()
  "Toggle the current `it' spec on/off."
  (interactive)
  (save-excursion
    (jc/end-of-line)
    (with-demoted-errors
      (re-search-backward (rx line-start (+ blank) "it" (group (? "x")))))
    (jc/move-to-indentation)
    (when (looking-at "it ") (forward-char 2) (insert "x"))
    (when (looking-at "itx") (forward-char 2) (delete-char 1))))

(defun jc/move-to-indentation ()
  "Internal function to jump to indentation column."
  (jc/end-of-line)
  (back-to-indentation))

(defun jc/end-of-line ()
  "Internal function jump to end of line."
  (move-end-of-line 1))

(provide 'jasmine-coffee)
;;; jasmine-coffee.el ends here
