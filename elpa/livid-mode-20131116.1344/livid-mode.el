;;; livid-mode.el --- Live browser eval of JavaScript every time a buffer changes

;; This is free and unencumbered software released into the public domain.

;; Author: Murphy McMahon
;; URL: https://github.com/pandeiro/livid-mode
;; Package-Version: 20131116.1344
;; Version: 0.1.0
;; Package-Requires: ((skewer-mode "1.5.3") (s "1.8.0"))

;;; Commentary:

;; This mode tries to create an environment similar to some online code editors
;; (such as jsbin or codepen), where JavaScript code in a buffer is evaluated
;; automatically every time it changes. However, in contrast to those tools,
;; the browser's state is not refreshed. The code is just evaluated, as if via the
;; browser's console.

;; By default, a buffer's code is first checked for syntax errors (using the
;; external `js` command) before actually being sent for evaluation.

;; Depends on skewer-mode and s. If livid-mode is activated in a buffer which is
;; not in skewer-mode already, skewer-mode is called.

;; As an alternative to turning the mode on and off, livid can be "paused"
;; (postponing any eval) with livid-pause, bound by default to C-c C-p.

;;; Code:

(require 'skewer-mode)

(require 's)

(defgroup livid-mode nil
  "Evaluate js via skewer whenever buffer changes, for better or worse"
  :group 'comm)

(defvar livid-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-c C-p") 'livid-toggle-pause)))
  "Keymap for livid mode")

(defcustom livid-validate-javascript-command
  "js -C -e"
  "Command to invoke a JavaScript process that will check for
syntax errors. Assumes a spidermonkey-compatible process
that uses the string \"SyntaxError\" to denote syntax errors."
  :group 'livid-mode
  :type 'string)

(defcustom livid-validate-javascript
  t
  "Determines whether to pass buffer code through `livid-validate-javascript-command'
before trying to evaluate it in the browser."
  :group 'livid-mode
  :type 'boolean)

(defvar livid-last-seen ""
  "String that contains the last buffer string sent for evaluation.
Compared against the current buffer value to determine whether to send.
Buffer contents have been trimmed before comparison and evaluation.")

(defvar livid-paused nil
  "Pauses livid if non-nil")

(defun livid-toggle-pause ()
  (interactive)
  (if livid-paused
      (progn
	(message "Unpaused livid-mode")
	(setq livid-paused nil)
	(livid-tick nil nil nil))
    (progn
      (message "Paused livid-mode")
      (setq livid-paused t))))

(defun livid-validate (code)
  (let ((escaped-code (shell-quote-argument code))
	(syntax-error (regexp-quote "SyntaxError")))
    (not (string-match-p syntax-error
			 (shell-command-to-string
			  (concat
			   livid-validate-javascript-command " "
			   escaped-code))))))

(defun livid-tick (start end length)
  "Run via after-change-functions, this gets the trimmed value of the buffer's
code, and then checks to see whether it should try to evaluate it.

First, it sees if livid is in a paused state. Then, it makes sure the code is different
than the last evaluated code. And last, it checks its syntax via an external JavaScript
command.

If everything is in order, it sends it the browser through the skewer connection."
  (let ((code (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (when (and (not livid-paused)
	       (not (equal livid-last-seen code))
	       (or (not livid-validate-javascript)
		   (livid-validate code)))
      (setq livid-last-seen code)
      (skewer-eval code))))

;;;###autoload
(define-minor-mode livid-mode
  "Minor mode for automatic evaluation of a JavaScript buffer on every change"
  :lighter " livid"
  :group 'livid
  :keymap livid-mode-map

  (make-local-variable 'livid-timer)
  (make-local-variable 'livid-last-seen)
  (make-local-variable 'livid-paused)

  (if livid-mode
      (progn
	(when (not (memq 'skewer-mode minor-mode-list))
	  (skewer-mode 1))
	(livid-tick nil nil nil)
	(add-hook 'after-change-functions 'livid-tick nil t))
    (progn
      (remove-hook 'after-change-functions 'livid-tick t)
      (setq livid-last-seen ""))))

(provide 'livid-mode)

;;; livid-mode.el ends here
