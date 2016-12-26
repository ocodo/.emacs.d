;;; context-coloring.el --- Highlight by scope  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016  Free Software Foundation, Inc.

;; Author: Jackson Ray Hamilton <jackson@jacksonrayhamilton.com>
;; Version: 8.1.0
;; Keywords: convenience faces tools
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jacksonrayhamilton/context-coloring

;; This file is part of GNU Emacs.

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

;; Highlights code by scope.  Top-level scopes are one color, second-level
;; scopes are another color, and so on.  Variables retain the color of the scope
;; in which they are defined.  A variable defined in an outer scope referenced
;; by an inner scope is colored the same as the outer scope.

;; By default, comments and strings are still highlighted syntactically.

;;; Code:


;;; Utilities

(defun context-coloring-join (strings delimiter)
  "Join a list of STRINGS with the string DELIMITER."
  (mapconcat #'identity strings delimiter))

(defun context-coloring-check-predicates (predicates)
  "Call PREDICATES until one returns t, otherwise return nil."
  (let ((satisfied-p nil))
    (while (and predicates
                (not satisfied-p))
      (setq satisfied-p (funcall (pop predicates))))
    satisfied-p))


;;; Faces

(defun context-coloring-defface (level light dark tty)
  "Define a face for LEVEL with LIGHT, DARK and TTY colors."
  (let ((face (intern (format "context-coloring-level-%s-face" level)))
        (doc (format "Context coloring face, level %s." level)))
    (custom-declare-face
     face
     `((((type tty)) (:foreground ,tty))
       (((background light)) (:foreground ,light))
       (((background dark)) (:foreground ,dark)))
     doc
     :group 'context-coloring)))

;; Provide some default colors based off Emacs's defaults.
(context-coloring-defface 0 "#000000" "#ffffff" nil)
(context-coloring-defface 1 "#008b8b" "#00ffff" "yellow")
(context-coloring-defface 2 "#0000ff" "#87cefa" "green")
(context-coloring-defface 3 "#483d8b" "#b0c4de" "cyan")
(context-coloring-defface 4 "#a020f0" "#eedd82" "blue")
(context-coloring-defface 5 "#a0522d" "#98fb98" "magenta")
(context-coloring-defface 6 "#228b22" "#7fffd4" "red")
(context-coloring-defface 7 "#3f3f3f" "#cdcdcd" nil)

(defconst context-coloring-default-maximum-face 7
  "Maximum face when there are no custom faces.")

;; Create placeholder faces for users and theme authors.
(dotimes (level 18)
  (let* ((level (+ level 8))
         (face (intern (format "context-coloring-level-%s-face" level)))
         (doc (format "Context coloring face, level %s." level)))
    (custom-declare-face face nil doc :group 'context-coloring)))

(defvar-local context-coloring-maximum-face nil
  "Dynamic index of the highest face available for coloring.")

(defsubst context-coloring-level-face (level)
  "Return symbol for face with LEVEL."
  ;; `concat' is faster than `format' here.
  (intern-soft
   (concat "context-coloring-level-" (number-to-string level) "-face")))

(defsubst context-coloring-bounded-level-face (level)
  "Return symbol for face with LEVEL, bounded by the maximum."
  (context-coloring-level-face (min level context-coloring-maximum-face)))

(defconst context-coloring-level-face-regexp
  "context-coloring-level-\\([[:digit:]]+\\)-face"
  "Extract a level from a face.")

(defun context-coloring-theme-highest-level (theme)
  "Return the highest coloring level for THEME, or -1."
  (let* ((settings (get theme 'theme-settings))
         (tail settings)
         face-string
         number
         (found -1))
    (while tail
      (and (eq (nth 0 (car tail)) 'theme-face)
           (setq face-string (symbol-name (nth 1 (car tail))))
           (string-match
            context-coloring-level-face-regexp
            face-string)
           (setq number (string-to-number
                         (substring face-string
                                    (match-beginning 1)
                                    (match-end 1))))
           (> number found)
           (setq found number))
      (setq tail (cdr tail)))
    found))

(defun context-coloring-update-maximum-face ()
  "Save the highest possible face for the current theme."
  (let ((themes (append custom-enabled-themes '(user)))
        (continue t)
        theme
        highest-level)
    (while continue
      (setq theme (car themes))
      (setq themes (cdr themes))
      (setq highest-level (context-coloring-theme-highest-level theme))
      (setq continue (and themes (= highest-level -1))))
    (setq context-coloring-maximum-face
          (cond
           ((= highest-level -1)
            context-coloring-default-maximum-face)
           (t
            highest-level)))))


;;; Change detection

(defvar-local context-coloring-changed-p nil
  "Indication that the buffer has changed recently, which implies
that it should be colored again by
`context-coloring-maybe-colorize-idle-timer' if that timer is
being used.")

(defvar-local context-coloring-changed-start nil
  "Beginning of last text that changed.")

(defvar-local context-coloring-changed-end nil
  "End of last text that changed.")

(defvar-local context-coloring-changed-length nil
  "Length of last text that changed.")

(defun context-coloring-change-function (start end length)
  "Register a change so that a buffer can be colorized soon.

START, END and LENGTH are recorded for later use."
  ;; Tokenization is obsolete if there was a change.
  (setq context-coloring-changed-start start)
  (setq context-coloring-changed-end end)
  (setq context-coloring-changed-length length)
  (setq context-coloring-changed-p t))

(defun context-coloring-maybe-colorize-with-buffer (buffer)
  "Color BUFFER and if it has changed."
  (when (and (eq buffer (current-buffer))
             context-coloring-changed-p)
    (context-coloring-colorize-with-buffer buffer)
    (setq context-coloring-changed-p nil)
    (setq context-coloring-changed-start nil)
    (setq context-coloring-changed-end nil)
    (setq context-coloring-changed-length nil)))

(defvar-local context-coloring-maybe-colorize-idle-timer nil
  "The currently-running idle timer for conditional coloring.")

(defvar-local context-coloring-colorize-idle-timer nil
  "The currently-running idle timer for unconditional coloring.")

(defcustom context-coloring-default-delay 0.25
  "Default delay between a buffer update and colorization.

Increase this if your machine is high-performing.  Decrease it if
it ain't."
  :type 'float
  :group 'context-coloring)

(defun context-coloring-cancel-timer (timer)
  "Cancel TIMER."
  (when timer
    (cancel-timer timer)))

(defun context-coloring-schedule-coloring (time)
  "Schedule coloring to occur once after Emacs is idle for TIME."
  (context-coloring-cancel-timer context-coloring-colorize-idle-timer)
  (setq context-coloring-colorize-idle-timer
        (run-with-idle-timer
         time
         nil
         #'context-coloring-colorize-with-buffer
         (current-buffer))))

(defun context-coloring-setup-idle-change-detection ()
  "Setup idle change detection."
  (let ((dispatch (context-coloring-get-current-dispatch)))
    (add-hook
     'after-change-functions #'context-coloring-change-function nil t)
    (add-hook
     'kill-buffer-hook #'context-coloring-teardown-idle-change-detection nil t)
    (setq context-coloring-maybe-colorize-idle-timer
          (run-with-idle-timer
           (or (plist-get dispatch :delay) context-coloring-default-delay)
           t
           #'context-coloring-maybe-colorize-with-buffer
           (current-buffer)))))

(defun context-coloring-teardown-idle-change-detection ()
  "Teardown idle change detection."
  (dolist (timer (list context-coloring-colorize-idle-timer
                       context-coloring-maybe-colorize-idle-timer))
    (context-coloring-cancel-timer timer))
  (remove-hook
   'kill-buffer-hook #'context-coloring-teardown-idle-change-detection t)
  (remove-hook
   'after-change-functions #'context-coloring-change-function t))


;;; Colorization utilities

(defsubst context-coloring-colorize-region (start end level)
  "Color from START (inclusive) to END (exclusive) with LEVEL."
  (add-text-properties
   start
   end
   `(face ,(context-coloring-bounded-level-face level))))

(defcustom context-coloring-syntactic-comments t
  "If non-nil, also color comments using `font-lock'."
  :type 'boolean
  :group 'context-coloring)

(defcustom context-coloring-syntactic-strings t
  "If non-nil, also color strings using `font-lock'."
  :type 'boolean
  :group 'context-coloring)

(defun context-coloring-font-lock-syntactic-comment-function (state)
  "Color a comment according to STATE."
  (if (nth 3 state) nil font-lock-comment-face))

(defun context-coloring-font-lock-syntactic-string-function (state)
  "Color a string according to STATE."
  (if (nth 3 state) font-lock-string-face nil))

(defsubst context-coloring-colorize-comments-and-strings (&optional min max keywords-p)
  "Maybe color comments and strings in buffer from MIN to MAX.
MIN defaults to beginning of buffer.  MAX defaults to end.  If
KEYWORDS-P is non-nil, also color keywords from MIN to MAX."
  (when (or context-coloring-syntactic-comments
            context-coloring-syntactic-strings)
    (let ((min (or min (point-min)))
          (max (or max (point-max)))
          (font-lock-syntactic-face-function
           (cond
            ((and context-coloring-syntactic-comments
                  (not context-coloring-syntactic-strings))
             #'context-coloring-font-lock-syntactic-comment-function)
            ((and context-coloring-syntactic-strings
                  (not context-coloring-syntactic-comments))
             #'context-coloring-font-lock-syntactic-string-function)
            (t
             font-lock-syntactic-face-function))))
      (save-excursion
        (font-lock-fontify-syntactically-region min max)
        (when keywords-p
          (font-lock-fontify-keywords-region min max))))))

(defcustom context-coloring-initial-level 0
  "Scope level at which to start coloring.

If top-level variables and functions do not become global, but
are scoped to a file (as in Node.js), set this to 1."
  :type 'integer
  :safe #'integerp
  :group 'context-coloring)


;;; Dispatch

;;;###autoload
(defvar context-coloring-dispatch-hash-table (make-hash-table :test #'eq)
  "Map dispatch strategy names to their property lists.

A \"dispatch\" is a property list describing a strategy for
coloring a buffer.

Its properties must include one of `:modes' or `:predicate', and
a `:colorizer'.

`:modes' - List of major modes this dispatch is valid for.

`:predicate' - Function that determines if the dispatch is valid
for any given state.

`:colorizer' - Function that parses and colors the buffer.

`:delay' - Delay between buffer update and colorization, to
override `context-coloring-default-delay'.

`:setup' - Arbitrary code to set up this dispatch when
`context-coloring-mode' is enabled.

`:teardown' - Arbitrary code to tear down this dispatch when
`context-coloring-mode' is disabled.

`:async-p' - Hint that code will be colorized asynchronously.
Please call `context-coloring-after-colorize' when colorization
completes.")

(defun context-coloring-find-dispatch (predicate)
  "Find the first dispatch satisfying PREDICATE."
  (let (found)
    (maphash
     (lambda (_ dispatch)
       (when (and (not found)
                  (funcall predicate dispatch))
         (setq found dispatch)))
     context-coloring-dispatch-hash-table)
    found))

(defun context-coloring-get-current-dispatch ()
  "Return the first dispatch appropriate for the current state."
  (cond
   ;; Maybe a predicate will be satisfied.
   ((context-coloring-find-dispatch
     (lambda (dispatch)
       (let ((predicate (plist-get dispatch :predicate)))
         (and predicate (funcall predicate))))))
   ;; If not, maybe a major mode (or a derivative) will.
   ((context-coloring-find-dispatch
     (lambda (dispatch)
       (let ((modes (plist-get dispatch :modes))
             match)
         (while (and modes (not match))
           (setq match (eq (pop modes) major-mode)))
         match))))))

(defun context-coloring-before-colorize ()
  "Set up environment for colorization."
  (context-coloring-update-maximum-face))

(defvar context-coloring-after-colorize-hook nil
  "Functions to run after colorizing.")

(defun context-coloring-after-colorize ()
  "Do final business after colorization."
  (run-hooks 'context-coloring-after-colorize-hook))

(defun context-coloring-dispatch ()
  "Determine how to color the current buffer, and color it."
  (let* ((dispatch (context-coloring-get-current-dispatch))
         (colorizer (plist-get dispatch :colorizer))
         (async-p (plist-get dispatch :async-p)))
    (context-coloring-before-colorize)
    (when colorizer
      (catch 'interrupted
        (funcall colorizer)))
    (unless async-p
      (context-coloring-after-colorize))))


;;; Colorization

(defvar context-coloring-fontify-keywords-predicates
  (list
   (lambda () (and (boundp 'prettify-symbols-mode) prettify-symbols-mode)))
  "Cases where the whole buffer should have keywords fontified.
Necessary in cases where a mode relies on fontifications in
regions where Context Coloring doesn't happen to touch.")

(defun context-coloring-maybe-fontify-keywords ()
  "Determine if the buffer ought to have keywords fontified."
  (when (context-coloring-check-predicates
         context-coloring-fontify-keywords-predicates)
    (with-silent-modifications
      (save-excursion
        (font-lock-fontify-keywords-region (point-min) (point-max))))))

(add-hook 'context-coloring-after-colorize-hook
          #'context-coloring-maybe-fontify-keywords)

(defun context-coloring-colorize ()
  "Color the current buffer by function context."
  (interactive)
  (context-coloring-dispatch))

(defun context-coloring-colorize-with-buffer (buffer)
  "Color BUFFER."
  ;; Don't select deleted buffers.
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (context-coloring-colorize))))


;;; Minor mode

(defvar context-coloring-ignore-unavailable-predicates
  (list
   #'minibufferp)
  "Cases when \"unavailable\" messages are silenced.
Necessary in editing states where coloring is only sometimes
permissible.")

(defun context-coloring-ignore-unavailable-message-p ()
  "Determine if the unavailable message should be silenced."
  (context-coloring-check-predicates
   context-coloring-ignore-unavailable-predicates))

(defvar context-coloring-interruptable-p t
  "When non-nil, coloring may be interrupted by user input.")

;;;###autoload
(define-minor-mode context-coloring-mode
  "Toggle contextual code coloring.
With a prefix argument ARG, enable Context Coloring mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

Context Coloring mode is a buffer-local minor mode.  When
enabled, code is colored by scope.  Scopes are colored
hierarchically.  Variables referenced from nested scopes retain
the color of their defining scopes.  Certain syntax, like
comments and strings, is still colored with `font-lock'.

The entire buffer is colored initially.  Changes to the buffer
trigger recoloring.

Define your own colors by customizing faces like
`context-coloring-level-N-face', where N is a number starting
from 0.  If no face is found on a custom theme nor the `user'
theme, the defaults are used.

New language / major mode support can be added with
`context-coloring-define-dispatch', which see.

Feature inspired by Douglas Crockford."
  nil " Context" nil
  (cond
   (context-coloring-mode
    (let ((dispatch (context-coloring-get-current-dispatch)))
      (cond
       (dispatch
        ;; Font lock is incompatible with this mode; the converse is also true.
        (font-lock-mode 0)
        ;; ...but we do use font-lock functions here.
        (font-lock-set-defaults)
        ;; Safely change the value of this function as necessary.
        (make-local-variable 'font-lock-syntactic-face-function)
        ;; Improve integration with `prettify-symbols-mode'.  It relies on Font
        ;; Lock's automatic fontification to apply it's changes on mode change,
        ;; so Context Coloring has to make those changes manually.
        (add-hook 'prettify-symbols-mode-hook #'context-coloring-maybe-fontify-keywords nil t)
        ;; Furthermore, on Emacs < 25.0, `prettify-symbols-mode' calls
        ;; `font-lock-fontify-buffer-function' which would overwrite context
        ;; coloring, so make it a no-op.
        (set (make-local-variable 'font-lock-fontify-buffer-function) (lambda ()))
        (let ((setup (plist-get dispatch :setup)))
          (when setup
            (funcall setup))
          ;; Colorize once initially.
          (let ((context-coloring-interruptable-p nil))
            (context-coloring-colorize))))
       ((not (context-coloring-ignore-unavailable-message-p))
        (message "Context coloring is unavailable here")))))
   (t
    (let ((dispatch (context-coloring-get-current-dispatch)))
      (when dispatch
        (let ((teardown (plist-get dispatch :teardown)))
          (when teardown
            (funcall teardown)))))
    (remove-hook 'prettify-symbols-mode-hook #'context-coloring-maybe-fontify-keywords t)
    (turn-on-font-lock-if-desired))))

(provide 'context-coloring)

;;; context-coloring.el ends here
