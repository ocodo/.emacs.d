;;; relative-line-numbers.el --- Display relative line numbers on the margin  -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/relative-line-numbers
;; Package-Version: 20161112.2151
;; Version: 0.3.3
;; Package-Requires: ((emacs "24"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2014-2016, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Code:

;; So we can use its face.
(require 'linum)

(defgroup relative-line-numbers nil
  "Show relative line numbers in the margin."
  :group 'convenience
  :prefix "relative-line-numbers-")

(defcustom relative-line-numbers-delay 0
  "The delay, in seconds, before updating the line numbers."
  :type 'number
  :group 'relative-line-numbers)

(defcustom relative-line-numbers-max-count 0
  "Count only up to this number of lines if greater than zero."
  :type 'integer
  :group 'relative-line-numbers)

(defcustom relative-line-numbers-motion-function #'forward-line
  "The function used internally to move between lines.

It should take one argument, the number of lines to move forward.
Recommended functions are:
 - `forward-line': invisible lines will be counted, counting follows physical
   lines,
 - `forward-visible-line': invisible lines will NOT be counted, counting follows
   physical lines.

Using other functions is possible, but only at one's own risk."
  :type 'function
  :group 'relative-line-numbers)

(defcustom relative-line-numbers-format #'relative-line-numbers-default-format
  "The function used to format the line numbers.
The function should take one integer argument: the line's distance, in
lines, from the current line, and return a string."
  :type 'function
  :group 'relative-line-numbers)

(defgroup relative-line-numbers-faces nil
  "Faces for displaying relative line numbers."
  :group 'relative-line-numbers
  :group 'faces)

(defface relative-line-numbers
  '((t :inherit linum))
  "Face for displaying relative line numbers."
  :group 'relative-line-numbers-faces)

(defface relative-line-numbers-current-line
  '((t :inherit relative-line-numbers))
  "Face for displaying the current line indicator."
  :group 'relative-line-numbers-faces)

(defvar relative-line-numbers--width 0
  "The current left margin width.")
(make-variable-buffer-local 'relative-line-numbers--width)

;;;###autoload
(define-minor-mode relative-line-numbers-mode
  "Display relative line numbers on the left margin.

Toggle Relative Line Numbers on or off.

With a prefix argument ARG, enable Relative Line Numbers mode if ARG
is positive, and disable it otherwise. If called from Lisp, enable the
mode if ARG is omitted or nil, and toggle it if ARG is `toggle'."
  :init-value nil
  :lighter ""
  :keymap nil
  (relative-line-numbers--off)
  (when relative-line-numbers-mode
    (relative-line-numbers--on)))

;;;###autoload
(define-globalized-minor-mode global-relative-line-numbers-mode
  relative-line-numbers-mode
  (lambda ()
    (unless (minibufferp)
      (relative-line-numbers-mode))))

(defun relative-line-numbers-default-format (offset)
  "The default formatting function.
Return the absolute value of OFFSET, converted to string."
  (number-to-string (abs offset)))

(defmacro relative-line-numbers--make-line-overlays (direction limit window)
  "Make the line number overlays for lines before or after point.
DIRECTION is either :forward or :backward.
LIMIT is the buffer position to end the operation when reached.
WINDOW is the window to show overlays in."
  (unless (memq direction '(:forward :backward))
    (error "Direction can be only :forward or :backward"))
  (let ((limitsym (make-symbol "limit"))
        (lineoffsetsym (make-symbol "lineoffset"))
        (windowsym (make-symbol "window")))
    `(let* ((,limitsym ,limit)
            (,lineoffsetsym 0)
            (,windowsym ,window))
       (while ,(if (eq direction :forward)
                   `(and (not (eobp))
                         (< (point) ,limitsym)
                         (or (eq relative-line-numbers-max-count 0)
                             (< ,lineoffsetsym relative-line-numbers-max-count)))
                 `(and (not (bobp))
                       (> (point) ,limitsym)
                       (or (eq relative-line-numbers-max-count 0)
                           (< (- relative-line-numbers-max-count) ,lineoffsetsym))))
         (funcall relative-line-numbers-motion-function
                  ,(if (eq direction :forward) 1 -1))
         (setq ,lineoffsetsym
               (,(if (eq direction :forward)
                     #'1+
                   #'1-)
                ,lineoffsetsym))
         (relative-line-numbers--make-overlay
          (funcall relative-line-numbers-format ,lineoffsetsym)
          'relative-line-numbers
          ,windowsym)))))

(defun relative-line-numbers--update-selected-window ()
  "Update line numbers in the visible portion of the selected window."
  (let ((window (selected-window)))
    (save-excursion
      (let* ((inhibit-point-motion-hooks t)
             (pos (save-excursion
                    (funcall relative-line-numbers-motion-function 0)
                    (point)))
             (start (window-start))
             (end (window-end nil t)))
        (when (and (<= start pos)
                   (<= pos end))
          (relative-line-numbers--delete-window-overlays window)
          (setq relative-line-numbers--width 0)
          (relative-line-numbers--make-line-overlays :forward end window)
          (goto-char pos)
          (relative-line-numbers--make-line-overlays :backward start window)
          (goto-char pos)
          (relative-line-numbers--make-overlay
           (funcall relative-line-numbers-format 0)
           'relative-line-numbers-current-line
           window))))))

(defun relative-line-numbers--update-current-buffer ()
  "Update line numbers in all windows displaying the current buffer."
  (dolist (window (get-buffer-window-list nil nil t))
    (with-selected-window window
      (relative-line-numbers--update-selected-window))))

(defun relative-line-numbers--run-scheduled-update (buffer)
  "Run a scheduled line number update in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when relative-line-numbers-mode
        (relative-line-numbers--update-current-buffer)))))

(defun relative-line-numbers--schedule-current-buffer-update ()
  "Schedule a line number update in the current buffer."
  (run-with-idle-timer relative-line-numbers-delay nil
                       #'relative-line-numbers--run-scheduled-update
                       (current-buffer)))

(defun relative-line-numbers--update-or-schedule-current-buffer ()
  "Update or schedule an update for the current buffer."
  (cond
   ((= relative-line-numbers-delay 0)
    (relative-line-numbers--update-current-buffer))
   (t
    (relative-line-numbers--schedule-current-buffer-update))))

(defun relative-line-numbers--scroll (window _displaystart)
  "Run or schedule a line number update after scrolling."
  (with-selected-window window
    (relative-line-numbers--set-margin-width)
    (when relative-line-numbers-mode
      (relative-line-numbers--update-or-schedule-current-buffer))))

(defvar relative-line-numbers--overlays nil
  "The list of overlays in the current buffer.")
(make-variable-buffer-local 'relative-line-numbers--overlays)

(defun relative-line-numbers--on ()
  "Set up `relative-line-numbers-mode'."
  (add-hook 'post-command-hook #'relative-line-numbers--update-or-schedule-current-buffer nil t)
  (add-hook 'window-configuration-change-hook #'relative-line-numbers--update-or-schedule-current-buffer nil t)
  (add-hook 'window-scroll-functions #'relative-line-numbers--scroll nil t)
  (add-hook 'change-major-mode-hook #'relative-line-numbers--off nil t)
  (relative-line-numbers--update-current-buffer))

(defun relative-line-numbers--off ()
  "Tear down `relative-line-numbers-mode'."
  (remove-hook 'post-command-hook #'relative-line-numbers--update-or-schedule-current-buffer t)
  (remove-hook 'window-configuration-change-hook #'relative-line-numbers--update-or-schedule-current-buffer t)
  (remove-hook 'window-scroll-functions #'relative-line-numbers--scroll t)
  (remove-hook 'change-major-mode-hook #'relative-line-numbers--off t)
  (kill-local-variable 'relative-line-numbers--width)
  (relative-line-numbers--delete-overlays)
  (kill-local-variable 'relative-line-numbers--overlays)
  (relative-line-numbers--set-current-buffer-margin))

(defun relative-line-numbers--set-margin-width ()
  "Set the left margin width to `relative-line-numbers--width'.
If `relative-line-numbers-mode' is off, hide the left margin.
The function operates on the selected window."
  (set-window-margins nil
                      (if relative-line-numbers-mode relative-line-numbers--width nil)
                      (cdr (window-margins))))

(defun relative-line-numbers--set-current-buffer-margin ()
  "Set the left margin width in all windows showing the current buffer."
  (dolist (window (get-buffer-window-list nil nil t))
    (with-selected-window window
      (relative-line-numbers--set-margin-width))))

(defun relative-line-numbers--delete-window-overlays (window)
  "Delete all overlays belonging to WINDOW."
  (let ((it relative-line-numbers--overlays))
    (while it
      (let ((ov (car it)))
        (when (eq window (overlay-get ov 'window))
          (delete-overlay ov)
          (setcar it nil)))
      (setq it (cdr it))))
  (setq relative-line-numbers--overlays (delq nil relative-line-numbers--overlays)))

(defun relative-line-numbers--delete-overlays ()
  "Delete all used overlays."
  (mapc #'delete-overlay relative-line-numbers--overlays)
  (setq relative-line-numbers--overlays nil))

(defun relative-line-numbers--make-overlay (str face window)
  "Make a line number overlay at point.
STR is the string to display, FACE is the face to fontify the string
with, WINDOW is the window the show the overlay in.

This function changes the margin width if STR would not fit."
  (let ((strlen (length str)))
    (when (> strlen relative-line-numbers--width)
      (setq relative-line-numbers--width strlen)
      (relative-line-numbers--set-current-buffer-margin)))
  (let* ((pos (point))
         (overlay (make-overlay pos pos)))
    (overlay-put overlay 'window window)
    (overlay-put overlay 'before-string
                 (propertize " " 'display `((margin left-margin)
                                            ,(propertize str 'face face))))
    (push overlay relative-line-numbers--overlays)))

(provide 'relative-line-numbers)
;;; relative-line-numbers.el ends here
