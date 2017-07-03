;;; hl-indent.el --- Highlight irregular indentation. -*- lexical-binding: t -*-
;;
;; Copyright (C) 2014 Kirill Ignatiev <github.com/ikirill>
;;
;; Author: Kirill Ignatiev <github.com/ikirill>
;; Version: 0.1
;; Package-Version: 20170429.1404
;; Keywords: convenience, faces
;; URL: https://github.com/ikirill/hl-indent
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This modes puts indentation highlights below the starting character
;; of a line on subsequent lines, like this:
;;
;; some line
;; |   some other line
;; |   | another line
;; |   | |                  an indented line
;; | fourth line
;; final line
;;
;; This works in every mode, but is more useful in haskell, which
;; doesn't require indentation levels to be at multiples of a specific
;; level.
;;
;; If the minor mode `hl-indent-mode-blocks' is on, this mode will
;; instead highlight blocks of indentation like so:
;;
;; xxxxxxxxxxxxxx
;;   oooooooooooo
;;   oooooooooooo
;;        *******
;;        *******
;;     **********
;;   oooooooooooo
;;     **********
;; xxxxxxxxxxxxxx
;;
;; (where different symbols represent different colours).
;;
;; To use:
;;
;; Enable `hl-indent-mode'.
;;
;; There is also `hl-indent-mode-blocks', but it is less useful
;; because of limited color contrast, depending on face settings.
;;
;; Screenshot:
;;
;; ![Screenshot](screenshot.png "Screenshot")
;;
;; Notes:
;;
;; - You can customize faces `hl-indent-face' (which is `fringe' by
;;   default), and also `hl-indent-block-face-1', from 1 to 6.
;;
;; - To easily see where `hl-indent-mode' puts its highlights, use the
;;   function `hl-indent--debug-faces' together with either
;;   `hl-indent-mode' or `hl-indent-mode-blocks'.
;;
;; - FIXME The mode will refuse to turn on in a very very large file,
;;   because right now it examines every single line once, which can
;;   take too long.
;;
;; - FIXME Indentation highlights override any non-trivial background.
;;   This is a problem for things like comments that might have a
;;   background different from the default background. It also
;;   conflicts with other highlights, like hl-line-mode.
;;
;;; Code:

(require 'cl-lib)

(defgroup hl-indent
  nil
  "Highlight indentation by structure instead of offset."
  :group 'basic-faces
  :prefix "hl-indent")

;; {{{ Face definitions

(defface hl-indent-face
  '((t (:inherit 'fringe)))
  "Face to highlight indentation levels in `hl-indent-mode'."
  :group 'hl-indent)

(defface hl-indent-block-face-1
  '((((background light)) (:background "#ebf5ff"))
    (((background dark)) (:background "#130606"))
    (t (:background "#ebf5ff")))
  "Highlight blocks that are indented by one level"
  :group 'hl-indent)
(defface hl-indent-block-face-2
  '((((background light)) (:background "#e6f3ff"))
    (((background dark)) (:background "#170707"))
    (t (:background "#e6f3ff")))
  "Highlight blocks that are indented by two levels"
  :group 'hl-indent)
(defface hl-indent-block-face-3
  '((((background light)) (:background "#e0f1ff"))
    (((background dark)) (:background "#1a0808"))
    (t (:background "#e0f1ff")))
  "Highlight blocks that are indented by three levels"
  :group 'hl-indent)
(defface hl-indent-block-face-4
  '((((background light)) (:background "#dbeeff"))
    (((background dark)) (:background "#1c0909"))
    (t (:background "#dbeeff")))
  "Highlight blocks that are indented by four levels"
  :group 'hl-indent)
(defface hl-indent-block-face-5
  '((((background light)) (:background "#d6ecff"))
    (((background dark)) (:background "#1f0a0a"))
    (t (:background "#d6ecff")))
  "Highlight blocks that are indented by five levels"
  :group 'hl-indent)
(defface hl-indent-block-face-6
  '((((background light)) (:background "#d1eaff"))
    (((background dark)) (:background "#210a0a"))
    (t (:background "#d1eaff")))
  "Highlight blocks that are indented by six levels"
  :group 'hl-indent)

(defvar hl-indent--block-faces
  (list
   'hl-indent-block-face-1
   'hl-indent-block-face-2
   'hl-indent-block-face-3
   'hl-indent-block-face-4
   'hl-indent-block-face-5
   'hl-indent-block-face-6)
  "List of faces that will be used by hl-indent.")

;; }}}
;; {{{ Debugging functions

(defvar hl-indent--debug-faces nil
  "Toggled by `hl-indent--debug-faces'.")

(defun hl-indent--debug-faces ()
  "Set indentation highlight faces to garish colours."
  (require 'face-remap)
  (with-no-warnings
    (cond
     (hl-indent--debug-faces
      (mapc (lambda (c) (face-remap-remove-relative c)) hl-indent--debug-faces)
      (setq hl-indent--debug-faces nil))
     (t
      (mapc (lambda (s)
              (push (face-remap-add-relative (car s) :background (cdr s))
                    hl-indent--debug-faces))
            '((hl-indent-block-face-1 . "green")
              (hl-indent-block-face-2 . "orange")
              (hl-indent-block-face-3 . "purple")
              (hl-indent-block-face-4 . "blue")
              (hl-indent-block-face-5 . "red")
              (hl-indent-block-face-6 . "yellow")
              (hl-indent-face . "pink")))
      t))))

(defvar hl-indent--debug
  nil
  "Whether hl-indent will print debugging messages.")

(defun hl-indent--debug ()
  (setq hl-indent--debug (not hl-indent--debug))
  (message "hl-indent--debug %s" hl-indent--debug)
  hl-indent--debug)

;; }}}
;; {{{ Variables

(defcustom hl-indent-match-paren
  nil
  "Highlight the column under the matching open parenthesis."
  :group 'hl-indent
  :type '(boolean))

(defvar hl-indent-mode-blocks)

(defvar hl-indent--current-indent
  nil
  "List containing previously encountered indentation levels.

For example, '(5 2 0) means that indentation highlights should be
drawn at 0, 2, and 5 spaces away from line beginning; the current
line will add a new offset if it is to the right of right-most
offset, or delete some offsets if it is to the left of it.")
(make-variable-buffer-local 'hl-indent--current-indent)

(defcustom hl-indent--skip-line-regexp
  "^\\( *$\\|#\\sw\\|\t\\)"
  "Regexp that matches all lines that need to be ignored.

By default, this is empty lines, lines that start with a
TAB (because `hl-indent-mode' fails for TABs, and also for
lines that start with a hash (for c/c++ pragmas).

`hl-indent--skip-comment-lines' allows generic comments to be skipped also."
  :type '(string)
  :safe #'stringp
  :group 'hl-indent)

(defcustom hl-indent--skip-comment-lines
  t
  "Whether comment lines should be skipped when highlighting indentation levels.

This fixes the problem that sometimes `font-lock-comment-face'
has a non-default background, so that a `fringe' face highlight
looks bad."
  :type 'boolean
  :group 'hl-indent)

(defcustom hl-indent-color-indents
  nil
  "Whether indent highlights should be colored or not."
  :type 'boolean
  :group 'hl-indent)

;; }}}
;; {{{ Overlay handling

(defun hl-indent--overlay-hook
    (overlay after-edit change-start change-end &optional prior-length)
  "Modification hook for indentation highlights.
It deletes overlays that start on an empty line.
The arguments are described in `overlay-put'."
  (when hl-indent--debug
    (message "hl-indent--overlay-hook %s %s %d %d %s"
             overlay after-edit change-start change-end prior-length))
  (when (and after-edit (overlay-buffer overlay))
    (save-excursion
      (goto-char (overlay-start overlay))
      (cond
       ((looking-at-p hl-indent--skip-line-regexp)
        (delete-overlay overlay))))))

(defun hl-indent--make-overlay (begin end face &optional category)
  "Construct an indentation highlight overlay.
It will have position (BEGIN, END), face FACE, and CATEGORY
'hl-indent, unless the argument category is
given."
  (unless category (setq category 'hl-indent))
  (let* ((o (make-overlay begin end))
         (face-begin (get-text-property begin 'face))
         (check-face
          (or (save-excursion (goto-char begin) (bolp))
              (memq face-begin '(font-lock-comment-face font-lock-doc-face)))))
    ;; Tabs make overlays far too wide, so set no face
    (overlay-put o 'face (unless check-face face))
    (overlay-put o 'priority 0)
    (overlay-put o 'evaporate t)
    (overlay-put o 'category category)
    (overlay-put o 'modification-hooks '(hl-indent--overlay-hook))
    o))

(defun hl-indent--make-block-highlight (line-start text-start)
  "Construct block highlights on the current line.
The line starts at LINE-START, and text starts at TEXT-START. It
relies on `hl-indent--current-indent' to correctly contain past
indentation levels."
  (let ((begin text-start)
        (end (1+ (line-end-position)))
        (num-faces (length hl-indent--block-faces))
        level)
    (setq level
          (1- (min num-faces (length hl-indent--current-indent))))
    (when (numberp hl-indent-mode-blocks)
      (setq level (- level hl-indent-mode-blocks)))
    (when (>= level 0)
      ;; Create overlays from blocks that started before this line
      (let ((it (cons (- text-start line-start) hl-indent--current-indent))
            (it-level (- (length hl-indent--current-indent) 2)))
        (when (numberp hl-indent-mode-blocks)
          (setq it-level (- it-level hl-indent-mode-blocks)))
        (while (and (>= it-level 0) (and it (cdr it)))
          (let ((x (+ line-start (cadr it)))
                (y (+ line-start (car it)))
                (face-level
                 (min it-level (1- (length hl-indent--block-faces)))))
            ;; (message "Partial block overlay from %d to %d, it-level %d, it %s." x y it-level it)
            (hl-indent--make-overlay
             x y
             (nth face-level hl-indent--block-faces)
             'hl-indent-block)
            (setq it (cdr it))
            (setq it-level (1- it-level)))))
      ;; Create the overlay that covers this line
      ;; (message "hl-indent--make-block-highlight, line-start %d, point %d, current %s, level %d, begin %d, end %d" line-start (point) hl-indent--current-indent level begin end)
      (hl-indent--make-overlay
       begin end
       (nth level hl-indent--block-faces)
       'hl-indent-block))))

(defun hl-indent--clear-all-overlays ()
  "Delete all overlays created by variable `hl-indent-mode'."
  (dolist (c '(hl-indent hl-indent-block))
    (remove-overlays (point-min) (point-max) 'category c)))

;; }}}
;; {{{ Scanning

(defun hl-indent--should-skip-line ()
  "Whether point is at beginning of line that should be skipped completely."
  (or (looking-at-p hl-indent--skip-line-regexp)
      (and hl-indent--skip-comment-lines
           (let ((pps (syntax-ppss (point))))
             (or (nth 3 pps) (nth 4 pps)))
           ;; (save-excursion
           ;;   (skip-syntax-forward "\\s-")
           ;;   (let ((face (get-char-property (point) 'face)))
           ;;     (or (eq face 'font-lock-comment-face)
           ;;         ;; (eq face 'font-lock-comment-delimiter-face)
           ;;         (eq face 'font-lock-doc-face))))
           )))

(defun hl-indent--scan ()
  "Highlight the indentation levels in the entire buffer.
Resets all necessary variables and recomputes all state."
  (let ((hl-indent--debug nil))
    (hl-indent--clear-all-overlays)
    (setq hl-indent--current-indent '(0))
    (save-excursion
      (goto-char (point-min))
      (hl-indent--rescan))))

(defun hl-indent--rescan (&optional stop-soon)
  "Scan this and the following lines.
If STOP-SOON is non-nil, scan the smallest possible number of
following lines, otherwise scan all of the them."
  (while (and (< (point) (point-max)) (hl-indent--should-skip-line))
    (forward-line))
  (let ((just-started t) ; First line is always rescanned
        (keep-going t))
    (when hl-indent--debug
      (message
       "(hl-indent--rescan %s) init: (line %d indent %d current %s)"
       stop-soon (line-number-at-pos (point))
       (hl-indent--line-indent-level) hl-indent--current-indent))
    (while (and (not (eobp))
                (or just-started
                    (not stop-soon)
                    (and keep-going
                         ;; Stop when the next line is not indented
                         (= 0 (syntax-class (syntax-after (point)))))))
      ;; keep-going becomes nil when a line's overlays are not changed.
      (setq keep-going (hl-indent--scan-line stop-soon))
      (when hl-indent--debug
        (message
         "(hl-indent--rescan %s; %s) (line %d indent %d indent %s)"
         stop-soon keep-going (line-number-at-pos (point))
         (hl-indent--line-indent-level) hl-indent--current-indent))
      ;; Move to the next line even if keep-going is nil
      (forward-line)
      (while (and (< (point) (point-max)) (hl-indent--should-skip-line))
        (forward-line))
      (setq just-started nil))))

(defun hl-indent--line-indent-level ()
  "Return the indentation level of the current line."
  (save-excursion
    (beginning-of-line)
    ;; (message "point %d:  %d,  %d:  \"%s\"" (point) (line-end-position) (1- (line-beginning-position 2)) (buffer-substring (point) (min (line-end-position) (+ 4 (point) (save-excursion (skip-syntax-forward " " (line-end-position)))))))
    (skip-syntax-forward " " (line-end-position))))

(defun hl-indent--face-for-level (level)
  (intern (format "hl-indent-block-face-%s"
                  (1+ (mod (cl-position level (reverse hl-indent--current-indent))
                           (length hl-indent--current-indent))))))

(defun hl-indent--scan-line (&optional stop-soon)
  "Highlight indentation levels on the current line.
The variable `hl-indent--current-indent' will contain indentation levels
from this and previous lines so that `hl-indent--scan-line' will work
correctly on the next line."
  (let (line-start text-start next-line-start prev-levels)
    (beginning-of-line)
    (setq line-start (point))
    (skip-syntax-forward " " (line-end-position))
    (setq text-start (point))
    (setq next-line-start (line-beginning-position 2))
    (when stop-soon (setq prev-levels (hl-indent--find-indent-levels)))
    (dolist (c '(hl-indent hl-indent-block))
      (remove-overlays line-start next-line-start 'category c))
    (let ((this-level (- text-start line-start)))
      ;; Discard all levels >= this one
      (while (and hl-indent--current-indent (>= (car hl-indent--current-indent) this-level))
        (setq hl-indent--current-indent (cdr hl-indent--current-indent)))
      (dolist (level hl-indent--current-indent)
        (let* ((pos (+ line-start level))
               (face (if hl-indent-color-indents
                         (hl-indent--face-for-level level)
                       'hl-indent-face))
               (o (hl-indent--make-overlay pos (1+ pos) face)))
          (when hl-indent-mode-blocks
            (overlay-put o 'face nil))))
      (when (and hl-indent-mode-blocks
                 (< line-start text-start))
        (hl-indent--make-block-highlight line-start text-start))
      (push this-level hl-indent--current-indent)
      (when hl-indent--debug
        (message "Line %d: level %d, current-indent: %s, prev: %s"
                 (line-number-at-pos (point)) this-level
                 hl-indent--current-indent prev-levels))
      ;; It is okay to stop soon when we didn't make any changes
      (or (not stop-soon) (not (equal prev-levels hl-indent--current-indent))))))

(defun hl-indent--find-indent-levels ()
  "Return indentation levels of hhi overlays present on the current line."
  (let (begin end overlays levels)
    (save-excursion
      (beginning-of-line)
      (setq begin (point))
      (skip-syntax-forward " " (line-end-position))
      (setq end (point))
      (setq overlays
            (cl-remove-if
             (lambda (o) (not (eq 'hl-indent (overlay-get o 'category))))
             (overlays-in begin end)))
      (setq levels (mapcar (lambda (o) (- (overlay-start o) begin)) overlays))
      (setq levels (sort (cons (- end begin) levels) '>=))
      (when hl-indent--debug
        (message "indent-levels(%d): %s" (line-number-at-pos) levels))
      ;; (message "Overlays (%d, %d): %s <- %s" begin end levels overlays)
      levels)))

(defun hl-indent--rescan-line ()
  "Examine the previous line for indentation levels, then rescan.
The point is left at the beginning of the first line that was not scanned."
  (setq hl-indent--current-indent nil)
  (save-excursion
    (forward-line -1)
    ;; Empty lines can occur in middle of indentation.
    (while (and (> (point) (point-min)) (hl-indent--should-skip-line))
      (forward-line -1))
    (setq hl-indent--current-indent (hl-indent--find-indent-levels)))
  (when hl-indent--debug
    (message "hl-indent--rescan-line(%d): found overlays at %s"
             (point) hl-indent--current-indent))
  (beginning-of-line)
  (hl-indent--rescan t))

;; }}}
;; {{{ Hooks

(defun hl-indent--hook (change-start change-end prior-length)
  "Rescan for changes in indentation.
This hook should be addded to `after-change-functions', and
expects arguments CHANGE-START, CHANGE-END and PRIOR-LENGTH to be
passed to it by `after-change-functions'."
  ;; It is important to some other modes like dabbrev that hooks don't
  ;; modify match data (and we don't even want to).
  (save-match-data
    (save-excursion
      (goto-char change-start)
      (beginning-of-line)
      (let* ((text-start (save-excursion
                           (skip-syntax-forward " " (line-end-position))
                           (point)))
             (scan-end (max (save-excursion (goto-char change-end)
                                            (line-beginning-position 2))
                            (+ prior-length change-end))))
        ;; If change-start is after text-start, then indentation has
        ;; not changed.
        (when (or (<= change-start text-start)
                  (>= (+ prior-length change-end)
                      (line-beginning-position 2))
                  (looking-at-p hl-indent--skip-line-regexp))
          (when hl-indent--debug
            (message
             "(hl-indent--hook %s %s %s -> %d) (text-start %s point %s)"
             change-start change-end prior-length scan-end text-start (point)))
          (hl-indent--rescan-line)
          (while (and (not (eobp)) (<= (point) scan-end))
            (hl-indent--rescan t))
          (when hl-indent--debug
            (message "Scanned %d lines"
                     (- (line-number-at-pos (point))
                        (line-number-at-pos change-start)))))))))

(defun hl-indent--match-paren-hook ()
  "Hook that highlights the column under matching paren in a sexp.
This goes into `post-command-hook'. It uses `backward-sexp' to
find the match."
  (remove-overlays
   (window-start) (window-end) 'category 'hl-indent-match-paren)
  (let (;; The point where we highlight the last line.
        (end-line-pos (line-beginning-position))
        column
        (open-paren (save-excursion (nth 1 (syntax-ppss (point)))))
        p)
    ;; Find the inner-most sexp that starts to the left of the current
    ;; indentation level. Since hl-indent skips tabs, we go back even
    ;; further if the inner-most sexp was skipped due to line starting
    ;; with a tab, going to the inner-most sexp starting on a
    ;; different line that starts with not a tab.
    (while (and open-paren
                (or (>= open-paren end-line-pos)
                    (save-excursion (goto-char open-paren)
                                    (beginning-of-line)
                                    (= (char-after) ?\t))))
      (setq open-paren (save-excursion (nth 1 (syntax-ppss open-paren)))))
    (when (and hl-indent-match-paren open-paren)
      (save-excursion
        (goto-char open-paren)
        (setq column (current-column))
        (while (and (= 0 (forward-line))
                    (<= (point) end-line-pos))
          (setq p (syntax-ppss (point)))
          (when (and (> (line-end-position) (+ (point) column))
                     (/= (char-after) ?\t)
                     ;; No highlighting inside a string/comment.
                     (not (or (nth 3 p) (nth 4 p))))
            (let* ((pos (+ (point) column))
                   (o (make-overlay pos (1+ pos))))
              (overlay-put o 'category 'hl-indent-match-paren))))))))

;; }}}
;; {{{ Modes

(define-minor-mode hl-indent-mode
  "Automatically highlights indentation levels by structure.

This modes puts indentation highlights below the starting character
of a line on subsequent lines, like this:

some line
|   some other line
|   | another line
|   | |                  an indented line
| fourth line
final line

This works in every mode, but is more useful in haskell, which
doesn't require indentation levels to be at multiples of a specific
level.

If the minor mode `hl-indent-mode-blocks' is on, this mode will
instead highlight blocks of indentation like so:

xxxxxxxxxxxxxx
  oooooooooooo
  oooooooooooo
       *******
       *******
    **********
  oooooooooooo
    **********
xxxxxxxxxxxxxx

where different symbols represent different colours.

To use:

Enable `hl-indent-mode'.

There is also `hl-indent-mode-blocks', but it is less useful
because of limited color contrast, depending on face settings.

Notes:

- You can customize faces `hl-indent-face' (which is `fringe' by
  default), and also `hl-indent-block-face-1', from 1 to 6.

- The mode will refuse to turn on in a very very large file,
  because right now it examines every single line once, which can
  take too long."
  :group 'hl-indent
  (put 'hl-indent-match-paren 'priority 100)
  (put 'hl-indent-match-paren 'face 'show-paren-match)
  (when (and hl-indent-mode (> (point-max) 100000))
    (message "hl-indent-mode not turned on: file too large")
    (setq hl-indent-mode nil))
  (cond
   (hl-indent-mode
    (add-hook 'after-change-functions 'hl-indent--hook t t)
    (add-hook 'post-command-hook 'hl-indent--match-paren-hook t t)
    (hl-indent--scan)
    )
   (t
    (hl-indent--clear-all-overlays)
    (remove-hook 'after-change-functions 'hl-indent--hook t)
    (remove-hook 'post-command-hook 'hl-indent--match-paren-hook t))))

(define-minor-mode hl-indent-mode-blocks
  "In `hl-indent-mode' highlight blocks of code instead of indentation levels."
  :group 'hl-indent
  (when (or hl-indent-mode-blocks hl-indent-mode)
    (hl-indent-mode)))

;; }}}

(provide 'hl-indent)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:
;;; hl-indent.el ends here
