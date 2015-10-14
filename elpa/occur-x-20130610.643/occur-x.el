;;; occur-x.el --- Extra functionality for occur

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Juan-Leon Lahoz <juanleon1@gmail.com>
;; Keywords: occur, search, convenience
;; Package-Version: 20130610.643
;; Version: 0.1.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; occur-x.el adds some extra functionality to occur-mode.  It allows the
;; user to refine any occur mode with extra regexp based filters.  Use
;; commands `occur-x-filter-out' and `occur-x-filter' to add positive and
;; negative filters.  By default those commands are bind to keys "f" and
;; "k" (from flush and keep).  Use command `occur-x-undo-filter' to remove
;; filters.  Filters are kept if the buffer is reverted (shortcut "g") or
;; cloned (shortcut "c").

;; Another useful addition of occur-x to occur-mode is the possibility to
;; displaying the line numbers in the margin of your choice, instead of
;; "inside" the occur buffer.  This way every match line in the occur
;; buffer is exactly the same as in the original buffer.  Customize
;; variable `occur-linenumbers-in-margin' and face `occur-margin-face' to
;; your liking.  When displayed in the margin, line numbers won't interfere
;; with the regexps of the additional filters.

;;; Usage

;; Put this file in your load-path and add this lines to your init file:

;; (require 'occur-x)
;; (add-hook 'occur-mode-hook 'turn-on-occur-x-mode)

;;; Feedback

;; Bugs reports, comments, ideas, etc. welcomed.

;; https://github.com/juan-leon/occur-x

;;; Code:

(defface occur-margin-face
  '((t :inverse-video nil :underline nil :weight normal
       :inherit (fringe shadow)))
  "Face for displaying line numbers in the margin."
  :group 'matching)

(defcustom occur-linenumbers-in-margin left-margin
  "*Control where the line numbers are displayed in occur-mode.

Non-nil means display line numbers in left margin, unless special
value `right-margin' is used.  When this variable to nil, line
numbers will be inserted into the occur buffer."
  :type '(choice (const :tag "Left margin" left-margin)
                 (const :tag "Right margin" right-margin)
                 (const :tag "Buffer" nil))
  :group 'matching)

(defvar occur-x-filter-ops nil
  "Extra filters applied to an occur buffer to refine matches")

(defvar occur-x-original nil
  "Original occur buffer for a `clone-buffer' operation")

(defvar occur-x-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "f" 'occur-x-filter-out)
    (define-key m "k" 'occur-x-filter)
    (define-key m "u" 'occur-x-undo-filter)
    m)
  "Keymap for `occur-x--mode'.")

;;;###autoload
(define-minor-mode occur-x-mode
  "Add some extra functionality to occur-mode.

User can refine the occur matches with any number of extra regexp
based filters.

Also, the line numbers are displayed in the margin of your
choice, instead of inside the occur buffer.  This way every match
line in the occur buffer is exactly the same as in the original
buffer.  See variable `occur-linenumbers-in-margin' and face
`occur-margin-face'.  When displayed in the margin, line numbers
won't interfere with the regexps of the additional filters.

\\{occur-x-mode-map}"
  :init-value nil
  :keymap 'occur-x-mode-map
  :group 'matching
  :lighter "-x"
  (if occur-x-mode
      (add-hook 'occur-hook 'occur-x-init nil t)
    (remove-hook 'occur-hook 'occur-x-init t)))

;;;###autoload
(defun turn-on-occur-x-mode ()
  "Turn on occur-x-mode unconditionally."
  (interactive)
  (occur-x-mode 1))

(defun occur-x-filter (regexp)
  "Add a regexp based filter to the occur buffer.

The filter will delete all lines except those containing matches
for REGEXP.  See `keep-lines' for more information about how
REGEXP is used.

Filter will remain if buffer is reverted or cloned, and it can be
un-applied with `occur-x-undo-filter'."
  (interactive (list (read-regexp "Keep lines matching regexp"
                                  (car regexp-history))))
  (occur-x--apply-filter regexp 'occur-x--keep-lines))

(defun occur-x-filter-out (regexp)
  "Add a regexp based filter to the occur buffer.

The filter will delete all lines that contain matches for REGEXP.
See `flush-lines' for more information about how REGEXP is used.

Filter will remain if buffer is reverted or cloned, and it can be
un-applied with `occur-x-undo-filter'."
  (interactive (list (read-regexp "Remove lines matching regexp"
                                  (car regexp-history))))
  (occur-x--apply-filter regexp 'occur-x--flush-lines))


(defun occur-x-undo-filter ()
  "Removes the last filter added to this occur buffer.

The filters are stored in a stack, so additional invocations of
this command will remove additional filters."
  (interactive)
  (when occur-x-filter-ops
    (pop occur-x-filter-ops)
    (occur-revert-function nil nil)))

(defun occur-x-init ()
  (unless occur-x-filter-ops
    (set (make-local-variable 'occur-x-filter-ops) nil))
  (set (make-local-variable 'occur-x-original) (current-buffer))
  (add-hook 'clone-buffer-hook 'occur-x--clone nil t)
  ;; When re-running occur, old overlays are piled in pos 1
  (occur-x--remove-overlays 1)
  (if occur-linenumbers-in-margin
      (occur-x--linenums-to-margin)))


(defun occur-x--apply-filters (filters)
  (let (op)
    (while (setq op (pop filters))
      (occur-x--apply-filter (car op) (cdr op)))))


(defun occur-x--flush-lines (regexp p1 p2)
  (goto-char p1)
  (forward-line 1)
  (setq p2 (copy-marker p2))
  (while (and (< (point) p2)
              (re-search-forward regexp p2 t))
    (goto-char (match-beginning 0))
    (forward-line 0)
    (occur-x--remove-overlays (point))
    (delete-region (point) (progn (forward-line 1) (point))))
  (set-marker p2 nil))

(defun occur-x--keep-lines (regexp p1 p2)
  (goto-char p1)
  (forward-line 1)
  (setq p2 (copy-marker p2))
  (while (< (point) p2)
    (if (re-search-forward regexp (save-excursion (forward-line 1)
                                                  (point)) t)
        (forward-line 1)
      (forward-line 0)
      (occur-x--remove-overlays (point))
      (delete-region (point) (progn (forward-line 1) (point)))))
  (set-marker p2 nil))


(defun occur-x--apply-filter (regexp func)
  (if (cadr occur-revert-arguments)
      (error "Additional filters do not work with context lines."))
  (save-excursion
    (let ((inhibit-read-only t)
          (posl (occur-x--titles-pos))
          (p2 (point-max))
          p1)
      (while (setq p1 (pop posl))
        (apply func regexp p1 p2 nil)
        (setq p2 p1)))
    (occur-x--remove-overlays (point-max))
    (occur-x--update-counts))
  (push (cons regexp func) occur-x-filter-ops))


(defun occur-x--update-counts ()
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (posl (occur-x--titles-pos))
        (p2 (point-max))
        (grand-total 0)
        p1)
    (while (setq p1 (pop posl))
      (let* ((n (count-lines (save-excursion
                               (goto-char p1)
                               (forward-line 1)
                               (point)) p2)))
        (setq p2 p1)
        (goto-char p2)
        (forward-word 1)
        (let ((p (point)))
          (insert-and-inherit (format "%s" n))
          (delete-region p2 p))
        (setq grand-total (+ grand-total n))
        ;; Delete titles where there are no results, but only when
        ;; multiple buffers were scanned
        (when (and (<= n 0)
                   (> (length (nth 2 occur-revert-arguments)) 1))
          (forward-line 1)
          (delete-region p2 (point)))))
    (when (< 1 (length (caddr occur-revert-arguments)))
      ;; A summary line is present if multiple buffers were scanned
      (goto-char (point-min))
      (forward-word 1)
      (let ((p (point)))
        (insert-and-inherit (format "%s" grand-total))
        (delete-region (point-min) p)))))


(defun occur-x--titles-pos ()
  "Return the position of the titles in the occur buffer."
  (goto-char (point-min))
  (let ((l nil))
    (if (< 1 (length (caddr occur-revert-arguments)))
        (forward-line 1))
    (while (not (eobp))
      (push (point) l)
      (forward-line 1)
      (if (not (get-text-property (point) 'occur-title))
          (let ((p (next-single-property-change (point) 'occur-title)))
            (goto-char (or p (point-max))))))
    l))

(defun occur-x--clone ()
  ;; Overlays are not cloned so...
  (let ((overlays (with-current-buffer occur-x-original
                    (overlays-in (point-min) (point-max)))))
    (setq occur-x-original (current-buffer))
    (dolist (o overlays)
      (when (overlay-get o 'before-string)
        (move-overlay (copy-overlay o) (overlay-start o)
                      (overlay-end o) (current-buffer))))))

(defadvice occur-revert-function (around occur-x-extra-filters activate)
  "When `occur-x-mode' is active, re-apply filters after reverting."
  (if occur-x-mode
      (let ((filters (reverse occur-x-filter-ops)))
        ad-do-it
        (occur-x--apply-filters filters))
    ad-do-it))


(defun occur-x--remove-overlays (p)
  (mapc #'(lambda (o) (if (overlay-get o 'before-string)
                          (delete-overlay o)))
        (overlays-in p p)))


(defun occur-x--set-margin (&optional width)
  (when (not width)
    (setq width 0)
    (goto-char (point-max))
    (while (not (bobp))
      (if (re-search-backward "^\s*\\([0-9]+\\):" 1 1)
          (setq width (max width
                           (- (match-end 1)
                              (match-beginning 1))))
        (goto-char (point-min)))
      (goto-char (previous-single-property-change
                  (point) 'occur-title nil 1))
      (forward-line -1)))
  (and (bound-and-true-p linum-mode) (linum-mode -1))
  (if (equal occur-linenumbers-in-margin 'right-margin)
      (setq right-margin-width width)
    (setq left-margin-width width))
  (set-window-buffer (get-buffer-window) (current-buffer)))


(defun occur-x--linenums-to-margin()
  (save-excursion
    (occur-x--set-margin)
    (goto-char (point-min))
    (forward-line 1)
    (let ((inhibit-read-only t)
          (context (cadr occur-revert-arguments))
          width side)
      (if (equal occur-linenumbers-in-margin 'right-margin)
          (setq width right-margin-width
                side 'right-margin)
        (setq width left-margin-width
              side 'left-margin))
      (while (not (eobp))
        (if (looking-at "^\s*\\([0-9]+\\):")
            (let ((n (propertize
                      (format (format "%%%ds" width) (match-string 1))
                      'face 'occur-margin-face))
                  (o (make-overlay (point) (point))))
              (delete-region (point) (match-end 0))
              (overlay-put o 'before-string
                           (propertize " " 'display
                                       `((margin ,side) ,n))))
          (if (and context (looking-at "^\s+:"))
              (delete-region (point) (match-end 0))))
        (forward-line 1)))))


;; Code below this point is sort of a hack... Since `occur-edit-mode'
;; relies heavily in column information present in the buffer, we need to
;; put it back.  And since `occur-hook' is not called when exiting, we need
;; an advice to restore column information where it belongs

(defadvice occur-edit-mode (around preserve-filters activate)
  "When `occur-x-mode' is active, re-apply filters after reverting."
  (if occur-x-mode
      (let ((filters (reverse occur-x-filter-ops)))
        (when occur-linenumbers-in-margin
          (let ((occur-linenumbers-in-margin nil))
            (revert-buffer)
            (occur-x--set-margin 0)))
        ad-do-it
        (occur-x--apply-filters filters))
    ad-do-it))


(defadvice occur-cease-edit (after occur-x-cease-edit activate)
  (if occur-x-mode
      (occur-x-init)))

;;; Bugs and limitations:

;; The extra filter functionality cannot be used with "context" lines

;;; Change-Log:

;; see https://github.com/juan-leon/occur-x

(provide 'occur-x)
;;; occur-x.el ends here
