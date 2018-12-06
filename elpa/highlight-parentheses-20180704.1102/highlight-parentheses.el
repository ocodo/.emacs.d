;;; highlight-parentheses.el --- highlight surrounding parentheses
;;
;; Copyright (C) 2007, 2009, 2013 Nikolaj Schumacher
;; Copyright (C) 2018 Tim Perkins
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Maintainer: Tassilo Horn <tsdh@gnu.org>
;; Version: 1.1.0
;; Package-Version: 20180704.1102
;; Keywords: faces, matching
;; URL: https://github.com/tsdh/highlight-parentheses.el
;;      http://nschum.de/src/emacs/highlight-parentheses/ (old website)
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x, GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
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
;; Add the following to your .emacs file:
;; (require 'highlight-parentheses)
;;
;; Enable the mode using M-x highlight-parentheses-mode or by adding it to a
;; hook.
;;
;;; Code:

(eval-when-compile (require 'cl))

(defgroup highlight-parentheses nil
  "Highlight surrounding parentheses"
  :group 'faces
  :group 'matching)

(defun hl-paren-set (variable value)
  (set variable value)
  (when (fboundp 'hl-paren-color-update)
    (hl-paren-color-update)))

(defcustom hl-paren-colors
  '("firebrick1" "IndianRed1" "IndianRed3" "IndianRed4")
  "List of colors for the highlighted parentheses.
The list starts with the inside parentheses and moves outwards."
  :type '(choice (repeat color) function)
  :set 'hl-paren-set
  :group 'highlight-parentheses)

(defcustom hl-paren-background-colors nil
  "List of colors for the background highlighted parentheses.
The list starts with the inside parentheses and moves outwards."
  :type '(choice (repeat color) function)
  :set 'hl-paren-set
  :group 'highlight-parentheses)

(defcustom hl-paren-attributes nil
  "List of face attributes for the highlighted parentheses.
The list starts with the inside parentheses and moves outwards."
  :type '(choice plist function)
  :set 'hl-paren-set
  :group 'highlight-parentheses)

(defcustom hl-paren-highlight-adjacent nil
  "Highlight adjacent parentheses, just like show-paren-mode."
  :type '(boolean)
  :set 'hl-paren-set
  :group 'highlight-parentheses)

(defface hl-paren-face nil
  "Face used for highlighting parentheses.
Color attributes might be overriden by `hl-paren-colors' and
`hl-paren-background-colors'."
  :group 'highlight-parentheses)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hl-paren-overlays nil
  "This buffers currently active overlays.")
(make-variable-buffer-local 'hl-paren-overlays)

(defvar hl-paren-last-point 0
  "The last point for which parentheses were highlighted.
This is used to prevent analyzing the same context over and over.")
(make-variable-buffer-local 'hl-paren-last-point)

(defvar hl-paren-timer nil
  "A timer initiating the movement of the `hl-paren-overlays'.")
(make-variable-buffer-local 'hl-paren-timer)

(defun* hl-paren-delete-overlays (&optional (overlays hl-paren-overlays))
  (mapc #'delete-overlay overlays))

(defun hl-paren-highlight ()
  "Highlight the parentheses around point."
  (unless (= (point) hl-paren-last-point)
    (setq hl-paren-last-point (point))
    (let ((overlays hl-paren-overlays)
          pos1 pos2)
      (save-excursion
        (ignore-errors
          (when hl-paren-highlight-adjacent
            (cond ((memq (preceding-char) '(?\) ?\} ?\] ?\>))
                   (backward-char 1))
                  ((memq (following-char) '(?\( ?\{ ?\[ ?\<))
                   (forward-char 1))))
          (while (and (setq pos1 (cadr (syntax-ppss pos1)))
                      (cdr overlays))
            (move-overlay (pop overlays) pos1 (1+ pos1))
            (when (setq pos2 (scan-sexps pos1 1))
              (move-overlay (pop overlays) (1- pos2) pos2)))))
      (hl-paren-delete-overlays overlays))))

(defcustom hl-paren-delay 0.137
  "Fraction of seconds after which the `hl-paren-overlays' are adjusted.
In general, this should at least be larger than your keyboard
repeat rate in order to prevent excessive movements of the
overlays when scrolling or moving point by pressing and holding
\\[next-line], \\[scroll-up-command] and friends."
  :type 'number
  :group 'highlight-parentheses)

(defun hl-paren-initiate-highlight ()
  "Move the `hl-paren-overlays' after a `hl-paren-delay' secs."
  (when hl-paren-timer
    (cancel-timer hl-paren-timer))
  (setq hl-paren-timer (run-at-time hl-paren-delay nil #'hl-paren-highlight)))

;;;###autoload
(define-minor-mode highlight-parentheses-mode
  "Minor mode to highlight the surrounding parentheses."
  nil " hl-p" nil
  (hl-paren-delete-overlays)
  (kill-local-variable 'hl-paren-overlays)
  (kill-local-variable 'hl-paren-last-point)
  (remove-hook 'post-command-hook 'hl-paren-initiate-highlight t)
  (remove-hook 'before-revert-hook 'hl-paren-delete-overlays)
  (remove-hook 'change-major-mode-hook 'hl-paren-delete-overlays)
  (when (and highlight-parentheses-mode
             ;; Don't enable in *Messages* buffer.
             ;; https://github.com/tsdh/highlight-parentheses.el/issues/14
             (not (eq major-mode 'messages-buffer-mode))
             (not (string= (buffer-name) "*Messages*")))
    (hl-paren-create-overlays)
    (add-hook 'post-command-hook 'hl-paren-initiate-highlight nil t)
    (add-hook 'before-revert-hook 'hl-paren-delete-overlays)
    (add-hook 'change-major-mode-hook 'hl-paren-delete-overlays)))

;;;###autoload
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda () (highlight-parentheses-mode 1)))

;;; overlays ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hl-paren-create-overlays ()
  (let ((fg (if (functionp hl-paren-colors)
                (funcall hl-paren-colors)
              hl-paren-colors))
        (bg (if (functionp hl-paren-background-colors)
                (funcall hl-paren-background-colors)
              hl-paren-background-colors))
        (attr (if (functionp hl-paren-attributes)
                  (funcall hl-paren-attributes)
                hl-paren-attributes))
        attributes)
    (while (or fg bg attr)
      (setq attributes (face-attr-construct 'hl-paren-face))
      (let ((car-fg (car fg))
            (car-bg (car bg))
            (car-attr (car attr)))
        (loop for (key . (val . _rest)) on car-attr by #'cddr
              do (setq attributes
                       (plist-put attributes key val)))
        (when car-fg
          (setq attributes (plist-put attributes :foreground car-fg)))
        (when car-bg
          (setq attributes (plist-put attributes :background car-bg))))
      (pop fg)
      (pop bg)
      (pop attr)
      (dotimes (i 2) ;; front and back
        (push (make-overlay 0 0 nil t) hl-paren-overlays)
        (overlay-put (car hl-paren-overlays) 'font-lock-face attributes)))
    (setq hl-paren-overlays (nreverse hl-paren-overlays))))

(defun hl-paren-color-update ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when hl-paren-overlays
        (hl-paren-delete-overlays)
        (setq hl-paren-overlays nil)
        (hl-paren-create-overlays)
        (let ((hl-paren-last-point -1)) ;; force update
          (hl-paren-highlight))))))

(provide 'highlight-parentheses)

;;; highlight-parentheses.el ends here
