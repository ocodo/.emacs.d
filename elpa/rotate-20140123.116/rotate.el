;;; rotate.el --- Rotate the layout of emacs

;; Copyright (C) 2013  daic-h

;; Author: daichi.hirata <daichi.hirat at gmail.com>
;; Version: 20140123.116
;; X-Original-Version: 0.0.1
;; Keywords: window, layout
;; URL: https://github.com/daic-h/emacs-rotate

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(eval-when-compile (require 'cl))

(defvar rotate-count 0)

(defvar rotate-functions
  '(rotate:even-horizontal
    rotate:even-vertical
    rotate:main-horizontal
    rotate:main-vertical
    rotate:tiled))

;;;###autoload
(defun rotate-layout ()
  (interactive)
  (let* ((len (length rotate-functions))
         (func (elt rotate-functions (% rotate-count len))))
    (prog1 (message "%s" func)
      (call-interactively func)
      (if (>= rotate-count (- len 1))
          (setq rotate-count 0)
        (incf rotate-count)))))

;;;###autoload
(defun rotate-window ()
  (interactive)
  (let ((wl (reverse (window-list))))
    (rotate:window wl (window-buffer (car wl)))))

;;;###autoload
(defun rotate:even-horizontal ()
  (interactive)
  (rotate:refresh #'rotate:horizontally-n))

;;;###autoload
(defun rotate:even-vertical ()
  (interactive)
  (rotate:refresh #'rotate:vertically-n))

;;;###autoload
(defun rotate:main-horizontal ()
  (interactive)
  (rotate:refresh #'rotate:main-horizontally-n))

;;;###autoload
(defun rotate:main-vertical ()
  (interactive)
  (rotate:refresh #'rotate:main-vertically-n))

;;;###autoload
(defun rotate:tiled ()
  (interactive)
  (rotate:refresh #'rotate:tiled-n))

(defun rotate:main-horizontally-n (num)
  (if (<= num 2)
      (split-window-horizontally
       (floor (* (window-width) (/ 2.0 3.0))))
    (split-window-vertically)
    (other-window 1)
    (rotate:horizontally-n (- num 1))))

(defun rotate:main-vertically-n (num)
  (if (<= num 2)
      (split-window-vertically
       (floor (* (window-height) (/ 2.0 3.0))))
    (split-window-horizontally)
    (other-window 1)
    (rotate:vertically-n (- num 1))))

(defun rotate:horizontally-n (num)
  (if (<= num 2)
      (split-window-horizontally)
    (split-window-horizontally
     (- (window-width) (/ (window-width) num)))
    (rotate:horizontally-n (- num 1))))

(defun rotate:vertically-n (num)
  (if (<= num 2)
      (split-window-vertically)
    (split-window-vertically
     (- (window-height) (/ (window-height) num)))
    (rotate:vertically-n (- num 1))))

(defun rotate:tiled-n (num)
  (cond
   ((<= num 2)
    (split-window-vertically))
   ((<= num 6)
    (rotate:tiled-2column num))
   (t
    (rotate:tiled-3column num))))

(defun rotate:tiled-2column (num)
  (rotate:vertically-n (/ (+ num 1) 2))
  (dotimes (i (/ num 2))
    (split-window-horizontally)
    (other-window 2)))

(defun rotate:tiled-3column (num)
  (rotate:vertically-n (/ (+ num 2) 3))
  (dotimes (i (/ (+ num 1) 3))
    (rotate:horizontally-n 3)
    (other-window 3))
  (when (= (% num 3) 2)
    (other-window -1)
    (delete-window)))

(defun rotate:refresh (proc)
  (let ((window-num (count-windows))
        (buffer-list (mapcar (lambda (wl) (window-buffer wl))
                             (window-list))))
    (when (not (one-window-p))
      (delete-other-windows)
      (save-selected-window
        (funcall proc window-num))
      (loop for wl in (window-list)
            for bl in buffer-list
            do (set-window-buffer wl bl)))))

(defun rotate:window (wl buf)
  (when (not (one-window-p))
    (cond
     ((equal (cdr wl) nil)
      (set-window-buffer (car wl) buf)
      (select-window (car wl)))
     (t
      (set-window-buffer (car wl) (window-buffer (cadr wl)))
      (rotate:window (cdr wl) buf)))))

(provide 'rotate)
;;; rotate.el ends here
