;;; nurumacs.el --- smooth-scrolling and minimap, like sublime editor

;; Copyright (C) 2013 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 3.5.4

;;; Commentary:

;; Just add an expression below in your init file :
;;
;;   (require 'nurumacs)
;;
;; then scrolling is automatically animated.

;; When you load this library, "auto-hscroll-mode" is automatically disabled,
;; and nurumacs-auto-hscroll will work instead. To disable this, evaluate
;;
;;   (setq nurumacs-auto-hscroll nil)
;;
;; and to use original auto-hscroll again, evaluate
;;
;;   (setq auto-hscroll-mode t)

;; An experimental minimap feature is also available. When you set "nurumacs-map" true,
;;
;;   (setq nurumacs-map t)
;;
;; then a minimap appears on every scrolling. If you want to toggle minimap manually,
;; call command "M-x nurumacs-toggle". This command works even when nurumacs-map is nil.

;;; Change Log:

;; 1.0.0 first released
;; 2.0.0 added horizontal animation
;;       now not depends on "pager.el"
;; 2.1.0 improved horizontal scrolling
;; 3.0.0 added minimap-like feature
;;       some minor fixes
;; 3.0.1 minor fizes
;; 3.2.0 improved post-command-function
;; 3.2.1 fixed bug that hscroll occurs
;;       even when the line is truncated
;; 3.3.0 modified trigger of minimap clean-up
;; 3.4.0 added nurumacs-map-hook
;;       added nurumacs-map-toggle
;; 3.4.1 fixed bug that
;;       o minimap for "popwin" is not killed
;;       o hscroll sometimes not works
;; 3.5.0 now works even when mark is active
;; 3.5.1 fix for nurumacs-map-toggle
;; 3.5.2 fixed bug that "lambda" command crush nurumacs
;; 3.5.3 now minimap is disabled by default
;;       changed default hscroll speed
;; 3.5.4 WORKING...

;;; Code:

;; * constants

(defconst nurumacs-version "3.5.4")

;; * customizable vars
;; ** vscroll

(defvar nurumacs-vdecc 1.7
  "how vertical scroll speed goes down")

(defvar nurumacs-vspeeds '(1000 300 100 50 7 3 2)
  "speeds of vertical scrolling animation")

;; ** hscroll

(defvar nurumacs-hdecc 1.5
  "how horizontal scroll speed goes down")

(defvar nurumacs-hspeeds '(400 200 10 3)
  "speeds of horizontal scrolling animation")

;; ** minimap

(defvar nurumacs-map nil
  "if non-nil minimap is enabled")

(defvar nurumacs-map-size 20
  "width of minimap")

(defvar nurumacs-map-fraction 0.3
  "maximum fraction of minimap width")

(defvar nurumacs-map-delay 3
  "wait before minimap clean-up")

(defvar nurumacs-map-kill-commands
  '(split-window-horizontally split-window-vertically)
  "commands that minimap must be killed before execution")

(defvar nurumacs-map-hook nil
  "hooks that are called just after minimap is activated
you may assume (selected-window) and (current-buffer) are minimap")

;; * replace auto-hscroll-mode

(defvar nurumacs-auto-hscroll auto-hscroll-mode)
(setq auto-hscroll-mode nil)

;; * utils

(defun nurumacs--scroll-up (lines)
  "for animation use only (cursor position may be lost)"
  (goto-char (window-start))
  (forward-line lines)
  (set-window-start (selected-window) (point)))

(defun nurumacs--scroll-down (lines)
  "for animation use only (cursor position may be lost)"
  (nurumacs--scroll-up (- lines)))

;; * vscroll

(defun nurumacs--vscroll-effect (len)
  "animate as if the window is scrolled from LEN lines upward."
  (save-excursion
    (let* ((fun (if (>= len 0) 'nurumacs--scroll-up 'nurumacs--scroll-down))
           (speeds nurumacs-vspeeds)
           (len (abs len))
           (cursor-type nil))
      (funcall fun (- len))
      (dolist (spd speeds)
        (while (>= len (floor (* nurumacs-vdecc spd)))
          (funcall fun spd) (setq len (- len spd)) (redisplay t)))
      (dotimes (tmp len)
        (funcall fun 1) (redisplay t)))))

;; * hscroll

(defun nurumacs--hscroll (len)
  "scroll LEN chars horizontally, with animation"
  (save-excursion
    (let* ((fun (if (>= len 0) 'scroll-left 'scroll-right))
           (speeds nurumacs-hspeeds)
           (len (abs len)))
      (dolist (spd speeds)
        (while (>= len (floor (* nurumacs-hdecc spd)))
          (funcall fun spd) (setq len (- len spd)) (redisplay t)))
      (dotimes (tmp len)
        (funcall fun 1) (redisplay t)))))

;; * map

(defvar nurumacs--map-window nil)
(defvar nurumacs--map-buffer nil)
(defvar nurumacs--map-killer nil)

(defun nurumacs--map-show ()
  "show minimap for the current bufer, and set clean-up timer"

  ;; search for the minimap window
  (setq nurumacs--map-window
        (cond ((window-live-p nurumacs--map-window)
               nurumacs--map-window)
              ((<= (/ nurumacs-map-size (window-width) 1.0)
                   nurumacs-map-fraction)
               (split-window (selected-window)
                             (- (window-width) nurumacs-map-size)
                             t))
              (t nil)))

  ;; setup minimap
  (when nurumacs--map-window
    (let ((text (buffer-string)) (point (point)) (beg (window-start))
          (end (window-end (selected-window) 'update)))
      (with-selected-window nurumacs--map-window
        ;; kill old buffer
        (when (buffer-live-p nurumacs--map-buffer)
          (kill-buffer nurumacs--map-buffer))
        ;; generate new buffer and configure
        (switch-to-buffer
         (setq nurumacs--map-buffer (generate-new-buffer "*nurumap*")))
        (text-scale-set -7)
        (run-hooks 'nurumacs-map-hook)
        ;; yank content
        (insert text)
        (goto-char point)
        (recenter)
        ;; put overlay
        (overlay-put (make-overlay beg end) 'face 'highlight)))
    ;; set a clean-up timer
    (when nurumacs--map-killer (cancel-timer nurumacs--map-killer))
    (setq nurumacs--map-killer
          (run-with-idle-timer nurumacs-map-delay t 'nurumacs--map-kill))))

(defun nurumacs--map-kill ()
  ;; cancel timer
  (when nurumacs--map-killer
    (cancel-timer nurumacs--map-killer)
    (setq nurumacs--map-killer nil))
  ;; delete window
  (when (window-live-p nurumacs--map-window)
    (delete-window nurumacs--map-window))
  ;; kill buffer
  (when (buffer-live-p nurumacs--map-buffer)
    (kill-buffer nurumacs--map-buffer))
  ;; finish this session
  (when (eq nurumacs-map 'temporally)
    (setq nurumacs-map nil)))

(defun nurumacs-map-toggle ()
  "toggle minimap immediately. this command works even when \"nurumacs-map\" is nil"
  (interactive)
  (if (and (window-live-p nurumacs--map-window)
           (buffer-live-p nurumacs--map-buffer))
      (nurumacs--map-kill)
    (setq nurumacs-map 'temporally)
    (nurumacs--map-show)))

;; * trigger

(defvar nurumacs--prev-lin nil)
(defvar nurumacs--prev-buf nil)
(defvar nurumacs--prev-wnd nil)

(defun nurumacs--pre-command-function ()
  (when (and (symbolp this-command)
             (member this-command nurumacs-map-kill-commands))
    (nurumacs--map-kill))
  (setq nurumacs--prev-lin (line-number-at-pos (window-start)))
  (setq nurumacs--prev-buf (current-buffer))
  (setq nurumacs--prev-wnd (selected-window)))

(defun nurumacs--post-command-function ()
  ;; make "deactivate-mark" local, and save global value
  ;; => http://debbugs.gnu.org/cgi/bugreport.cgi?bug=3350
  (let (deactivate-mark)
    (if (or (eq this-command 'keyboard-quit) ; WORKING--------------------------------...
            (not (eq nurumacs--prev-buf (current-buffer)))
            (not (eq nurumacs--prev-wnd (selected-window)))
            (and (boundp 'cua--rectangle) cua--rectangle)
            (and (boundp 'multiple-cursors-mode) multiple-cursors-mode))
        (unless (eq this-command 'nurumacs-map-toggle) (nurumacs--map-kill))
      ;; automatic vscroll
      (if (or (< (point) (window-start))
              (>= (point) (window-end)))
          (recenter))
      ;; animate vscroll
      (let* ((lines (- (line-number-at-pos (window-start)) nurumacs--prev-lin)))
        (when (not (= lines 0))
          (when nurumacs-map (nurumacs--map-show))
          (nurumacs--vscroll-effect lines)))
      ;; automatic hscroll with animation
      (when (and nurumacs-auto-hscroll
                 (or truncate-lines
                     (truncated-partial-width-window-p))
                 (or (< (current-column) (window-hscroll))
                     (< (+ (window-hscroll) (window-width)) (current-column))))
        (nurumacs--hscroll
         (- (current-column) (window-hscroll) (/ (window-width) 2)))))))

(add-hook 'pre-command-hook 'nurumacs--pre-command-function)
(add-hook 'post-command-hook 'nurumacs--post-command-function)

;; * fix for popwin

(eval-after-load "popwin"
  '(progn
     (defadvice popwin:close-popup-window
       (before kill-nurumacs-map-with-popwin activate)
       (nurumacs--map-kill))
     (defadvice popwin:create-popup-window
       (before kill-nurumacs-map-with-popwin activate)
       (nurumacs--map-kill))))

;; * provide

(provide 'nurumacs)

;;; nurumacs.el ends here
