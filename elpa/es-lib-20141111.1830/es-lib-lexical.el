;;; es-lib-lexical.el --- A collection of emacs utilities: lexically bound functions.  -*- lexical-binding: t -*-
;;; Version: 0.4
;;; Author: sabof
;;; URL: https://github.com/sabof/es-lib

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; The project is hosted at https://github.com/sabof/es-lib

;;; Code:

(require 'cl-lib)
(require 'es-lib-core-macros)

(defun es-back-curry (func &rest more-args)
  "Like (apply-partially), but adds arguments to the end."
  (lambda (&rest args)
    (apply func (append args more-args))))

(defun es-comp (&rest funcs)
  "Same as clojure's (comp)."
  (lambda (arg)
    (let ((arg arg)
          (funcs (reverse funcs)))
      (mapc
       (lambda (func)
         (setq arg (funcall func arg)))
       funcs)
      arg)))

(defun es-complement (func)
  "Same as clojure's \(complement)."
  (lambda (&rest args)
    (not (apply func args))))

(defun es-constantly (arg)
  "Same as clojure's \(constantly)."
  (with-no-warnings
    (lambda (&rest args)
      arg)))

(defun es-flip (func)
  "Create a function with FUNC's arguments reversed."
  (lambda (&rest args)
    (apply func (reverse args))))

(defun es-timer (time-limit &optional callback)
  "Accepts a time-limit in minutes."
  (interactive (list (read-number "Time limit: ")))
  (let (( start-time (current-time))
        ( buf (generate-new-buffer "*timer*"))
        win
        time-difference
        the-timer)
    (setq the-timer
          (run-with-timer
           0 1 (lambda ()
                 (catch 'ablock
                   (unless (buffer-live-p buf)
                     (cancel-timer the-timer)
                     (throw 'ablock nil))
                   (setq time-difference
                         (time-subtract (current-time) start-time))
                   (with-current-buffer buf
                     (erase-buffer)
                     (insert
                      (if (> (float-time time-difference)
                             (* 60 time-limit))
                          (let (( message
                                  (format "%s minutes passed at: %s"
                                          time-limit
                                          (format-time-string
                                           "%H:%M"))))
                            (when (fboundp 'sauron-add-event)
                              (sauron-add-event 'es-timer 5 message))
                            (cancel-timer the-timer)
                            message)
                        (format "%s / %s:00"
                                (format-time-string
                                 "%M:%S"
                                 time-difference)
                                time-limit))))))))
    ;; Shouln't defvars be dynamically bound?
    (setq win
          (split-window (frame-root-window)
                        (- (frame-height)
                           (+ 2
                              (if (default-value 'mode-line-format) 1 0)
                              (if (default-value 'header-line-format) 1 0)))))
    (set-window-buffer win buf)
    (set-window-dedicated-p win t)
    (with-current-buffer buf
      (setq cursor-type nil)
      (setq window-size-fixed t))))

(provide 'es-lib-lexical)
;;; es-lib-lexical.el ends here
