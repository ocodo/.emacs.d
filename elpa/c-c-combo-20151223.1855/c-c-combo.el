;;; c-c-combo.el --- Make stuff happen when you reach a target wpm -*- lexical-binding: nil -*-
;; Copyright (C) 2015  Diego Berrocal

;; Author: Diego Berrocal <cestdiego@gmail.com>
;; Homepage: https://www.github.com/CestDiego/c-c-combo.el
;; Created: Tue Dec 15
;; Version: 0.5
;; URL: https://github.com/CestDiego/c-c-combo.el

;;; License:
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
;;; Commentary:
;;
;; This will make sounds appear after you hit more than 60 wpm
;;
;;; Usage:
;;     (require 'c-c-combo) ;; Not necessary if using ELPA package
;;     (c-c-combo-mode 1)

;;; Code:

(defvar c-c-combo--last-key  '(("timestamp" . 0)
                               ("key" . nil)
                               ("n-repeats". 0))
  "Cons Cell, first item is timestamp second is the key")
(defvar c-c-combo--curr-cps  0
  "Moving Average Rate in Characters per second")
(defvar c-c-combo--target-wpm 60.0
  "Words per Minute Target Rate")
(defvar c-c-combo--target-cps 4.0
  "Characters per second Target Rate")
(defvar c-c-combo-check-timer nil
  "Timer that checks if we are over the target CPS")
(defvar c-c-combo--counter 0
  "Stores how many seconds you have been with acceptable wpm")

(defconst c-c-combo--files-path (file-name-directory load-file-name))
(defvar c-c-combo--announcer-files-path '()
  "Paths for the announcer sound files")

(defun c-c-combo--get-random-vpos (vpos)
  (let ((value (+ vpos (random 3))))
    (if (< value (window-height))
        value
      (1- (window-height)))))

(defun c-c-combo--get-random-hpos (hpos)
  (let ((value (+ hpos (random 2))))
    (if (< value (1- (window-width)))
        value
      (- (window-width) 2))))

(defun c-c-combo--animate-initialize (string vpos hpos)
  (let ((characters nil))
    (dotimes (i (length string))
      (setq characters
            (cons (list (aref string i)
                        ;; Random starting positions.
                        (c-c-combo--get-random-vpos vpos)
                        (c-c-combo--get-random-hpos hpos)
                        ;; All the chars should end up
                        ;; on the specified line.
                        vpos
                        ;; The Ith character in the string
                        ;; needs to end up I positions later.
                        (+ hpos i))
                  characters)))
    characters))

(defun c-c-combo--is-keycode-valid (keycode)
  (and (< keycode ?~) (> keycode ?!)))

(defun c-c-combo--animate-insertion ()
  (interactive)
  (with-demoted-errors "c-c-combo combination not catched %s"
    (let ((keys   (this-command-keys-vector))
          (column (current-column))
          (animate-n-steps 3)
          (animate-initialize #'c-c-combo--animate-initialize)
          (row    (1- (line-number-at-pos))))
      (when (and (derived-mode-p 'text-mode)
                 (or (evil-insert-state-p)
                     (evil-hybrid-state-p)
                     (evil-normal-state-p))
                 (c-c-combo--is-keycode-valid (aref keys 0))
                 (= 1 (length keys)))
        (save-excursion
          (when (= (line-end-position) (nth 5 (posn-at-point)))
            (animate-string (string (aref keys 0))
                            row
                            column)
            (delete-char -1)))))))

(defun c-c-combo-get-announcer-file-paths ()
  (unless (= 4 (length c-c-combo--announcer-files-path))
    (setq c-c-combo--announcer-files-path
          (mapcar (lambda (file)
                    (expand-file-name
                     (concat file ".wav")
                     c-c-combo--files-path))
                  '("fatality"
                    "flawless_spree"
                    "unstoppable"
                    "stop_this_modafoca")))))

(defun c-c-combo-set-target-rate (rate)
  (setq c-c-combo--target-wpm rate
        c-c-combo--target-cps (c-c-combo--wpm-to-cps rate)))

(defun c-c-combo--current-time-in-seconds ()
  (string-to-number (format-time-string "%s.%3N" (current-time))))

(defun c-c-combo--wpm-to-cps (rate-wpm)
  (let* ((chars-in-word 4.0)
         (chars-per-min (* rate-wpm chars-in-word))
         (chars-per-sec (/ chars-per-min 60.0)))
    chars-per-sec))

(defun c-c-combo--play-sound-file (path)
  (if (eq system-type 'darwin)
      (start-process "*Messages*" nil "afplay" path)
    (start-process "*Messages*" nil "aplay" path)))

(defun c-c-combo--play-announcer-sound ()
  "This will end when our list ends."
  (let ((current-sound (pop c-c-combo--announcer-files-path)))
    (when current-sound
      (c-c-combo--play-sound-file current-sound))))

(defun c-c-combo--encourage-user ()
  (when (and (not (equal c-c-combo--counter 0))
             (equal (mod c-c-combo--counter 5) 0))
    (c-c-combo--play-announcer-sound))
  (when (equal c-c-combo--counter 15)
    ;; (if selectric-mode
    ;;     nil
    ;;   (selectric-mode))
    (add-hook 'post-self-insert-hook #'c-c-combo--animate-insertion))
  (setq c-c-combo--counter (1+ c-c-combo--counter)))

(defun c-c-combo--check-if-over-target-rate ()
  (let ((n-repeats (assoc-default "n-repeats" c-c-combo--last-key))
        (computed-cps (c-c-combo--compute-cps)))
    (setq c-c-combo--curr-cps computed-cps)
    (if (and (< n-repeats 3)
             (> computed-cps c-c-combo--target-cps))
        (c-c-combo--encourage-user)
      (c-c-combo-get-announcer-file-paths)
      (remove-hook 'post-self-insert-hook #'c-c-combo--animate-insertion)
      ;; (when selectric-mode
      ;;   (selectric-mode -1))
      (setq c-c-combo--counter 0))))


(defun c-c-combo--compute-cps ()
  (let* ((now        (c-c-combo--current-time-in-seconds))
         (last-time  (assoc-default "timestamp" c-c-combo--last-key))
         (interval   (- now last-time))
         (exponent   (* interval c-c-combo--target-cps))
         (base       (- 1.0 (/ 1.0 c-c-combo--target-cps)))
         (decay-factor (expt base exponent))
         (new-rate (* c-c-combo--curr-cps decay-factor)))
    new-rate))

(defun c-c-combo--process ()
  (with-demoted-errors "Error while running C-c combo: %s"
    (when (and (this-command-keys)
               (derived-mode-p 'text-mode))
      (let* ((now      (c-c-combo--current-time-in-seconds))
             (key      (this-command-keys))
             (last-key (assoc-default "key" c-c-combo--last-key))
             (new-rate (+ 1 (c-c-combo--compute-cps)))
             (repeated? (equal key last-key))
             (n-repeats (assoc-default "n-repeats" c-c-combo--last-key)))
        (setq c-c-combo--curr-cps new-rate
              c-c-combo--last-key `(("timestamp" . ,now)
                                    ("key"       . ,key)
                                    ("n-repeats" . ,(if repeated? (1+ n-repeats) 0))))))))

(defun c-c-combo--activate ()
  "Activates Combo mode."
  (setq c-c-combo-check-timer (run-at-time
                               "1 second"
                               1
                               'c-c-combo--check-if-over-target-rate))
  (add-hook 'pre-command-hook #'c-c-combo--process))

(defun c-c-combo--deactivate ()
  "Deactivates Combo mode, and deletes timer."
  (remove-hook 'pre-command-hook #'c-c-combo--process)
  (cancel-timer c-c-combo-check-timer))

(defun c-c-combo--toggle ()
  "Toggle Combo mode."
  (interactive)
  (if c-c-combo-mode
      (c-c-combo--activate)
    (c-c-combo--deactivate)))

;;;###autoload
(define-minor-mode c-c-combo-mode
  "c-c-combo"
  :global t
  :lighter nil
  :keymap nil
  :init-value nil
  (c-c-combo--toggle))

(provide 'c-c-combo)
;;; c-c-combo.el ends here
