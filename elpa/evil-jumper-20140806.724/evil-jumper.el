;;; evil-jumper.el --- Jump like vimmers do!

;; Copyright (C) 2014 by Bailey Ling
;; Author: Bailey Ling
;; URL: https://github.com/bling/evil-jumper
;; Filename: evil-jumper.el
;; Description: Jump like vimmers do!
;; Created: 2014-07-01
;; Version: 20140806.724
;; X-Original-Version: 0.0.1
;; Keywords: evil vim jumplist jump list
;; Package-Requires: ((evil "0"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;
;; evil-jumper is an add-on for evil-mode which replaces the
;; implementation of the jump list such that it mimics more closely
;; with Vim's behavior. Specifically, it will jump across buffer
;; boundaries and revive dead buffers if necessary. The jump list can
;; also be persisted to a file and restored between sessions.
;;
;; Install:
;;
;; (require 'evil-jumper)
;;
;; Usage:
;;
;; Requiring will automatically rebind C-o and C-i.

;;; Code:

(require 'evil)

(defgroup evil-jumper nil
  "evil-jumper configuration options."
  :prefix "evil-jumper"
  :group 'evil)

(defcustom evil-jumper-max-length 100
  "The maximum number of jumps to keep track of."
  :type 'integer
  :group 'evil-jumper)

(defcustom evil-jumper-auto-center nil
  "Auto-center the line after jumping."
  :type 'boolean
  :group 'evil-jumper)

(defcustom evil-jumper-ignored-file-patterns '("COMMIT_EDITMSG")
  "A list of pattern regexps to match on the file path to exclude from being included in the jump list."
  :type '(repeat string)
  :group 'evil-jumper)

(defcustom evil-jumper-file nil
  "The location of the file to save/load the jump list."
  :type 'string
  :group 'evil-jumper)

(defcustom evil-jumper-auto-save-interval 0
  "If positive, specifies the interval in seconds to persist the jump list.

Note: The value of `evil-jumper-file' must also be non-nil."
  :type 'integer
  :group 'evil-jumper)

(defvar evil-jumper--list nil)
(defvar evil-jumper--idx -1)
(defvar evil-jumper--jumping nil)

(defun evil-jumper--read-file ()
  "Restores the jump list from the persisted file."
  (when (file-exists-p evil-jumper-file)
    (setq evil-jumper--list nil)
    (let ((lines (with-temp-buffer
                   (insert-file-contents evil-jumper-file)
                   (split-string (buffer-string) "\n" t))))
      (dolist (line lines)
        (let* ((parts (split-string line " "))
               (pos (string-to-number (car parts)))
               (file-name (cadr parts)))
          (push (list pos file-name) evil-jumper--list))))))

(defun evil-jumper--write-file ()
  "Saves the current contents of the jump list to a persisted file."
  (with-temp-file evil-jumper-file
    (dolist (jump evil-jumper--list)
      (let ((pos (car jump))
            (file-name (cadr jump)))
        (when (file-exists-p file-name)
          (insert (format "%d" pos))
          (insert " ")
          (insert file-name)
          (insert "\n"))))))

(defun evil-jumper--jump-to-index (idx)
  (when (and (< idx (length evil-jumper--list))
             (>= idx 0))
    (setq evil-jumper--idx idx)
    (let* ((place (nth idx evil-jumper--list))
           (pos (car place))
           (file-name (cadr place)))
      (setq evil-jumper--jumping t)
      (if (equal file-name "*scratch*")
          (switch-to-buffer file-name)
        (find-file file-name))
      (setq evil-jumper--jumping nil)
      (goto-char pos)
      (when evil-jumper-auto-center
        (recenter)))))

(defun evil-jumper--push ()
  "Pushes the current cursor/file position to the jump list."
  (while (> (length evil-jumper--list) evil-jumper-max-length)
    (nbutlast evil-jumper--list 1))
  (let ((file-name (buffer-file-name))
        (buffer-name (buffer-name))
        (current-pos (point))
        (first-pos nil)
        (first-file-name nil)
        (excluded nil))
    (when (and (not file-name) (equal buffer-name "*scratch*"))
      (setq file-name buffer-name))
    (when file-name
      (dolist (pattern evil-jumper-ignored-file-patterns)
        (when (string-match-p pattern file-name)
          (setq excluded t)))
      (unless excluded
        (when evil-jumper--list
          (setq first-pos (caar evil-jumper--list))
          (setq first-file-name (car (cdar evil-jumper--list))))
        (unless (and (equal first-pos current-pos)
                     (equal first-file-name file-name))
          (push `(,current-pos ,file-name) evil-jumper--list))))))

(defun evil-jumper--set-jump ()
  (unless evil-jumper--jumping
    ;; clear out intermediary jumps when a new one is set
    (nbutlast evil-jumper--list evil-jumper--idx)
    (setq evil-jumper--idx -1)
    (evil-jumper--push)))

(evil-define-motion evil-jumper/backward (count)
  (let ((count (or count 1)))
    (evil-motion-loop (nil count)
      (when (= evil-jumper--idx -1)
        (setq evil-jumper--idx (+ evil-jumper--idx 1))
        (evil-jumper--push))
      (evil-jumper--jump-to-index (+ evil-jumper--idx 1)))))

(evil-define-motion evil-jumper/forward (count)
  (let ((count (or count 1)))
    (evil-motion-loop (nil count)
      (evil-jumper--jump-to-index (- evil-jumper--idx 1)))))

(defadvice evil-set-jump (after evil-jumper--evil-set-jump activate)
  (evil-jumper--set-jump))

(defadvice switch-to-buffer (before evil-jumper--switch-to-buffer activate)
  (evil-jumper--set-jump))

(define-key evil-motion-state-map (kbd "C-o") 'evil-jumper/backward)
(when evil-want-C-i-jump
  (define-key evil-motion-state-map (kbd "C-i") 'evil-jumper/forward))

(add-hook 'next-error-hook 'evil-jumper--set-jump)

(when evil-jumper-file
  (evil-jumper--read-file)
  (add-hook 'kill-emacs-hook 'evil-jumper--write-file)
  (when (> evil-jumper-auto-save-interval 0)
    (run-with-timer evil-jumper-auto-save-interval evil-jumper-auto-save-interval 'evil-jumper--write-file)))

(provide 'evil-jumper)

;;; evil-jumper.el ends here
