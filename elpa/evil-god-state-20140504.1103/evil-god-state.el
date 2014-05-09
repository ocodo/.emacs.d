;;; evil-god-state.el --- use god-mode keybindings in evil-mode

;; Copyright (C) 2014 by Eric Seidel
;; Author: Eric Seidel
;; URL: https://github.com/gridaphobe/evil-god-state
;; Filename: evil-god-state.el
;; Description: use god-mode keybindings in evil-mode
;; Version: 20140504.1103
;; X-Original-Version: 0.1
;; Keywords: evil leader god-mode
;; Package-Requires: ((evil "1.0.8") (god-mode "2.12.0"))

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This is an evil-mode state for using god-mode.

;; It provides a command `evil-execute-in-god-state' that switches to
;; `god-local-mode' for the next command. I bind it to ","
;;
;;     (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
;;
;; for an automatically-configured leader key.
;;
;; Since `evil-god-state' includes an indicator in the mode-line, you may want
;; to use `diminish' to keep your mode-line uncluttered, e.g.
;;
;;     (add-hook 'evil-god-start-hook (lambda () (diminish 'god-local-mode)))
;;     (add-hook 'evil-god-stop-hook (lambda () (diminish-undo 'god-local-mode)))


;;; Code:
(require 'evil)
(require 'god-mode)

(evil-define-state god
  "God state."
  :tag " <G> "
  :message "-- GOD MODE --"
  :entry-hook (evil-god-start-hook)
  :exit-hook (evil-god-stop-hook)
  :input-method t
  :intercept-esc nil)

(defun evil-god-start-hook ()
  "Run before entering `evil-god-state'."
  (god-local-mode 1))

(defun evil-god-stop-hook ()
  "Run before exiting `evil-god-state'."
  (god-local-mode -1))

(defvar evil-execute-in-god-state-buffer nil)

(defun evil-stop-execute-in-god-state ()
  "Switch back to previous evil state."
  (when (and (not (eq this-command #'evil-execute-in-god-state))
             (not (minibufferp)))
    (remove-hook 'post-command-hook 'evil-stop-execute-in-god-state)
    (when (buffer-live-p evil-execute-in-god-state-buffer)
      (with-current-buffer evil-execute-in-god-state-buffer
        (if (and (eq evil-previous-state 'visual)
                 (not (use-region-p)))
            (progn
              (evil-change-to-previous-state)
              (evil-exit-visual-state))
          (evil-change-to-previous-state))))
    (setq evil-execute-in-god-state-buffer nil)))

;;;###autoload
(defun evil-execute-in-god-state ()
  "Execute the next command in God state."
  (interactive)
  (add-hook 'post-command-hook #'evil-stop-execute-in-god-state t)
  (setq evil-execute-in-god-state-buffer (current-buffer))
  (cond
   ((evil-visual-state-p)
    (let ((mrk (mark))
          (pnt (point)))
      (evil-god-state)
      (set-mark mrk)
      (goto-char pnt)))
   (t
    (evil-god-state)))
  (evil-echo "Switched to God state for the next command ..."))

(provide 'evil-god-state)
;;; evil-god-state.el ends here
