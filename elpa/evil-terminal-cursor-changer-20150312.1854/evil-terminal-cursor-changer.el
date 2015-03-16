;;; evil-terminal-cursor-changer.el --- Change cursor by evil state on terminal
;;
;; Filename: evil-terminal-cursor-changer.el
;; Description: Change cursor by evil state on terminal.
;; Author: 7696122
;; Maintainer: 7696122
;; Created: Sat Nov  2 12:17:13 2013 (+0900)
;; Version: 0.0.1
;; Package-Version: 20150312.1854
;; Package-Requires: ((evil "1.0.8"))
;; Last-Updated: Fri Mar 13 10:27:43 2015 (+0900)
;;           By: 7696122
;;     Update #: 332
;; URL: https://github.com/7696122/evil-terminal-cursor-changer
;; Doc URL: https://github.com/7696122/evil-terminal-cursor-changer/blob/master/README.md
;; Keywords: evil, terminal, cursor
;; Compatibility: GNU Emacs: 24.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; Then add the following lines to ~/.emacs:
;;
;;      (unless (display-graphic-p)
;;        (require 'evil-terminal-cursor-changer))
;;
;; If want change cursor type, add below line. This is evil's setting.
;;
;;      (setq evil-visual-state-cursor 'box) ; █
;;      (setq evil-insert-state-cursor 'bar) ; ⎸
;;      (setq evil-emacs-state-cursor 'hbar) ; _
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;
;;; Code:

(require 'evil)

(defun etcc--is-iterm ()
  "Running on iTerm."
  (string= (getenv "TERM_PROGRAM") "iTerm.app"))

(defun etcc--is-gnome-terminal ()
  "Running on gnome-terminal."
  (string= (getenv "COLORTERM") "gnome-terminal"))

(defun etcc--is-tmux ()
  "Running on tmux."
  (if (getenv "TMUX") t nil))

(defun etcc--get-cursor-type (evil-cursor)
  "Return Evil cursor type for state."
  (if (not (listp evil-cursor))
      (if (symbolp evil-state)
          evil-cursor
        cursor-type)
    (cond
     ((find 'bar evil-cursor) 'bar)
     ((find 'hbar evil-cursor) 'hbar)
     ((find 'box evil-cursor) 'box)
     (t cursor-type))))

(defun etcc--get-current-gnome-profile-name ()
  "Return Current profile name of Gnome Terminal."
  ;; https://github.com/helino/current-gnome-terminal-profile/blob/master/current-gnome-terminal-profile.sh
  (if (etcc--is-gnome-terminal)
      (let ((cmd "#!/bin/bash
FNAME=$HOME/.current_gnome_profile
gnome-terminal --save-config=$FNAME
ENTRY=`grep ProfileID < $FNAME`
rm $FNAME
TERM_PROFILE=${ENTRY#*=}
echo -n $TERM_PROFILE"))
        (shell-command-to-string cmd))
    "Default"))

(defun etcc--get-evil-visual-state-cursor ()
  "Evil visual state cursor."
  (etcc--get-cursor-type evil-visual-state-cursor))

(defun etcc--get-evil-insert-state-cursor ()
  "Evil insert state cursor."
  (etcc--get-cursor-type evil-insert-state-cursor))

(defun etcc--get-evil-emacs-state-cursor ()
  "Evil Emacs state cursor."
  (etcc--get-cursor-type evil-emacs-state-cursor))

;; https://code.google.com/p/iterm2/wiki/ProprietaryEscapeCodes
;; http://unix.stackexchange.com/questions/3759/how-to-stop-cursor-from-blinking
;; http://www.joinc.co.kr/modules/moniwiki/wiki.php/man/1/echo
;; http://vim.wikia.com/wiki/Change_cursor_shape_in_different_modes
;; \<Esc>]50;CursorShape=0\x7
;; konsole
;; "\e]50;CursorShape=2\x7"
;; "\e]50;CursorShape=1\x7"
;; "\e]50;CursorShape=0\x7"
;; (send-string-to-terminal "\e]50;CursorShape=2\x7")
(defvar etcc--box-cursor-string "\e]50;CursorShape=0\x7"
  "The cursor type box(block) on iTerm.")

(defvar etcc--bar-cursor-string "\e]50;CursorShape=1\x7"
  "The cursor type bar(ibeam) on iTerm.")

(defvar etcc--hbar-cursor-string "\e]50;CursorShape=2\x7"
  "The cursor type hbar(underline) on iTerm.")

(defvar etcc--tmux-box-cursor-string
  (concat "\ePtmux;\e" etcc--box-cursor-string "\e\\")
  "The cursor type box(block) on iTerm and tmux.")

(defvar etcc--tmux-bar-cursor-string
  (concat "\ePtmux;\e" etcc--bar-cursor-string "\e\\")
  "The cursor type bar(ibeam) on iTerm and tmux.")

(defvar etcc--tmux-hbar-cursor-string
  (concat "\ePtmux;\e" etcc--hbar-cursor-string "\e\\")
  "The cursor type hbar(underline) on iTerm and tmux.")

(defvar etcc--gnome-terminal-set-cursor-string
  (format "gconftool-2 --type string --set /apps/gnome-terminal/profiles/%s/cursor_shape " (etcc--get-current-gnome-profile-name))
  "The gconftool string for changing cursor.")

(defvar etcc--gnome-terminal-bar-cursor-string
  (concat etcc--gnome-terminal-set-cursor-string "ibeam")
  "The cursor type bar(ibeam) on gnome-terminal.")

(defvar etcc--gnome-terminal-box-cursor-string
  (concat etcc--gnome-terminal-set-cursor-string "block")
  "The cursor type box(block) on gnome-terminal.")

(defvar etcc--gnome-terminal-hbar-cursor-string
  (concat etcc--gnome-terminal-set-cursor-string "underline")
  "The cursor type hbar(underline) on gnome-terminal.")

(defun etcc--set-bar-cursor ()
  "Set cursor type bar(ibeam)."
  (if (etcc--is-iterm)
      (if (etcc--is-tmux)
          (send-string-to-terminal etcc--tmux-bar-cursor-string)
        (send-string-to-terminal etcc--bar-cursor-string)))

  (if (etcc--is-gnome-terminal)
      (with-temp-buffer
        (shell-command etcc--gnome-terminal-bar-cursor-string t))))

(defun etcc--set-hbar-cursor ()
  "Set cursor type hbar(underline)."
  (if (etcc--is-iterm)
      (if (etcc--is-tmux)
          (send-string-to-terminal etcc--tmux-hbar-cursor-string)
        (send-string-to-terminal etcc--hbar-cursor-string)))

  (if (etcc--is-gnome-terminal)
      (with-temp-buffer
        (shell-command etcc--gnome-terminal-hbar-cursor-string t))))

(defun etcc--set-box-cursor ()
  "Set cursor type box(block)."
  (if (etcc--is-iterm)
      (if (etcc--is-tmux)
          (send-string-to-terminal etcc--tmux-box-cursor-string)
        (send-string-to-terminal etcc--box-cursor-string)))

  (if (etcc--is-gnome-terminal)
      (with-temp-buffer
        (shell-command etcc--gnome-terminal-box-cursor-string t))))

(defun etcc--set-evil-cursor ()
  "Set cursor type for Evil."
  (if (evil-emacs-state-p)
      (cond ((eq (etcc--get-evil-emacs-state-cursor) 'hbar)
             (etcc--set-hbar-cursor))
            ((eq (etcc--get-evil-emacs-state-cursor) 'box)
             (etcc--set-box-cursor))
            ((eq (etcc--get-evil-emacs-state-cursor) 'bar)
             (etcc--set-bar-cursor))))
  (if (evil-insert-state-p)
      (cond ((eq (etcc--get-evil-insert-state-cursor) 'hbar)
             (etcc--set-hbar-cursor))
            ((eq (etcc--get-evil-insert-state-cursor) 'box)
             (etcc--set-box-cursor))
            ((eq (etcc--get-evil-insert-state-cursor) 'bar)
             (etcc--set-bar-cursor))))
  (if (evil-normal-state-p)
      (cond ((eq (etcc--get-evil-visual-state-cursor) 'hbar)
             (etcc--set-hbar-cursor))
            ((eq (etcc--get-evil-visual-state-cursor) 'box)
             (etcc--set-box-cursor))
            ((eq (etcc--get-evil-visual-state-cursor) 'bar)
             (etcc--set-bar-cursor)))))

(add-hook 'post-command-hook 'etcc--set-evil-cursor)

(provide 'evil-terminal-cursor-changer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evil-terminal-cursor-changer.el ends here
