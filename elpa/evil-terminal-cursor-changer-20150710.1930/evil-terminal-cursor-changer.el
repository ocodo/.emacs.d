;;; evil-terminal-cursor-changer.el --- Change cursor shape by evil state on terminal.
;;
;; Filename: evil-terminal-cursor-changer.el
;; Description: Change cursor by evil state on terminal.
;; Author: 7696122
;; Maintainer: 7696122
;; Created: Sat Nov  2 12:17:13 2013 (+0900)
;; Version: 0.0.2
;; Package-Version: 20150710.1930
;; Package-Requires: ((evil "1.0.8"))
;; Last-Updated: Sat May  9 01:53:50 2015 (+0900)
;;           By: Yongmun Kim
;;     Update #: 387
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
;; 
;; If want change cursor shape type, add below line. This is evil's setting.
;;
;;      (setq evil-visual-state-cursor 'box) ; █
;;      (setq evil-insert-state-cursor 'bar) ; ⎸
;;      (setq evil-emacs-state-cursor 'hbar) ; _
;;
;; Now, works on Gnome Terminal(Gnome Desktop), iTerm(Mac OS X), Konsole(KDE Desktop).
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

(defun etcc--on-iterm? ()
  "Running on iTerm."
  (string= (getenv "TERM_PROGRAM") "iTerm.app"))

(defun etcc--on-xterm? ()
  "Runing on xterm."
  (downcase (getenv "XTERM_VERSION")))

(defun etcc--on-gnome-terminal? ()
  "Running on gnome-terminal."
  (string= (getenv "COLORTERM") "gnome-terminal"))

(defun etcc--on-konsole? ()
  "Running on konsole."
  (if (getenv "KONSOLE_PROFILE_NAME") t nil))

(defun etcc--on-apple-terminal? ()
  "Running on Apple Terminal"
  (string= (getenv "TERM_PROGRAM") "Apple_Terminal"))

(defun etcc--on-tmux? ()
  "Running on tmux."
  (if (getenv "TMUX") t nil))

(defun etcc--get-cursor-shape (evil-cursor)
  "Detect cursor shape in evil-*-state-cursor variable"
  (if (listp evil-cursor)
      (dolist (el evil-cursor)
	(if el
	    (if (symbolp el) (return el)
	      (if (consp el) (return (car el))))))
    (if (symbolp evil-cursor)
        evil-cursor
      cursor-type)))

(defun etcc--get-current-gnome-profile-name ()
  "Return Current profile name of Gnome Terminal."
  ;; https://github.com/helino/current-gnome-terminal-profile/blob/master/current-gnome-terminal-profile.sh
  (if (etcc--on-gnome-terminal?)
      (let ((cmd "#!/bin/zsh
FNAME=$HOME/.current_gnome_profile
gnome-terminal --save-config=$FNAME
ENTRY=`grep ProfileID < $FNAME`
rm $FNAME
TERM_PROFILE=${ENTRY#*=}
echo -n $TERM_PROFILE"))
        (shell-command-to-string cmd))
    "Default"))

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
(defvar etcc--iterm-box-cursor-string "\e]50;CursorShape=0\x7"
  "The cursor type box(block) on iTerm.")

(defvar etcc--iterm-bar-cursor-string "\e]50;CursorShape=1\x7"
  "The cursor type bar(ibeam) on iTerm.")

(defvar etcc--iterm-hbar-cursor-string "\e]50;CursorShape=2\x7"
  "The cursor type hbar(underline) on iTerm.")

(defvar etcc--tmux-iterm-box-cursor-string
  (concat "\ePtmux;\e" etcc--iterm-box-cursor-string "\e\\")
  "The cursor type box(block) on iTerm and tmux.")

(defvar etcc--tmux-iterm-bar-cursor-string
  (concat "\ePtmux;\e" etcc--iterm-bar-cursor-string "\e\\")
  "The cursor type bar(ibeam) on iTerm and tmux.")

(defvar etcc--tmux-iterm-hbar-cursor-string
  (concat "\ePtmux;\e" etcc--iterm-hbar-cursor-string "\e\\")
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
  (if (or (etcc--on-iterm?) (etcc--on-konsole?))
      (if (etcc--on-tmux?)
          (send-string-to-terminal etcc--tmux-iterm-bar-cursor-string)
        (send-string-to-terminal etcc--iterm-bar-cursor-string)))

  (if (etcc--on-gnome-terminal?)
      (with-temp-buffer
        (shell-command etcc--gnome-terminal-bar-cursor-string t))))

(defun etcc--set-hbar-cursor ()
  "Set cursor type hbar(underline)."
  (if (or (etcc--on-iterm?) (etcc--on-konsole?))
      (if (etcc--on-tmux?)
          (send-string-to-terminal etcc--tmux-iterm-hbar-cursor-string)
        (send-string-to-terminal etcc--iterm-hbar-cursor-string)))

  (if (etcc--on-gnome-terminal?)
      (with-temp-buffer
        (shell-command etcc--gnome-terminal-hbar-cursor-string t))))

(defun etcc--set-box-cursor ()
  "Set cursor type box(block)."
  (if (or (etcc--on-iterm?) (etcc--on-konsole?))
      (if (etcc--on-tmux?)
          (send-string-to-terminal etcc--tmux-iterm-box-cursor-string)
        (send-string-to-terminal etcc--iterm-box-cursor-string)))

  (if (etcc--on-gnome-terminal?)
      (with-temp-buffer
        (shell-command etcc--gnome-terminal-box-cursor-string t))))

(defun etcc--set-cursor-shape (shape)
  "Set cursor shape."
  (cond
   ((eq shape 'box)               (etcc--set-box-cursor))
   ((eq shape 'evil-half-cursor)  (etcc--set-box-cursor))
   ((eq shape 'bar)               (etcc--set-bar-cursor))
   ((eq shape 'hbar)              (etcc--set-hbar-cursor))
   (t (etcc--set-box-cursor))))

(defun etcc--get-evil-emacs-state-cursor ()         (etcc--get-cursor-shape evil-emacs-state-cursor))
;; (defun etcc--get-evil-evilified-state-cursor ()     (etcc--get-cursor-shape evil-evilified-state-cursor))
(defun etcc--get-evil-insert-state-cursor ()        (etcc--get-cursor-shape evil-insert-state-cursor))
;; (defun etcc--get-evil-lisp-state-cursor ()          (etcc--get-cursor-shape evil-lisp-state-cursor))a
(defun etcc--get-evil-motion-state-cursor ()        (etcc--get-cursor-shape evil-motion-state-cursor))
(defun etcc--get-evil-normal-state-cursor ()        (etcc--get-cursor-shape evil-normal-state-cursor))
(defun etcc--get-evil-operator-state-cursor ()      (etcc--get-cursor-shape evil-operator-state-cursor))
(defun etcc--get-evil-replace-state-cursor ()       (etcc--get-cursor-shape evil-replace-state-cursor))
(defun etcc--get-evil-visual-state-cursor ()        (etcc--get-cursor-shape evil-visual-state-cursor))
;; (defun etcc--get-evil-iedit-state-cursor ()         (etcc--get-cursor-shape evil-iedit-state-cursor))
;; (defun etcc--get-evil-iedit-insert-state-cursor ()  (etcc--get-cursor-shape evil-iedit-insert-state-cursor))

(defun etcc--set-evil-cursor ()
  "Set cursor type for Evil."
  (cond
   ;; ((evil-evilified-state-p)     (etcc--set-cursor-shape (etcc--get-evil-evilified-state-cursor)))
   ((evil-insert-state-p)        (etcc--set-cursor-shape (etcc--get-evil-insert-state-cursor)))
   ;; ((evil-lisp-state-p)          (etcc--set-cursor-shape (etcc--get-evil-lisp-state-cursor)))
   ((evil-motion-state-p)        (etcc--set-cursor-shape (etcc--get-evil-motion-state-cursor)))
   ((evil-normal-state-p)        (etcc--set-cursor-shape (etcc--get-evil-normal-state-cursor)))
   ((evil-operator-state-p)      (etcc--set-cursor-shape (etcc--get-evil-operator-state-cursor)))
   ((evil-replace-state-p)       (etcc--set-cursor-shape (etcc--get-evil-replace-state-cursor)))
   ((evil-visual-state-p)        (etcc--set-cursor-shape (etcc--get-evil-visual-state-cursor)))
   ((evil-emacs-state-p)         (etcc--set-cursor-shape (etcc--get-evil-emacs-state-cursor)))
   ;; ((evil-iedit-state-p)         (etcc--set-cursor-shape (etcc--get-evil-iedit-state-cursor)))
   ;; ((evil-iedit-insert-state-p)  (etcc--set-cursor-shape (etcc--get-evil-iedit-insert-state-cursor)))
   ))

(add-hook 'post-command-hook 'etcc--set-evil-cursor)

(defun turn-on-evil-terminal-cursor-changer ()
  (interactive)
  (add-hook 'post-command-hook 'etcc--set-evil-cursor))

(defun turn-off-evil-terminal-cursor-changer ()
  (interactive)
  (remove-hook 'post-command-hook 'etcc--set-evil-cursor))

(provide 'evil-terminal-cursor-changer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evil-terminal-cursor-changer.el ends here
