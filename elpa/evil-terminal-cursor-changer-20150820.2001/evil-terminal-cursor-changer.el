;;; evil-terminal-cursor-changer.el --- Change cursor shape and color by evil state in terminal
;;
;; Filename: evil-terminal-cursor-changer.el
;; Description: Change cursor shape and color by evil state in terminal.
;; Author: 7696122
;; Maintainer: 7696122
;; Created: Sat Nov  2 12:17:13 2013 (+0900)
;; Version: 0.0.3
;; Package-Version: 20150820.2001
;; Package-X-Original-Version: 20150819.907
;; Package-Requires: ((evil "1.0.8") (hexrgb "21.0"))
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

;; [![MELPA](http://melpa.org/packages/evil-terminal-cursor-changer-badge.svg)](http://melpa.org/#/evil-terminal-cursor-changer)

;; ## Introduce ##
;;
;; evil-terminal-cursor-changer is changing cursor shape and color by evil state for evil-mode.
;;
;; When running in terminal, It's especially helpful to recognize evil's state.
;;
;; ## Install ##
;;
;; 1. Config melpa: http://melpa.org/#/getting-started
;;
;; 2. M-x package-install RET evil-terminal-cursor-changer RET
;;
;; 3. Add code to your emacs config file:（for example: ~/.emacs）：
;;
;; For Only terminal
;;
;;      (unless (display-graphic-p)
;;              (require 'evil-terminal-cursor-changer))
;;
;; For All
;;
;;      (require 'evil-terminal-cursor-changer)
;;
;; If want change cursor shape type, add below line. This is evil's setting.
;; 
;;      (setq evil-visual-state-cursor '("red" box)); █
;;      (setq evil-insert-state-cursor '("green" bar)); ⎸
;;      (setq evil-emacs-state-cursor '("blue" hbar)); _
;;
;; Now, works in XTerm, Gnome Terminal(Gnome Desktop), iTerm(Mac OS
;; X), Konsole(KDE Desktop), dumb(etc. mintty), Apple
;; Terminal.app(restrictive supporting). If using Apple Terminal.app,
;; must install SIMBL(http://www.culater.net/software/SIMBL/SIMBL.php)
;; and MouseTerm
;; plus(https://github.com/saitoha/mouseterm-plus/releases) to use
;; evil-terminal-cursor-changer. That makes to support VT's DECSCUSR
;; sequence.
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
(require 'hexrgb)

(defgroup evil-terminal-cursor-changer nil
  "Change cursor shape in terminal for Evil."
  :group 'evil-terminal-cursor-changer)

(defcustom etcc--enable-cursor-color? nil
  "On/off evil cursor color in Evil.")

(defcustom etcc--enable-blink-cursor? t
  "On/off blink cursor in Evil")

(defun etcc--in-dumb? ()
  "Running in dumb."
  (string= (getenv "TERM") "dumb"))

(defun etcc--in-iterm? ()
  "Running in iTerm."
  (string= (getenv "TERM_PROGRAM") "iTerm.app"))

(defun etcc--in-xterm? ()
  "Runing in xterm."
  (getenv "XTERM_VERSION"))

(defun etcc--in-gnome-terminal? ()
  "Running in gnome-terminal."
  (string= (getenv "COLORTERM") "gnome-terminal"))

(defun etcc--in-konsole? ()
  "Running in konsole."
  (getenv "KONSOLE_PROFILE_NAME"))

(defun etcc--in-apple-terminal? ()
  "Running in Apple Terminal"
  (string= (getenv "TERM_PROGRAM") "Apple_Terminal"))

(defun etcc--in-tmux? ()
  "Running in tmux."
  (getenv "TMUX"))

(defun etcc--get-evil-cursor-color (evil-cursor)
  "Detect cursor shape in evil-*-state-cursor variable"
  (if (listp evil-cursor)
      (dolist (el evil-cursor)
        (if (stringp el)
            (return el)))
    evil-cursor))

(defun etcc--get-evil-cursor-shape (evil-cursor)
  "Detect cursor shape in evil-*-state-cursor variable"
  (if (listp evil-cursor)
      (dolist (el evil-cursor)
        (if el
            (if (symbolp el)
                (return el)
              (if (consp el)
                  (return (car el))))))
    (if (symbolp evil-cursor)
        evil-cursor
      (if (or (eq cursor-type t)
              (not cursor-type))
          'box
        cursor-type))))

(defun etcc--color-name-to-hex (name)
  "Convert color name to hex value."
  (if (hexrgb-rgb-hex-string-p name)
      name
    (let* ((rgb (color-name-to-rgb name))
           (r (nth 0 rgb))
           (g (nth 1 rgb))
           (b (nth 2 rgb)))
      (color-rgb-to-hex r g b))))

(defun etcc--get-evil-cursor-color-by-hex (evil-cursor)
  (let ((cursor-background (etcc--get-evil-cursor-color evil-cursor)))
    (let ((hex-color (etcc--color-name-to-hex cursor-background)))
      (if (etcc--in-iterm?)
          (if (string-prefix-p "#" hex-color)
              (substring hex-color 1))
        hex-color))))

(defun etcc--get-current-gnome-profile-name ()
  "Return Current profile name of Gnome Terminal."
  ;; https://github.com/helino/current-gnome-terminal-profile/blob/master/current-gnome-terminal-profile.sh
  (if (etcc--in-gnome-terminal?)
      (let ((cmd "#!/bin/sh
FNAME=$HOME/.current_gnome_profile
gnome-terminal --save-config=$FNAME
ENTRY=`grep ProfileID < $FNAME`
rm $FNAME
TERM_PROFILE=${ENTRY#*=}
echo -n $TERM_PROFILE"))
        (shell-command-to-string cmd))
    "Default"))

;;; Cursor Color
(defun etcc--get-xterm-cursor-color-string (evil-cursor)
  (if etcc--enable-cursor-color?
      ;; https://www.iterm2.com/documentation-escape-codes.html
      (let ((prefix (if (etcc--in-iterm?)
                        "\e]Pl"
                      "\e]12;"))
            (suffix (if (etcc--in-iterm?)
                        "\e\\"
                      "\a")))
        (concat prefix (etcc--get-evil-cursor-color-by-hex evil-cursor) suffix))))

;;; Cursor Shape
(let ((prefix "\e[")
      (suffix " q"))
  (defvar etcc--xterm-box-blink-cursor-string 
    (concat prefix "1" suffix)
    "The cursor type box(block) in xterm.")

  (defvar etcc--xterm-box-cursor-string
    (concat prefix "2" suffix)
    "The cursor type box(block) in xterm.")

  (defvar etcc--xterm-hbar-blink-cursor-string
    (concat prefix "3" suffix)
    "The cursor type hbar(underline) in xterm.")

  (defvar etcc--xterm-hbar-cursor-string
    (concat prefix "4" suffix)
    "The cursor type hbar(underline) in xterm.")

  (defvar etcc--xterm-bar-blink-cursor-string
    (concat prefix "5" suffix)
    "The cursor type bar(ibeam) in xterm.")

  (defvar etcc--xterm-bar-cursor-string
    (concat prefix "6" suffix)
    "The cursor type bar(ibeam) in xterm."))

(let ((prefix "\e]50;CursorShape=")
      (suffix "\x7"))
  (defvar etcc--iterm-box-cursor-string
    (concat prefix "0" suffix)
    "The cursor type box(block) in iTerm.")

  (defvar etcc--iterm-bar-cursor-string
    (concat prefix "1" suffix)
    "The cursor type bar(ibeam) in iTerm.")

  (defvar etcc--iterm-hbar-cursor-string 
    (concat prefix "2" suffix)
    "The cursor type hbar(underline) in iTerm."))

(let ((prefix "\ePtmux;\e")
      (suffix "\e\\"))
  (defvar etcc--tmux-iterm-box-cursor-string
    (concat prefix etcc--iterm-box-cursor-string suffix)
    "The cursor type box(block) in iTerm and tmux.")

  (defvar etcc--tmux-iterm-bar-cursor-string
    (concat prefix etcc--iterm-bar-cursor-string suffix)
    "The cursor type bar(ibeam) in iTerm and tmux.")

  (defvar etcc--tmux-iterm-hbar-cursor-string
    (concat prefix etcc--iterm-hbar-cursor-string suffix)
    "The cursor type hbar(underline) in iTerm and tmux."))

;;; Gnome Terminal
(defvar etcc--gnome-terminal-set-cursor-string
  (format "gconftool-2 --type string --set /apps/gnome-terminal/profiles/%s/cursor_shape " 
          (etcc--get-current-gnome-profile-name))
  "The gconftool string for changing cursor.")

(defvar etcc--gnome-terminal-bar-cursor-string
  (concat etcc--gnome-terminal-set-cursor-string "ibeam")
  "The cursor type bar(ibeam) in gnome-terminal.")

(defvar etcc--gnome-terminal-box-cursor-string
  (concat etcc--gnome-terminal-set-cursor-string "block")
  "The cursor type box(block) in gnome-terminal.")

(defvar etcc--gnome-terminal-hbar-cursor-string
  (concat etcc--gnome-terminal-set-cursor-string "underline")
  "The cursor type hbar(underline) in gnome-terminal.")

(defun etcc--set-bar-cursor (evil-cursor)
  "Set cursor type bar(ibeam)."
  (if (etcc--in-gnome-terminal?)
      (with-temp-buffer
        (shell-command etcc--gnome-terminal-bar-cursor-string t))
    (if (etcc--in-konsole?)
        (if (etcc--in-tmux?)
            (send-string-to-terminal etcc--tmux-iterm-bar-cursor-string)
          (send-string-to-terminal ;; etcc--iterm-bar-cursor-string
           (concat (if (and etcc--enable-blink-cursor? blink-cursor)
                       etcc--xterm-bar-blink-cursor-string
                     etcc--xterm-bar-cursor-string)
                   (etcc--get-xterm-cursor-color-string evil-cursor))))
      (if (or (etcc--in-xterm?)
              (etcc--in-iterm?)
              (etcc--in-apple-terminal?)
              (etcc--in-dumb?))
          (send-string-to-terminal
           (concat (if (and etcc--enable-blink-cursor? blink-cursor)
                       etcc--xterm-bar-blink-cursor-string
                     etcc--xterm-bar-cursor-string)
                   (etcc--get-xterm-cursor-color-string evil-cursor)))))))

(defun etcc--set-hbar-cursor (evil-cursor)
  "Set cursor type hbar(underline)."
  (if (etcc--in-gnome-terminal?)
      (with-temp-buffer
        (shell-command etcc--gnome-terminal-hbar-cursor-string t))
    (if (etcc--in-konsole?)
        (if (etcc--in-tmux?)
            (send-string-to-terminal etcc--tmux-iterm-hbar-cursor-string)
          (send-string-to-terminal etcc--iterm-hbar-cursor-string))
      (if (or (etcc--in-xterm?)
              (etcc--in-iterm?)
              (etcc--in-apple-terminal?)
              (etcc--in-dumb?))
          (send-string-to-terminal
           (concat (if (and etcc--enable-blink-cursor? blink-cursor)
                       etcc--xterm-hbar-blink-cursor-string
                     etcc--xterm-hbar-cursor-string)
                   (etcc--get-xterm-cursor-color-string evil-cursor)))))))

(defun etcc--set-box-cursor (evil-cursor)
  "Set cursor type box(block)."
  (if (etcc--in-gnome-terminal?)
      (with-temp-buffer
        (shell-command etcc--gnome-terminal-box-cursor-string t))
    (if (etcc--in-konsole?)
        (if (etcc--in-tmux?)
            (send-string-to-terminal etcc--tmux-iterm-box-cursor-string)
          (send-string-to-terminal etcc--iterm-box-cursor-string))
      (if (or (etcc--in-xterm?)
              (etcc--in-iterm?)
              (etcc--in-apple-terminal?)
              (etcc--in-dumb?))
          (send-string-to-terminal
           (concat (if (and etcc--enable-blink-cursor? blink-cursor)
                       etcc--xterm-box-blink-cursor-string
                     etcc--xterm-box-cursor-string)
                   (etcc--get-xterm-cursor-color-string evil-cursor)))))))

(defun etcc--set-cursor-shape (shape evil-cursor)
  "Set cursor shape."
  (cond
   ((eq shape 'box)               (etcc--set-box-cursor evil-cursor))
   ((eq shape 'evil-half-cursor)  (etcc--set-box-cursor evil-cursor))
   ((eq shape 'bar)               (etcc--set-bar-cursor evil-cursor))
   ((eq shape 'hbar)              (etcc--set-hbar-cursor evil-cursor))
   (t (etcc--set-box-cursor))))

(defun etcc--get-evil-emacs-state-cursor ()         
  (etcc--get-evil-cursor-shape evil-emacs-state-cursor))

;; (defun etcc--get-evil-evilified-state-cursor ()
;;   (etcc--get-evil-cursor-shape evil-evilified-state-cursor))

(defun etcc--get-evil-insert-state-cursor ()        
  (etcc--get-evil-cursor-shape evil-insert-state-cursor))

;; (defun etcc--get-evil-lisp-state-cursor ()
;;   (etcc--get-evil-cursor-shape evil-lisp-state-cursor))

(defun etcc--get-evil-motion-state-cursor ()
  (etcc--get-evil-cursor-shape evil-motion-state-cursor))

(defun etcc--get-evil-normal-state-cursor ()
  (etcc--get-evil-cursor-shape evil-normal-state-cursor))

(defun etcc--get-evil-operator-state-cursor ()
  (etcc--get-evil-cursor-shape evil-operator-state-cursor))

(defun etcc--get-evil-replace-state-cursor ()
  (etcc--get-evil-cursor-shape evil-replace-state-cursor))

(defun etcc--get-evil-visual-state-cursor ()
  (etcc--get-evil-cursor-shape evil-visual-state-cursor))

;; (defun etcc--get-evil-iedit-state-cursor ()
;;   (etcc--get-evil-cursor-shape evil-iedit-state-cursor))

;; (defun etcc--get-evil-iedit-insert-state-cursor ()
;;   (etcc--get-evil-cursor-shape evil-iedit-insert-state-cursor))

(defun etcc--set-evil-cursor ()
  "Set cursor type for Evil."
  (cond
   ;; ((evil-evilified-state-p)
   ;;  (etcc--set-cursor-shape (etcc--get-evil-evilified-state-cursor)))
   ((evil-insert-state-p)
    (etcc--set-cursor-shape (etcc--get-evil-insert-state-cursor) evil-insert-state-cursor))
   ;; ((evil-lisp-state-p)
   ;;  (etcc--set-cursor-shape (etcc--get-evil-lisp-state-cursor)))
   ((evil-motion-state-p)
    (etcc--set-cursor-shape (etcc--get-evil-motion-state-cursor) evil-motion-state-cursor))
   ((evil-normal-state-p)
    (etcc--set-cursor-shape (etcc--get-evil-normal-state-cursor) evil-normal-state-cursor))
   ((evil-operator-state-p)
    (etcc--set-cursor-shape (etcc--get-evil-operator-state-cursor) evil-operator-state-cursor))
   ((evil-replace-state-p)
    (etcc--set-cursor-shape (etcc--get-evil-replace-state-cursor) evil-replace-state-cursor))
   ((evil-visual-state-p)
    (etcc--set-cursor-shape (etcc--get-evil-visual-state-cursor) evil-visual-state-cursor))
   ((evil-emacs-state-p)
    (etcc--set-cursor-shape (etcc--get-evil-emacs-state-cursor) evil-emacs-state-cursor))
   ;; ((evil-iedit-state-p)
   ;;  (etcc--set-cursor-shape (etcc--get-evil-iedit-state-cursor)))
   ;; ((evil-iedit-insert-state-p)
   ;;  (etcc--set-cursor-shape (etcc--get-evil-iedit-insert-state-cursor)))
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
