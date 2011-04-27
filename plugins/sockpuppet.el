;;; sockpuppet.el - Emacs client interface for sockpuppet chat server
(defconst sockpuppet-version "0.8.0")

;; Copyright (c)2010 Jasonm23. (by)(nc)(sa) Some rights reserved.
;; Author: Jasonm23 <jasonm23@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;
;; Commentary:
;;
;; Sockpuppet chat client mode

(require 'easymenu)

;; customization options, e.g. Nickname, Color, Host.

(defgroup sockpuppet nil
  "Sockpuppet connection options"
  :group 'sockpuppet)

(defcustom sockpuppet-nick "GorgaR!" 
  "Nickname to use on sockpuppet"
  :group 'sockpuppet)

(defcustom sockpuppet-color "#555"
  "Color to use on sockpuppet"
  :type 'color
  :group 'sockpuppet)

(defcustom sockpuppet-host "10.1.6.110" 
  "Sockpuppet host ip"
  :group 'sockpuppet)

(defcustom sockpuppet-service 4440
  "Sockpuppet port number"
  :group 'sockpuppet)

;; End of customizations...
(defvar sockpuppet-process "sock-process"
  "name to use for sockpuppet connection process")

(defvar sockpuppet-buffer "sock-buffer"
  "name to use for sockpuppet output buffer")

(defvar sockpuppet-is-online nil)

(defvar sockpuppet-color-match-regexp  
  "^0x\\([0-9A-Fa-f]\\{6\\}\\)§"
  "Regexp for matching message color headers")

;;; Functions ==============================================

(defun sockpuppet-connect ()
  "Connect to sockpuppet"
  (interactive)
  (when (not sockpuppet-is-online)
    (open-network-stream sockpuppet-process sockpuppet-buffer sockpuppet-host sockpuppet-service)
    (setq sock-puppet-frame
    (make-frame '(
     (name . "sock-puppet") 
     (width . 45)
     (height . 45))))
    (select-frame sock-puppet-frame)
    (switch-to-buffer "sock-buffer")
    (set-buffer-process-coding-system 'utf-8-dos 'utf-8-dos)
    (setq buffer-read-only 1)
    (sockpuppet-mode)
    (setq sockpuppet-is-online t)
    (sockpuppet-send-message (concat "/nick " sockpuppet-nick))
    (sleep-for 1)
    (sockpuppet-send-message (concat "/color 0x" 
				     (8-bit-hex (nth 0 (color-values sockpuppet-color)))
				     (8-bit-hex (nth 1 (color-values sockpuppet-color)))
				     (8-bit-hex (nth 2 (color-values sockpuppet-color)))))))
  
(defun sockpuppet-disconnect ()
  "End the sockpuppet session and kill it's buffer and frame"
  (interactive)
  (when (eq sockpuppet-is-online t)
    (delete-process sockpuppet-process)
    (select-frame sock-puppet-frame)
    (delete-frame)
    (kill-buffer "sock-buffer")
    (setq sockpuppet-is-online nil)
    ))

(defun sockpuppet-send-message (message)
  "Send a string message to sockpuppet"
  (interactive "sSockpuppet Message: ")
  (when (eq sockpuppet-is-online t)
    (process-send-string sockpuppet-process message)))

(defun 8-bit-hex (i)
  (format "%02X" (floor (* 255 (/ (float i) (float 65535))))))

(defun sockpuppet-send-region (start end)
  "send the selected region to sockpuppet" 
  (interactive "r")
  (when (eq sockpuppet-is-online t)
    (process-send-region sockpuppet-process start end )))

(defun sockpuppet-show-version ()
  "Show the current version of SockPuppet for Emacs"
  (interactive)
  (message (format "SockPuppet for Emacs v%s" sockpuppet-version)))

(defun entry-color-block (beg end len)
  "Tidy up the message color string."
  (linum-mode 0)
  (let (in-message new-message color match-overlay)
    (setq in-message (buffer-substring-no-properties beg end))
    (setq new-message 
	  (replace-regexp-in-string sockpuppet-color-match-regexp "" in-message))
    (setq color (concat "#" (substring-no-properties in-message 2 8)))
    (if (not (eq nil (color-values color)))
	(progn
	  (replace-string in-message new-message nil beg end)
	  (setq  match-overlay (make-overlay (match-beginning 0) (match-end 0)))
	  (overlay-put match-overlay 'face `((:background ,color)
					     (:foreground ,(if (> 128.0 (color-luminance color))
						   "white" "black"))))) 
      nil)))

(defun color-luminance (color)
  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\")."
  (let* ((values (color-values color))
	 (r (car values))
	 (g (cadr values))
	 (b (caddr values)))
    (floor  (+ (* .2126 r) (* .7152 g) (* .0722 b)) 256 )))

(defun customize-sockpuppet ()
  "Customize sockpuppet."
  (interactive)
  (customize-group-other-window 'sockpuppet))

(defun reduce-text ()
  "reduce sockpuppet text"
  (interactive)
  (text-scale-decrease 1))

(defun enlarge-text ()
  "reduce sockpuppet text"
  (interactive)
  (text-scale-increase 1))

(defun google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Query: ")))))

;;; Keymap ================================================================

(defvar sockpuppet-mode-map
  (let ((sockpuppet-mode-map (make-keymap)))
    ;; Element insertion
    (define-key sockpuppet-mode-map "\C-m" 'sockpuppet-send-message)
    (define-key sockpuppet-mode-map '[(mouse-3)] 'browse-url-at-mouse)
    sockpuppet-mode-map)
  "Keymap for Markdown major mode.")

;;; Menu ==================================================================

(easy-menu-define sockpuppet-mode-menu sockpuppet-mode-map
  "Menu for SockPuppet mode"
  '("SockPuppet"
    ["Send Message" sockpuppet-send-message]
    ["Send Region" sockpuppet-send-region]
    "---"
    ["Enlarge text" enlarge-text]
    ["Reduce text" reduce-text]
    "---"
    ["Google Region" google]
    "---"
    ["Customize Sockpuppet" customize-sockpuppet ]
    "---"
    ["Version" sockpuppet-show-version]))

;;; Sockpuppet Mode =======================================================

(define-derived-mode sockpuppet-mode fundamental-mode "SockPuppet"
  "Major mode for using sockpuppet buffers"
  (add-hook 'after-change-functions 'entry-color-block nil 1)
  (easy-menu-add sockpuppet-mode-menu sockpuppet-mode-map)
  (global-set-key "\M-p" 'sockpuppet-send-region)
  (text-scale-adjust -1)
  (linum-mode 0)
)

(provide 'sockpuppet)
;; end-of-sockpuppet.el