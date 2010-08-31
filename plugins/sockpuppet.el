;;; sockpuppet.el - Emacs client interface for sockpuppet chat server
(defconst sockpuppet-version "0.2")

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
;; Sockpuppet Mode

(require 'easymenu)

;; customization options, e.g. Nickname, Color, Host.

(defgroup sockpuppet nil
  "Sockpuppet connection options"
  :group 'sockpuppet)

(defcustom sockpuppet-nick "Jason" 
  "Nickname to use on sockpuppet"
  :group 'sockpuppet)

(defcustom sockpuppet-color "#555"
  "Color to use on sockpuppet"
  :type 'color
  :group 'sockpuppet)

(defcustom sockpuppet-host "10.1.6.107" 
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

;;; Keymap ================================================================

(defvar sockpuppet-mode-map
  (let ((sockpuppet-mode-map (make-keymap)))
    ;; Element insertion
    (define-key sockpuppet-mode-map "\C-m" 'sockpuppet-send-message)
    sockpuppet-mode-map)
  "Keymap for Markdown major mode.")

;;; Menu ==================================================================

(easy-menu-define sockpuppet-mode-menu sockpuppet-mode-map
  "Menu for SockPuppet mode"
  '("SockPuppet"
    ["Send Message" sockpuppet-send-message]
    ["Send Region" sockpuppet-send-region]
    "---"
    ["Version" sockpuppet-show-version]))

;;; Sockpuppet Mode =======================================================

(define-derived-mode sockpuppet-mode fundamental-mode "SockPuppet"
  "Major mode for using sockpuppet buffers"
  (easy-menu-add sockpuppet-mode-menu sockpuppet-mode-map)
  (global-set-key "\M-p" 'sockpuppet-send-region))

(provide 'sock-puppet)
;; end-of-sockpuppet.el
