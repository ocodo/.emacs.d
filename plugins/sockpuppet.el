;;; sockpuppet.el - Emacs client interface for sockpuppet chat server
(defconst sockpuppet-version "0.1")

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
;; Allows you to connect to sock.exe (chat server) and provides 
;; customization options, e.g. Nickname setting.

(defgroup sockpuppet nil
  "Sockpuppet connection options"
  :tag "SockPuppet")

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

(defvar sockpuppet-process "sock-process"
  "name to use for sockpuppet connection process")

(defvar sockpuppet-buffer "sock-buffer"
  "name to use for sockpuppet output buffer")

(defun sockpuppet-connect ()
  "Connect to sockpuppet"
  (interactive)
  (when (not (get-process sockpuppet-process))
    (open-network-stream sockpuppet-process sockpuppet-buffer sockpuppet-host sockpuppet-service)
    (sockpuppet-send-message (concat "/nick " sockpuppet-nick))
    (sleep-for 1)
    (sockpuppet-send-message (concat "/color 0x" 
				     (8-bit-hex (nth 0 (color-values sockpuppet-color)))
				     (8-bit-hex (nth 1 (color-values sockpuppet-color)))
				     (8-bit-hex (nth 2 (color-values sockpuppet-color)))))))

(defun sockpuppet-disconnect ()
  (interactive)
  (when (get-process sockpuppet-process)
    (delete-process sockpuppet-process)))

(defun sockpuppet-send-message (message)
  "Send a string message to sockpuppet"
  (interactive "sSockpuppet Message: ")
  (when (get-process sockpuppet-process)
    (print message)
    (process-send-string sockpuppet-process message)))

(defun 8-bit-hex ( i )
  (format "%02X" (floor (* 255 (/ (float i) (float 65535))))))

(defun sockpuppet-send-region (start end)
  "send the selected region to sockpuppet" 
  (interactive "r")
  (when (get-process sockpuppet-process)
    (print (buffer-substring-no-properties start end))
    (process-send-region sockpuppet-process start end )))

(provide 'sock-puppet)

;; end-of-sockpuppet.el
