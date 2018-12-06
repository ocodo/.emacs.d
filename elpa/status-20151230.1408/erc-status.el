;;; erc-status.el --- notification area support for ERC

;; Copyright (C) 2007, 2014 Tom Tromey <tromey@redhat.com>

;; Author: Tom Tromey <tromey@redhat.com>
;; Version: 0.2
;; Keywords: comm

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This provides nice support for the notification area to ERC.  In
;; particular it:
;; * Will blink the icon when you get a private message or are paged
;;   in a channel.
;; * Left-click on the blinking icon will show the appropriate channel
;;   buffer in some frame (which is then raised).  If there are
;;   multiple pages at once, it will show one and you can click again
;;   to go to the next one.
;; * Will pop up notification bubbles when you connect to or
;;   disconnect from a server.
;; This is regular erc module named 'status'; you can enable it as you
;; would any other module.

;;; Change log:

;; 2007-02-15  raise frame on left click
;; 2007-02-13  make sure priv message added after auto-query
;; 2007-02-08  turn into an ERC module
;; 2007-02-08  another try at private messages
;; 2007-01-29  try to make private messages work.  show buffer on click

;; TO DO:
;; - make tool tip show some kind of real status ...?
;; - use a nicer icon
;; - integrate with auto-query a bit better
;; - let left click use specified frame or make a new frame?

;; Probably not its final name, but this is the name it was posted
;; under.
(require 'status)

(require 'erc)

;; The status icon object.
(defvar erc-status-status-icon nil)

;; List of ERC buffers that caused the status icon to blink.
(defvar erc-status-buffer-list nil)

(defun erc-status-remove-buffer (buffer)
  ;; If the list is not empty, and removing an element makes the list
  ;; empty, stop blinking.
  (and erc-status-buffer-list
       (not (setq erc-status-buffer-list (delq buffer erc-status-buffer-list)))
       (status-set-blink erc-status-status-icon nil)))

(defun erc-status-add-buffer (buffer)
  (unless (erc-buffer-visible buffer)
    (status-set-blink erc-status-status-icon t)
    (unless (memq buffer erc-status-buffer-list)
      (setq erc-status-buffer-list (cons buffer
					 erc-status-buffer-list)))))

(defun erc-status-match-hook (match-type nick message)
  ;; Look for user's nick and make the icon blink.
  (if (eq match-type 'current-nick)
      (erc-status-add-buffer (current-buffer))))

(defun erc-status-buffer-killed ()
  ;; If one of our buffers was killed, remove it.
  (erc-status-remove-buffer (current-buffer)))

(defun erc-status-window-configuration-changed ()
  (let ((new-list))
    (dolist (buffer erc-status-buffer-list)
      (unless (erc-buffer-visible buffer)
	(setq new-list (cons buffer new-list))))
    (unless (setq erc-status-buffer-list new-list)
      (status-set-blink erc-status-status-icon nil))))

(defun erc-status-disconnected (nick ip reason)
  ;; FIXME: should mention the server from which we were disconnected.
  ;; FIXME: add a :action to reconnect.
  (status-post-message erc-status-status-icon
		       (concat "Disconnected: " reason)))

(defun erc-status-after-connect (server nick)
  (status-post-message erc-status-status-icon
		       (concat "Connected to " server " as " nick)))

(defun erc-status-select-first-buffer ()
  "Switch to the first ERC buffer requiring your attention.
If there is no such buffer, do nothing."
  (when erc-status-buffer-list
    (switch-to-buffer (car erc-status-buffer-list))
    (raise-frame)))



;; From: http://www.emacswiki.org/cgi-bin/wiki/ErcPageMe
;; Then modified to suit.

(defun erc-status-PRIVMSG (proc parsed)
  (let* ((nick (car (erc-parse-user (erc-response.sender parsed))))
	 (target (car (erc-response.command-args parsed)))
	 (msg (erc-response.contents parsed))
	 (query  (if (not erc-query-on-unjoined-chan-privmsg)
		     nick
		   (if (erc-current-nick-p target)
		       nick
		     target))))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg)))
      ;; Note: assumes you are using auto-query.
      (erc-status-add-buffer (erc-get-buffer query proc))))
  ;; Always return nil.
  nil)



(define-erc-module status nil
  "Notification area support for ERC."
  ;; Enable.
  ((unless erc-status-status-icon
     (setq erc-status-status-icon (status-new))
     (status-set-tooltip erc-status-status-icon
			 "ERC - IRC client for Emacs")
     (status-set-click-callback erc-status-status-icon
				'erc-status-select-first-buffer)
     (status-set-visible erc-status-status-icon t))
   (add-hook 'erc-text-matched-hook 'erc-status-match-hook)
   (add-hook 'kill-buffer-hook 'erc-status-buffer-killed)
   (add-hook 'window-configuration-change-hook
	     'erc-status-window-configuration-changed)
   (add-hook 'erc-after-connect 'erc-status-after-connect)
   (add-hook 'erc-disconnected-hook 'erc-status-disconnected)
   ;; FIXME: Must come *after* erc-auto-query.  Some sort of
   ;; auto-query hook or the like would be good here.
   (add-hook 'erc-server-PRIVMSG-functions 'erc-status-PRIVMSG t))

  ;; Disable.
  ((when erc-status-status-icon
     (status-delete erc-status-status-icon)
     (setq erc-status-status-icon nil))
   (remove-hook 'erc-text-matched-hook 'erc-status-match-hook)
   (remove-hook 'kill-buffer-hook 'erc-status-buffer-killed)
   (remove-hook 'window-configuration-change-hook
		'erc-status-window-configuration-changed)
   (remove-hook 'erc-after-connect 'erc-status-after-connect)
   (remove-hook 'erc-disconnected-hook 'erc-status-disconnected)
   (remove-hook 'erc-server-PRIVMSG-functions 'erc-status-PRIVMSG)))

;;; erc-status.el ends here
