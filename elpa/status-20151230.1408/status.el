;;; status.el --- notification area support for Emacs.

;; Copyright (C) 2007, 2012, 2014 Tom Tromey <tom@tromey.com>

;; Author: Tom Tromey <tom@tromey.com>
;; Version: 0.3
;; Keywords: frames multimedia

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

;; Commentary:

;; There are no user-visible features of this module, only features
;; for Emacs Lisp programs.  You may like to use erc-status.el, which
;; provides some nice notification area support for ERC.

;; Change log:

;; 2007-03-24   updated documentation, added autoload, added variable
;;              for path
;; 2007-03-01   generate new buffer name for each status-new
;; 2007-01-29   reorder code in status-process-filter

;;; Code:

(defvar status-python "python"
  "Name of the Python interpreter.")

(defvar status-status.py-path
  (expand-file-name "status.py" (file-name-directory load-file-name))
  "Path to status.py.")

;; Callback function for a left-click on the status icon.  Internal.
(defvar status-click-callback)
(make-variable-buffer-local 'status-click-callback)

;; Data used by the process filter.  Internal.
(defvar status-input-string)
(make-variable-buffer-local 'status-input-string)

;; Default to the GNU.
(defvar status-default-icon
  (expand-file-name
   "images/icons/hicolor/scalable/apps/emacs.svg"
   data-directory))

(defvar status--debug nil)

;; Convenience function to send to process.
;; This is mostly handy for debugging.
(defun status--send (icon string)
  (process-send-string icon string)
  (when status--debug
    (message "send: %s" string)))

;;;###autoload
(defun status-new ()
  "Create a new status icon and return it."
  (let ((result (start-process "status-icon"
			       (generate-new-buffer-name " *status-icon*")
			       status-python
			       status-status.py-path)))
    (set-process-filter result 'status--process-filter)
    (status--send result "click: @@@click@@@\n")
    (status-set-icon result status-default-icon)
    (set-process-query-on-exit-flag result nil)
    result))

(defun status-set-click-callback (status-icon function)
  "Set the click callback function.
STATUS-ICON is the status icon.  FUNCTION is the callback function.
It will be called with no arguments when the user clicks on the
status icon."
  (with-current-buffer (process-buffer status-icon)
    (setq status-click-callback function)))

;; Set the icon, either to a file name or to a stock icon name.
(defun status-set-icon (status-icon file-or-name)
  "Set the image for the status icon.
STATUS-ICON is the status icon object.  FILE-OR-NAME is either a file
name, or it is one of the stock icon names: \"warning\", \"info\",
\"question\", or \"error\"."
  (status--send status-icon (concat "icon: " file-or-name "\n")))

(defun status-set-visible (status-icon arg)
  "Make the status icon visible or invisible.
If ARG is nil, make icon invisible.  Otherwise, make it visible"
  (status--send status-icon (concat "visible: "
				    (if arg "true" "false")
				    "\n")))

(defun status-post-message (status-icon text)
  "Post a message by the status icon.
STATUS-ICON is the status icon object.  TEXT is the text to post.
It will appear as a popup near the icon.  TEXT should not contain
any newlines."
  (status--send status-icon (concat "message:" text "\n")))

(defun status-set-tooltip (status-icon text)
  "Set the tooltip for the status icon."
  (status--send status-icon (concat "tooltip: " text "\n")))

(defun status-delete (status-icon)
  "Destroy the status icon."
  (delete-process status-icon))

(defun status-set-blink (status-icon arg)
  "Enable or disable blinking of the status icon.
If ARG is nil, blinking will be disabled.  Otherwise it will be enabled."
  (status--send status-icon (concat "blink: "
				    (if arg "true" "false")
				    "\n")))

(defun status--process-filter (status-icon string)
  (with-current-buffer (process-buffer status-icon)
    (when status--debug
      (message "status <- %s" string))
    (setq status-input-string (concat status-input-string string))
    (let ((index nil))
      (while (setq index (cl-search "\n" status-input-string))
	(let ((cb-name (substring status-input-string 0 index)))
	  (setq status-input-string
		(substring status-input-string (+ 1 index)))
	  ;; Look for the callback.
	  (if (equal cb-name "@@@click@@@")
	      (and status-click-callback
		   (funcall status-click-callback))
	    ;; Got some kind of error.
	    (insert cb-name "\n")))))))

(provide 'status)

;;; status.el ends here
