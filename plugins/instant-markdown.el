;;; instant-markdown.el --- A instant-markdown-d client for GNU Emacs

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/itiut/emacs-instant-markdown
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Preview Markdown files in your browser by instant-markdown-d.
;; This requires instant-markdown-d to be installed.
;; To get information about instant-markdown-d, visit
;; https://github.com/suan/instant-markdown-d

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'url)
(require 'url-http)

(defgroup instant-markdown nil
  "Emacs `instant-markdown' client"
  :group 'markdown)

(defcustom instant-markdown:executable "instant-markdown-d"
  "The path of the instant-markdown-d executable."
  :type 'string
  :group 'instant-markdown)

(defcustom instant-markdown:port 8090
  "Port number of `instant-markdown'"
  :type 'integer
  :group 'instant-markdown)

(defcustom instant-markdown:turn-on-auto-refresh-delay 1
  "The number of seconds of delay time from enabling instant-markdown-mode to turning on auto refresh."
  :type 'number
  :group 'instant-markdown)

(defcustom instant-markdown:idle-delay 0.25
  "The number of seconds of idle delay time before auto refreshing."
  :type 'number
  :group 'instant-markdown)

(defcustom instant-markdown-mode-lighter " i-md"
  "Lighter of instant-markdown-mode."
  :type 'string
  :group 'instant-markdown)

(defvar instant-markdown-mode nil)

(defun instant-markdown:default-callback (status)
  (message "%s" status))

(defun instant-markdown:request-url ()
  (format "http://localhost:%d" instant-markdown:port))

(defun instant-markdown:request (method input &optional cb)
  (let ((url-request-method method)
        (url-request-data input)
        (url-request-extra-headers '(("Content-Type" . "text/x-markdown")))
        (callback (or cb #'instant-markdown:default-callback)))
    (url-retrieve (instant-markdown:request-url) callback)))

(defvar instant-markdown:modified-tick-last nil)

(defun instant-markdown:refresh-if-buffer-modified ()
  (when instant-markdown-mode
    (let ((modified-tick (buffer-chars-modified-tick)))
      (when (or (not (numberp instant-markdown:modified-tick-last))
                (/= instant-markdown:modified-tick-last modified-tick))
        (setq instant-markdown:modified-tick-last modified-tick)
        (instant-markdown:refresh)))))

(defun instant-markdown:refresh ()
  (let ((markdown (buffer-substring-no-properties (point-min) (point-max))))
    (instant-markdown:request "PUT" markdown)))

(defvar instant-markdown:server-proc nil)

(defun instant-markdown:start ()
  (unless instant-markdown:server-proc
    (let ((proc (start-process "instant-markdown-d" "*instant-markdown-d*"
                               instant-markdown:executable)))
      (unless proc
        (error (format "Failed exec `%s'" instant-markdown:executable)))
      (sit-for 0.5)
      (setq instant-markdown:server-proc proc))
    (instant-markdown:turn-on-auto-refresh)))

(defun instant-markdown:stop-callback (status)
  (when (process-live-p instant-markdown:server-proc)
    (kill-process instant-markdown:server-proc))
  (setq instant-markdown:server-proc nil))

(defun instant-markdown:stop ()
  (unless instant-markdown:server-proc
    (error (format "`%s' does not started" instant-markdown:executable)))
  (instant-markdown:request "DELETE" nil #'instant-markdown:stop-callback)
  (instant-markdown:turn-off-auto-refresh))

(defun instant-markdown:turn-on-auto-refresh ()
  (run-at-time instant-markdown:turn-on-auto-refresh-delay nil
               '(lambda ()
                  (instant-markdown:update-timer instant-markdown:idle-delay))))

(defun instant-markdown:turn-off-auto-refresh ()
  (instant-markdown:cancel-timer)
  (setq instant-markdown:modified-tick-last nil))

(defvar instant-markdown:timer nil)

(defun instant-markdown:update-timer (value)
  (instant-markdown:cancel-timer)
  (setq instant-markdown:timer
        (and value (/= value 0)
             (run-with-idle-timer value 'repeat 'instant-markdown:refresh-if-buffer-modified))))

(defun instant-markdown:cancel-timer ()
  (when instant-markdown:timer
    (cancel-timer instant-markdown:timer)))

;;;###autoload
(define-minor-mode instant-markdown-mode
  "Minor mode for previewing markdown via instant-markdown-d."
  :lighter instant-markdown-mode-lighter
  :group 'instant-markdown
  (if instant-markdown-mode
      (instant-markdown:start)
    (progn
      (instant-markdown:stop)
      (message "Instant-Markdown mode disabled"))))

(provide 'instant-markdown)

;;; instant-markdown.el ends here
