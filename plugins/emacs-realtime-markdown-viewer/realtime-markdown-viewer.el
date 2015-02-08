;;; realtime-markdown-viewer.el --- Realtime Markdown editing / viewing

;; Author: Jason Milkins
;; Original Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/jasonm23/emacs-realtime-markdown-viewer

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
;;  A simplified fork of Syohei Yoshida's RTMV (only uses Ruby)

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'websocket)

(defgroup realtime-markdown-viewer nil
  "Realtime Markdown Viewer"
  :group 'text
  :prefix "rtmv:")

(defcustom rtmv:port 5021
  "Port number for web server App."
  :type 'integer
  :group 'realtime-markdown-viewer)

(defvar rtmv:websocket)

(defvar rtmv:module-path (file-name-directory load-file-name))

(defun rtmv:init-websocket (port)
  (let ((url (format "ws://0.0.0.0:%d/emacs" port)))
    (message "Connect to %s" url)
    (setq rtmv:websocket
          (websocket-open
           url
           :on-message (lambda (websocket frame)
                         (message "%s" (websocket-frame-payload frame)))
           :on-error (lambda (ws type err)
                       (message "error connecting"))
           :on-close (lambda (websocket) (setq wstest-closed t))))))

(defun rtmv:send-to-server ()
  (when realtime-markdown-viewer-mode
    (let ((str (buffer-substring-no-properties (point-min) (point-max))))
      (websocket-send-text rtmv:websocket str))))

(defvar rtmv:webapp-process nil)

(defun rtmv:webapp-launch-command (port)
  (format "ruby realtime_markdown_viewer.rb -p %d" port))

(defun rtmv:webapp-launch (port)
  (when (not rtmv:webapp-process)
    (let ((cmd (rtmv:webapp-launch-command port))
          (default-directory rtmv:module-path))
      (setq rtmv:webapp-process
            (start-process-shell-command "rtmv" "*realtime markdown*" cmd)))))

(defun rtmv:kill-process ()
  (when rtmv:webapp-process
    (with-demoted-errors "Inactive rtmv process (%s)"
      (kill-process rtmv:webapp-process))
    (setq rtmv:webapp-process nil)))

(defun rtmv:init ()
  (let ((port rtmv:port))
    (rtmv:webapp-launch port)
    (sleep-for 5)
    (rtmv:init-websocket port)
    (add-hook 'kill-emacs-hook 'rtmv:kill-process)
    (add-hook 'post-command-hook 'rtmv:send-to-server nil t)))

(defun rtmv:finalize ()
  (websocket-close rtmv:websocket)
  (remove-hook 'post-command-hook 'rtmv:send-to-server t)
  (rtmv:kill-process))

(define-minor-mode realtime-markdown-viewer-mode
  "Realtime Markdown Viewer mode"
  :group      'realtime-markdown-viewer
  :init-value nil
  :global     nil
  (if realtime-markdown-viewer-mode
      (rtmv:init)
    (rtmv:finalize)))

(provide 'realtime-markdown-viewer)

;;; realtime-markdown-viewer.el ends here
