;;; realtime-emacs-markdown-view.el --- Realtime Markdown editing / viewing in Emacs

;; Author: Jason Milkins

;; Original Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/jasonm23/realtime-emacs-markdown-view

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
;;  Provides support for Github Flavoured Markdown and uses a CSS theme
;;  broadly based on Github Markdown view.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'websocket)

(defgroup realtime-emacs-markdown-view nil
  "Realtime Emacs Markdown View"
  :group 'text
  :prefix "remv:")

(defcustom remv:port 5021
  "Port number for Ruby/SinatraRb web server."
  :type 'integer
  :group 'realtime-emacs-markdown-view)

(defvar remv:websocket)

(defvar remv:module-path (file-name-directory load-file-name))

(defun remv:init-websocket (port)
  (let ((url (format "ws://0.0.0.0:%d/emacs" port)))
    (message "Connect to %s" url)
    (setq remv:websocket
          (websocket-open
           url
           :on-message (lambda (websocket frame)
                         (message "%s" (websocket-frame-payload frame)))
           :on-error (lambda (ws type err)
                       (message "error connecting"))
           :on-close (lambda (websocket) (setq wstest-closed t))))))

(defun remv:send-to-server ()
  (when realtime-emacs-markdown-view-mode
    (let ((str (buffer-substring-no-properties (point-min) (point-max))))
      (websocket-send-text remv:websocket str))))

(defvar remv:webapp-process nil)

(defun remv:webapp-launch-command (port)
  (let ((ruby "bundle exec ruby"))
    (with-demoted-errors "RVM not available: %S"
      (when (and (boundp 'rvm--current-ruby-binary-path) (not (eq nil rvm--current-ruby-binary-path)))
        (setq ruby (format "bundle exec %sruby" (car rvm--current-ruby-binary-path)))))
    (format "%s realtime-emacs-markdown-view.rb -p %d" ruby port)))

(defun remv:webapp-launch (port)
  (when (not remv:webapp-process)
    (let ((cmd (remv:webapp-launch-command port))
          (default-directory remv:module-path))
      (setq remv:webapp-process
            (start-process-shell-command "remv" "*realtime markdown*" cmd)))))

(defun remv:kill-process ()
  (when remv:webapp-process
    (with-demoted-errors "Inactive remv process (%s)"
      (kill-process remv:webapp-process))
    (setq remv:webapp-process nil)))

(defun remv:init ()
  (let ((port remv:port))
    (remv:webapp-launch port)
    (sleep-for 5)
    (remv:init-websocket port)
    (add-hook 'kill-emacs-hook 'remv:kill-process)
    (add-hook 'post-command-hook 'remv:send-to-server nil t)))

(defun remv:finalize ()
  (websocket-close remv:websocket)
  (remove-hook 'post-command-hook 'remv:send-to-server t)
  (remv:kill-process))

(define-minor-mode realtime-emacs-markdown-view-mode
  "Realtime Emacs Markdown View mode"
  :group      'realtime-emacs-markdown-view
  :init-value nil
  :global     nil
  (if realtime-emacs-markdown-view-mode
      (remv:init)
    (remv:finalize)))

(provide 'realtime-emacs-markdown-view)

;;; realtime-emacs-markdown-view.el ends here
