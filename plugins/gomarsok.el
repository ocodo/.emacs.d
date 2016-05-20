;;; gomarsok.el --- Realtime Markdown editing / viewing via Gomarsok

;; Author: Jason Milkins

;; URL: https://github.com/jasonm23/emacs-gomarsok

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
;;
;;  Based on Github Markdown view, but using a Go based socket server.
;;

;;; Code:

(eval-when-compile  (require 'cl))

(require 'websocket)

(defgroup gomarsok nil
  "Gomarsok Realtime Emacs Markdown View"
  :group 'text
  :prefix "gomarsok-")

(defcustom gomarsok-port 8080
  "Port number for gomarsok server."
  :type 'integer
  :group 'gomarsok)

(defvar gomarsok-websocket)

(defvar gomarsok-module-path (file-name-directory load-file-name))

(defun gomarsok-init-websocket (port)
  (let ((url (format "ws://0.0.0.0:%d/echo" port)))
    (message "Connect to %s" url)
    (setq gomarsok-websocket
          (websocket-open
           url
           :on-message (lambda (websocket frame)
                         (message "%s" (websocket-frame-payload frame)))
           :on-error (lambda (ws type err)
                       (message "error connecting"))
           :on-close (lambda (websocket)
                       (message "websocket closed"))))))

(defun gomarsok-send-to-server ()
  (when gomarsok-mode
    (let ((str (buffer-substring-no-properties (point-min) (point-max))))
      (websocket-send-text gomarsok-websocket str))))

;; (defvar gomarsok-webapp-process nil)

;; (defun gomarsok-webapp-launch-command (port)
;;   (let ((ruby "bundle exec ruby"))
;;     (with-demoted-errors "RVM not available: %S"
;;       (when (and (boundp 'rvm--current-ruby-binary-path) (not (eq nil rvm--current-ruby-binary-path)))
;;         (setq ruby (format "bundle exec %sruby" (car rvm--current-ruby-binary-path)))))
;;     (format "%s gomarsok.rb -p %d" ruby port)))

;; (defun gomarsok-webapp-launch (port)
;;   (when (not gomarsok-webapp-process)
;;     (let ((cmd (gomarsok-webapp-launch-command port))
;;           (default-directory gomarsok-module-path))
;;       (setq gomarsok-webapp-process
;;             (start-process-shell-command "gomarsok" "*gomarsok realtime markdown*" cmd)))))

;; (defun gomarsok-kill-process ()
;;   (when gomarsok-webapp-process
;;     (with-demoted-errors "Inactive remv process (%s)"
;;       (kill-process gomarsok-webapp-process))
;;     (setq gomarsok-webapp-process nil)))

(defun gomarsok-init ()
  (let ((port gomarsok-port))
    ; (gomarsok-webapp-launch port)
    (sleep-for 5)
    (gomarsok-init-websocket port)
    ; (add-hook 'kill-emacs-hook 'gomarsok-kill-process)
    (add-hook 'post-command-hook 'gomarsok-send-to-server nil t)))

(defun gomarsok-finalize ()
  (websocket-close gomarsok-websocket)
  (remove-hook 'post-command-hook 'gomarsok-send-to-server t)
  ;(gomarsok-kill-process)
  )

(define-minor-mode gomarsok-mode
  "Realtime Emacs Markdown via Gomarsok"
  :group      'gomarsok
  :init-value nil
  :global     nil
  (if gomarsok-mode
      (gomarsok-init)
    (gomarsok-finalize)))

(provide 'gomarsok)

;;; gomarsok.el ends here
