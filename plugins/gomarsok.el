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

(defun gomarsok-init-websocket (port)
  (let ((url (format "ws://localhost:%d/ws" port)))
    (message "Connect to %s" url)
    (setq gomarsok-websocket
          (websocket-open
           url
           :on-error (lambda (ws type err)
                       (message "error connecting"))
           :on-close (lambda (websocket)
                       (message "websocket closed"))))))

(defun gomarsok-send-to-server ()
  (when gomarsok-mode
    (let ((str (buffer-substring-no-properties (point-min) (point-max))))
      (websocket-send-text gomarsok-websocket str))))

(defun gomarsok-init ()
  (let ((port gomarsok-port))
    (gomarsok-init-websocket port)
    (add-hook 'post-command-hook 'gomarsok-send-to-server nil t)))

(defun gomarsok-finalize ()
  (websocket-close gomarsok-websocket)
  (remove-hook 'post-command-hook 'gomarsok-send-to-server t))

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
