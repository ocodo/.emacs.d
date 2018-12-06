;;; kite-mini.el --- Remotely evaluate JavaScript in the WebKit debugger
;;
;; Copyright (c) 2014, 2015  Tung Dao <me@tungdao.com>
;;
;; Author: Tung Dao <me@tungdao.com>
;; URL: https://github.com/tungd/kite-mini.el
;; Keywords: webkit
;; Version: 0.2.0
;; Package-Requires: ((dash "2.11.0") (websocket "1.5"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Minor mode for remote evaluate JavaScript in WebKit debugger, with
;; a little icing. Included features are:
;; - Evaluate JavaScript (running at top level)
;; - Modify and update external JavaScript files (live)
;; - Reload
;;
;; Planned features includes:
;; - Live reload stylesheets (without reload)
;; - JavaScript console (REPL)
;;
;;; Code:

(require 'url)
(require 'json)
(require 'dash)
(require 'websocket)


(defcustom kite-mini-remote-host "127.0.0.1"
  "Default host for connection to WebKit remote debugging API."
  :group 'kite-mini)

(defcustom kite-mini-remote-port 9222
  "Default port for connection to WebKit remote debugging API."
  :group 'kite-mini)

(defvar kite-mini-socket nil
  "Websocket connection to WebKit remote debugging API.")

(defvar kite-mini-rpc-id 0)
(defvar kite-mini-rpc-callbacks nil)
(defvar kite-mini-rpc-scripts nil
  "List of JavaScript files available for live editing.")


(defun kite-mini-encode (data)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-encode data)))

(defun kite-mini-decode (data)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-read-from-string data)))

(defun kite-mini-next-rpc-id ()
  (setq kite-mini-rpc-id (+ 1 kite-mini-rpc-id)))


(defun kite-mini-register-callback (id fn)
  (let ((hook (intern (number-to-string id) kite-mini-rpc-callbacks)))
    (add-hook hook fn t)))

(defun kite-mini-dispatch-callback (id data)
  (let ((hook (intern (number-to-string id) kite-mini-rpc-callbacks)))
    (when hook
      (run-hook-with-args hook data)
      (unintern hook kite-mini-rpc-callbacks))))


(defun kite-mini-on-open (socket)
  (message "Kite: connected."))

(defun kite-mini-on-close (socket)
  (message "Kite: disconnected."))

(defun kite-mini-on-script-parsed (data)
  (let ((extension? (plist-get data :isContentScript))
        (url (plist-get data :url))
        (id (plist-get data :scriptId)))
    (when (and (eq extension? :json-false) (not (string-equal "" url)))
      (add-to-list 'kite-mini-rpc-scripts (list :id id :url url)))))

(defun kite-mini-on-script-failed-to-parse (data)
  (kite-mini-console-append (format "%s" data)))

(defun kite-mini-on-message-added (data)
  (let* ((message (plist-get data :message))
         (url (plist-get message :url))
         (column (plist-get message :column))
         (line (plist-get message :line))
         (type (plist-get message :type))
         (level (plist-get message :level))
         (text (plist-get message :text)))
    ;; TODO: add colors based on level
    (kite-mini-console-append (propertize
                               (format "%s: %s\t%s (line: %s column: %s)"
                                       level text url line column)
                               'font-lock-face (intern (format "kite-mini-log-%s" level))))))

(defun kite-mini-on-message (socket data)
  (let* ((data (kite-mini-decode (websocket-frame-payload data)))
         (method (plist-get data :method))
         (params (plist-get data :params)))
    (pcase method
      ("Debugger.scriptParsed" (kite-mini-on-script-parsed params))
      ;; we are getting an error in Console.messageAdded
      ;; ("Debugger.scriptFailedToParse" (kite-mini-on-script-failed-to-parse params))
      ("Console.messageAdded" (kite-mini-on-message-added params))
      ;; ;; TODO: do something usefull here, possibly great for REPL
      ("Console.messageRepeatCountUpdated")
      ;; nil -> These are return messages from RPC calls, not notification
      (_ (if method
             (message "Kite: %s" data) ; Generic fallback, only used in development
           (kite-mini-dispatch-callback (plist-get data :id)
                                        (plist-get data :result)))))))

(defun kite-mini-call-rpc (method &optional params callback)
  (let ((id (kite-mini-next-rpc-id)))
    (when callback
      (kite-mini-register-callback id callback))
    (websocket-send-text
     kite-mini-socket
     (kite-mini-encode (list :id id
                             :method method
                             :params params)))))

(defun kite-mini-open-socket (url)
  (websocket-open url
                  :on-open #'kite-mini-on-open
                  :on-message #'kite-mini-on-message
                  :on-close #'kite-mini-on-close))

(defun kite-mini-get-json (url)
  (let* ((url-request-method "GET")
         (url-http-attempt-keepalives nil)
         (json-array-type 'list)
         (json-object-type 'plist))
    (with-current-buffer (url-retrieve-synchronously url)
      (if (not (eq 200 (url-http-parse-response)))
          (error "Unable to connect to host.")
        (goto-char (+ 1 url-http-end-of-headers))
        (json-read)))))

(defun kite-mini-get-tabs (host port)
  (let* ((url (url-parse-make-urlobj
               "http" nil nil host port "/json"))
         (tabs (kite-mini-get-json url)))
    (-filter (lambda (tab)
               (and (plist-get tab :webSocketDebuggerUrl)
                    (string-equal (plist-get tab :type) "page")))
             tabs)))

(defun kite-mini-tab-completion (tab)
  (let ((title (plist-get tab :title))
        (url (plist-get tab :url)))
    (cons (format "%s" title) tab)))

(defun kite-mini-select-tab (host port)
  (let* ((tabs (mapcar #'kite-mini-tab-completion
                       (kite-mini-get-tabs host port)))
         (selection (completing-read
                     "Tab: " tabs nil t "" nil (caar tabs)))
         (tab (cdr (assoc selection tabs))))
    (plist-get tab :webSocketDebuggerUrl)))


(defun kite-mini-connect ()
  (interactive)
  (kite-mini-disconnect)
  (let* ((socket-url (kite-mini-select-tab kite-mini-remote-host
                                           kite-mini-remote-port)))
    (setq kite-mini-socket (kite-mini-open-socket socket-url))
    (kite-mini-call-rpc "Console.enable")
    (kite-mini-call-rpc "Debugger.enable")
    (kite-mini-call-rpc "Network.setCacheDisabled" '(:cacheDisabled t))))

(defun kite-mini-disconnect ()
  (interactive)
  (when (websocket-openp kite-mini-socket)
    (websocket-close kite-mini-socket)
    (setq kite-mini-socket nil
          kite-mini-rpc-scripts nil)))


(defun kite-mini-send-eval (code &optional callback)
  (kite-mini-call-rpc
   "Runtime.evaluate"
   (list :expression code
         :returnByValue t)
   callback))

(defun kite-mini-remove-script (script)
  (setq kite-mini-rpc-scripts
        (delete script kite-mini-rpc-scripts)))

(defun kite-mini-script-id (file)
  (let* ((name (file-name-nondirectory file))
         (script (--find (string-suffix-p name (plist-get it :url))
                         kite-mini-rpc-scripts)))
    (when script (plist-get script :id))))

(defun kite-mini-update ()
  (interactive)
  (let ((id (kite-mini-script-id (buffer-file-name)))
        (source (buffer-substring-no-properties
                 (point-min) (point-max))))
    (if id
        (kite-mini-call-rpc
         "Debugger.setScriptSource"
         (list :scriptId id :scriptSource source))
      (message "No matching script for current buffer."))))

(defun kite-mini-reload ()
  (interactive)
  (kite-mini-call-rpc
   "Page.reload"
   (list :ignoreCache t)))

(defun kite-mini-evaluate-region-or-line (&optional args)
  (interactive "*P")
  (let ((start (if (region-active-p)
                   (region-beginning)
                 (line-beginning-position)))
        (end (if (region-active-p)
                 (region-end)
               (line-end-position))))
    (kite-mini-send-eval (buffer-substring-no-properties start end))))


(defvar kite-mini-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-c C-c") #'kite-mini-evaluate-region-or-line)
      (define-key map (kbd "C-c C-k") #'kite-mini-update)
      (define-key map (kbd "C-c C-r") #'kite-mini-reload)))
  "Keymap for Kite Mini mode.")

;;;###autoload
(defun turn-on-kite-mini-mode ()
  "Turn on Kite Mini mode.")

;;;###autoload
(defun turn-off-kite-mini-mode ()
  "Turn off Kite Mini mode.")

;;;###autoload
(define-minor-mode kite-mini-mode
  "Minor mode for interact with WebKit remote debugging API."
  :global nil
  :group 'kite-mini
  :init-value nil
  :lighter ""
  :keymap kite-mini-mode-map
  (if kite-mini-mode
      (turn-on-kite-mini-mode)
    (turn-off-kite-mini-mode)))

(provide 'kite-mini)
;;; kite-mini.el ends here
