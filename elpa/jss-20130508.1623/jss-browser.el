;;; jss-browser.el -- jss mode for viewing information about a running browser
;;
;; Copyright (C) 2013 Edward Marco Baringer
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(require 'cl)
(require 'eieio)
(require 'jss-utils)
(require 'jss-browser-api)

(defvar jss-browser nil
  "Dummy value used to pass an argumnet to the function jss-browser-mode.")

(make-variable-buffer-local
 (defvar jss-current-browser-instance nil))

(define-derived-mode jss-browser-mode jss-super-mode "JSS Browser"
  "This is mode used by buffers created with jss-connect. It serves,
mainly, to list the tabs in the browser that can be debugged.

The first line specifies the backend, `webkit` or `firefox` used
to communicate with the browser and the host and port we're
currently connected to (or trying to connect to).

For each tab we list the currently visited url and provide a
button, \"[open console]\" which jumps to the corresponding tab's
console buffer.

The browser mode also displays a list of links to the jss
doucementation.

Note: While the browser buffer attempts to keep itself in sync
with the state of the browser (by refreshing when opening and
closing jss consoles), it is possible for changes to be made in
the browser which aren't communicated to jss. For this reason
manually running jss-browser-mode-refresh (usually bound to
\"g\") will required from time to time."
  ;; bound by caller
  (setf jss-current-browser-instance jss-browser)
  (add-hook 'kill-buffer-hook 'jss-browser-kill-buffer nil t)
  (jss-browser-mode-refresh))

(defun jss-browser-mode* (browser)
  "Change to jss-browser-mode using the browser object `browser`.

This is a simple wrapper around jss-browser-mode since that
function is defined by define-derived-mode to not take any
arguments."
  (let ((jss-browser browser))
    (jss-browser-mode)))

(define-key jss-browser-mode-map (kbd "g") 'jss-browser-mode-refresh)

(defun jss-current-browser ()
  jss-current-browser-instance)

(defun jss-browser-delete-and-insert-header ()
  "Clear the contents of the current buffer and insert the
browser description line (usually backend type, host and port)."
  (widen)
  (delete-region (point-min) (point-max))
  (insert (jss-browser-description (jss-current-browser)) "\n\n"))

(defun jss-browser-refresh (browser)
  "Refresh the contents of the buffer describing `buffer`.

See `jss-browser-mode-refresh` for the actual implementation."
  (with-current-buffer (jss-browser-buffer browser)
    (jss-browser-mode-refresh)))

(defun jss-browser-mode-refresh ()
  "Delete the contents of the current buffer, get a new list of
available tabs, and reinsert buttons to consoles (if
applicable)."
  (interactive)
  
  (lexical-let* ((browser (jss-current-browser))
                 (jss-browser-buffer (current-buffer))
                 (tab-handler (lambda (browser)
                                (with-current-buffer jss-browser-buffer
                                  (let ((inhibit-read-only t))
                                    (jss-browser-delete-and-insert-header)
                                    (if (jss-browser-tabs browser)
                                        (progn
                                          (dolist (tab (jss-browser-tabs browser))
                                            (insert (format "%s.%s - %s\n" (jss-tab-id tab) (jss-tab-title tab) (jss-tab-url tab)))
                                            (when (jss-tab-available-p tab)
                                              (insert "  ")
                                              (jss-insert-button (if (jss-tab-console tab)
                                                                     "[ goto console ]"
                                                                   "[ open console ]")
                                                                 'jss-tab-goto-console
                                                                 :other-properties (list 'jss-tab-id (jss-tab-id tab)))
                                              (insert "\n")))
                                          (jss-browser-insert-help-topics)
                                          (goto-char (point-min))
                                          (jss-next-button))
                                      (insert "No debuggable tabs found.")
                                      (jss-browser-insert-help-topics)
                                      (goto-char (point-min)))))))
                 (tab-error-handler (lambda (error)
                                      (with-current-buffer jss-browser-buffer
                                        (let ((inhibit-read-only t))
                                          (jss-browser-delete-and-insert-header)
                                          (insert "\nConnection error:\n\n" (prin1-to-string error))
                                          (jss-browser-insert-help-topics)
                                          (goto-char (point-min))
                                          (signal (first error) (rest error)))))))
    (setf buffer-read-only t
          (jss-browser-buffer browser) (current-buffer))
      
    (let ((inhibit-read-only t))
      (jss-browser-delete-and-insert-header)
      (insert (format "[ Connecting to %s:%s... ]"
                      (jss-browser-host browser)
                      (jss-browser-port browser))))
    
    (jss-deferred-then
     (if (jss-browser-connected-p browser)
         (make-jss-completed-deferred browser)
       (jss-browser-connect browser))
     (lambda (conn)
       (jss-deferred-then
        (jss-browser-get-tabs browser)
        tab-handler
        tab-error-handler)))))

(defun jss-browser-insert-help-topics ()
  "Insert a list of links to the documentation for jss's main
modes."
  (insert "\n\n")
  (insert "JSS Help:\n")
  (dolist (help-topic '(("browser mode" jss-browser-mode)
                        ("console mode" jss-console-mode)
                        ("debugger mode" jss-debugger-mode)
                        ("remote-objects" jss-insert-remote-value)
                        ("the prompt" jss-insert-prompt)))
    (insert "  ")
           
    (jss-insert-button (first help-topic) `(lambda ()
                                             (interactive)
                                             (describe-function ',(second help-topic))))
    (insert "\n")))

(defclass jss-browser-connection-details ()
  ((label         :accessor jss-browser-spec-label :initarg :label)
   (browser-class :accessor jss-browser-spec-class :initarg :class)
   (default-host  :accessor jss-browser-spec-default-host :initarg :default-host :initform "127.0.0.1")
   (default-port  :accessor jss-browser-spec-default-port :initarg :default-port))
  (:documentation "Represents the different kinds of backend we
can connect to, what they're called, what class of jss-browser
objects they need, and the default connection parameters."))

(defcustom jss-browsers
  (list (make-instance 'jss-browser-connection-details
                      :label "webkit"
                      :default-port "9222"
                      :class 'jss-webkit-browser)
        (make-instance 'jss-browser-connection-details
                       :label "firefox"
                       :default-port "6000"
                       :class 'jss-firefox-browser))
  "List of known browsers"
  :group 'jss)

(defcustom jss-browser-default-host "127.0.0.1"
  "Default port for the browser's debugging api."
  :group 'jss)

(defvar jss-connect/select-browser-history '())

;;;###autoload
(defun* jss-connect (browser-label &key host port)
  "Query the user for a browser type, a host, and a port, and
jump to its browser buffer."
  (interactive (list (let ((completion-ignore-case t))
                       (completing-read "Browser: "
                                        (mapcar (lambda (browser-spec)
                                                  (cons (jss-browser-spec-label browser-spec) browser-spec))
                                                jss-browsers)
                                        nil
                                        t
                                        (first jss-connect/select-browser-history)
                                        'jss-connect/select-browser-history))))
  (let ((browser-spec (cl-find browser-label jss-browsers :key 'jss-browser-spec-label :test 'string=)))
    (assert browser-spec nil "Unable to find browser named %s" browser-spec)
    (let ((host (or host (read-from-minibuffer "Host: " (jss-browser-spec-default-host browser-spec))))
          (port (or port (read-from-minibuffer "Port: " (jss-browser-spec-default-port browser-spec)))))
      (with-current-buffer (get-buffer-create (format "*JSS Browser @%s:%s*" host port))
        (switch-to-buffer (current-buffer))
        (jss-browser-mode* (make-instance (jss-browser-spec-class browser-spec)
                                          :host host
                                          :port port))))))

(defun jss-browser-kill-buffer ()
  (interactive)
  (jss-browser-cleanup (jss-current-browser)))

(provide 'jss-browser)
