;;; jss-console.el -- a jss console and logger for a given browser tab
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

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'jss-prompt)
(require 'jss-browser-api)
(require 'jss-browser)
(require 'jss-remote-value)
(require 'jss-io)

(defvar jss-console nil
  "Dummy variable used to pass a console to the function jss-console-mode")

(define-derived-mode jss-console-mode jss-super-mode "JSS Console"
  "A jss console buffer serves two purposes:

1. It collects asynchronous various events (console logs, network
traffice, errors, etc.) from the browser

2. It displays a console where we can run code inside the current
state of the browser. (a jss-prompt).

Every event on the server generates one line (more if it's a
multiline console message) in the buffer:

  // <level> // <message>

Where <message> is a string of text or buttons and <level> is one
of:

  * \"note\" - used for logging/debugging/status messages from jss itself
  * \"log\" - used for log level console messages and network traffice
  * \"warning\" - used for warning level console messages
  * \"ERROR\" - used for error level console messages and uncaught exceptions

For IO events, request sent, response received, etc., we display
the url (with query parameters) as a button which will open up
the corresponding *JSS IO* buffer.

The currently active prompt, which is always at the bottom of the
buffer, is a highlighted line which starts with \"> \". Unlike
the prompts in the debugger buffers all the normal console
keybindings are available. One consequence of this is that the
<tab> key jumps to the next button in the buffer and does not
indent the current line of code.

jss-console-mode also binds jss-expand-nearest-remote-value
globally (and not just in prompt fields), which makes it easy to
expand objects while moving around the buffer."
  (add-hook 'kill-buffer-hook 'jss-console-kill nil t)
  ;; assume caller binds jss-console
  (setf jss-current-console-instance jss-console
        jss-current-tab-instance (jss-console-tab jss-console))

  (lexical-let ((console (jss-current-console)))
    (goto-char (jss-prompt-start-of-input
                (jss-insert-prompt (lambda (text) (jss-evaluate console text))))))
  
  (jss-console-ensure-connection)
  t)

(easy-menu-define jss-console-menu jss-console-mode-map
  "Menu for JSS Console buffers."
  '("JSS Console"
    [ "Reload Tab" jss-console-reload-page t ]
    [ "Clear Buffer"  jss-console-clear-buffer t ]
    [ "Disable Timing Data Log"  jss-console-toggle-timing-data
      :visible jss-console-log-timing-data ]
    [ "Enable Timing Data Log"  jss-console-toggle-timing-data
      :visible (not jss-console-log-timing-data) ]
    ))

(define-key jss-console-mode-map (kbd "C-c C-o") 'jss-console-clear-buffer)
(define-key jss-console-mode-map (kbd "C-c C-r") 'jss-console-reload-page)
(define-key jss-console-mode-map (kbd "C-c C-n") 'jss-toggle-network-monitor)
(define-key jss-console-mode-map (kbd "C-c C-i") 'jss-expand-nearest-remote-value)

(defface jss-console-debug-message '((t :inherit font-lock-comment-face))
  "Face for JSS debug messages"
  :group 'jss)
(defface jss-console-log-message   '((t :inherit font-lock-doc-face))
  "Face for JSS log messages"
  :group 'jss)
(defface jss-console-warn-message  '((t :inherit font-lock-other-emphasized-face))
  "Face for JSS warning messages"
  :group 'jss)
(defface jss-console-error-message '((t :inherit font-lock-warning-face))
  "Face for JSS error messages"
  :group 'jss)

(defmethod jss-console-mode* ((console jss-generic-console))
  (let ((jss-console console))
    (jss-console-mode)))

(defun jss-current-tab () jss-current-tab-instance)

(defun jss-tab-goto-console (&optional tab)
  "Switch to the console buffer for `tab`."
  (interactive (list (let ((tab-id (get-text-property (point) 'jss-tab-id)))
                       (unless tab-id
                         (error "No tab at point."))
                       (let ((tab (jss-browser-find-tab (jss-current-browser) tab-id)))
                         (unless tab
                           (error "Unable to find tab with id %s in current browser (%s)" tab-id (jss-current-browser)))
                         tab))))
  (switch-to-buffer
   (jss-console-buffer
    (jss-tab-ensure-console tab))))

(defun jss-console-ensure-connection ()
  "Return a deferred which will complete when the connection to
the current tab's console has been established (this may happen
immediately if the connection already exsits)."
  (interactive)
  (unless (jss-current-console)
    (error "No current console object. Can't open console here."))
  (if (jss-tab-connected-p (jss-current-tab))
      (make-jss-completed-deferred (jss-current-tab))
    (unless (jss-tab-connected-p (jss-current-tab))
      (jss-console-debug-message (jss-current-console) "Connecting...")
      (lexical-let ((buf (current-buffer)))
        (jss-deferred-then
         (jss-tab-connect (jss-current-tab))
         (lambda (tab)
           (with-current-buffer buf
             (jss-console-debug-message (jss-current-console) "Connected."))
           tab))))))

(defun jss-console-kill ()
  "Close the connection to the current console/tab and perfrom
any necessary cleanup."
  (interactive)
  (jss-when-bind (console (jss-current-console))
    (jss-when-bind (tab (jss-console-tab console))
      (jss-when-bind (browser (jss-tab-browser tab))
        (lexical-let ((browser browser))
          (jss-deferred-then
           (jss-console-disconnect console)
           (lambda (console)
             (setf (jss-tab-console (jss-console-tab console)) nil
                   (jss-console-tab console) nil)
             (jss-browser-refresh browser))))))))

(defmethod jss-console-debug-message ((console jss-generic-console) &rest format-message-args)
  "Append a message, of priority \"debug\", to `console`."
  (apply 'jss-console-format-message console 'debug format-message-args))

(defmethod jss-console-log-message ((console jss-generic-console) &rest format-message-args)
  "Append a message, of priority \"log\", to `console`."
  (apply 'jss-console-format-message console 'log format-message-args))

(defmethod jss-console-warn-message ((console jss-generic-console) &rest format-message-args)
  "Append a message, of priority \"warn\", to `console`."
  (apply 'jss-console-format-message console 'warn format-message-args))

(defmethod jss-console-error-message ((console jss-generic-console) &rest format-message-args)
  "Append a message, of priority \"error\", to `console`."
  (apply 'jss-console-format-message console 'error format-message-args))

(defun jss-console-level-face (level)
  "Returns the emacs face to use for console message of priority `level`"
  (ecase level
    (debug 'jss-console-debug-message)
    (log   'jss-console-log-message)
    (warn  'jss-console-warn-message)
    (error 'jss-console-error-message)))

(defun jss-console-level-label (level)
  (concat "// "
          (ecase level
            (debug "note")
            (log   "log")
            (warn  "warning")
            (error "ERROR"))
          " // "))

(defmethod jss-console-format-message ((console jss-generic-console) level format-string &rest format-args-and-properties)
  "Insert a message, of the face correspoding to `level`, in
`console`'s buffer. `format-string` and
`format-args-and-properties` are passed to `format` to compute
the text to be inserted.

if `format-args-and-properties` contains a :properties <values>
element then the text properites <values> will be added to the
inserted text."
  (let ((properties nil)
        (format-args nil))
    (if (member :properties format-args-and-properties)
        (loop
         for head on format-args-and-properties
         if (eql (car head) :properties)
         do (setf properties (second head)
                  head (cdr head))
         else do (push (car head) format-args)
         finally (setf format-args (reverse format-args)))
      (setf format-args format-args-and-properties))
    (unless (cl-getf 'face properties)
      (setf properties (list* 'face (jss-console-level-face level) properties)))
    (with-current-buffer (jss-console-buffer console)
      (save-excursion
        (jss-before-last-prompt)
        (let ((start (point))
              (inhibit-read-only t))
          (insert (jss-console-level-label level)) 
          (insert (apply 'format format-string format-args) "\n")
          (unless (cl-getf properties 'read-only)
            (setf properties (list* 'read-only t properties)))
          (add-text-properties start (point) properties))))))

(defmethod jss-console-insert-message-objects ((console jss-generic-console) level objects)
  (save-excursion
    (with-current-buffer (jss-console-buffer console)
      (let ((inhibit-read-only t))
        (jss-before-last-prompt)
        (jss-wrap-with-text-properties (list 'face (jss-console-level-face level)
                                             'read-only t)
          (insert (jss-console-level-label level))
          (dolist (o objects)
            (jss-insert-remote-value o)))
        (unless (bolp)
          (insert "\n"))))))

(defcustom jss-console-log-timing-data t
  "When non-NIL, the default, io log events in the console will also log timing information."
  :type 'boolean
  :group 'jss)

(defun jss-console-toggle-timing-data ()
  (interactive)
  (setf jss-console-log-timing-data (not jss-console-log-timing-data)))

(defun jss-lifecycle-event-to-string (event-code)
  (ecase event-code
    (:sent "request sent")
    (:loading-finished "done")
    (:data-received "data")
    (:loading-failed "Failed")
    (:served-from-cache "cached")
    (:served-from-memory-cache "mem cached")
    (:response-received "response")))

(defmethod jss-console-insert-io-line ((console jss-generic-console) io)
  "Insert a line into the current buffer (which must be a console
buffer) describing the current state of `io`."
  (with-current-buffer (jss-console-buffer console)
    (save-excursion
      (jss-before-last-prompt)
      (jss-wrap-with-text-properties (list 'jss-io-id (jss-io-id io)
                                           'face (jss-console-level-face 'log)
                                           'read-only t)
        (let ((inhibit-read-only t))
          (insert (jss-console-level-label 'log))
          (jss-insert-button (jss-limit-string-length (concat (jss-io-request-method io)
                                                              " "
                                                              (jss-io-request-url io))
                                                      80)
                             'jss-console-switch-to-io-inspector)
          (insert " " (jss-lifecycle-event-to-string (first (car (last (jss-io-lifecycle io))))) "\n")

          (when jss-console-log-timing-data
            (jss-when-bind (lifecycle (jss-io-lifecycle io))
              (let* ((start-time (second (first lifecycle)))
                     (start-time.ms (* 1000 (float-time start-time)))
                     (last-time.ms 0))
                (insert "          @"
                        (format-time-string "%T.%6N" start-time)
                        " "
                        (jss-lifecycle-event-to-string (first (first lifecycle)))                        
                        ";")
                (dolist (l (butlast (rest (jss-io-lifecycle io))))
                  (let* ((what (first l))
                         (when.ms (* (float-time (second l)) 1000)))
                    (setf last-time.ms when.ms)
                    (insert " "
                            (jss-lifecycle-event-to-string what)
                            " after "
                            (format "%0.3fms;" (- when.ms start-time.ms)))))
                (let* ((last (car (last (jss-io-lifecycle io))))
                       (last-what (first last))
                       (last-when (second last))
                       (last-when.ms (* 1000 (float-time last-when))))
                  (insert "\n          @"
                          (format-time-string "%T.%6N" last-when)
                          " "
                          (jss-lifecycle-event-to-string last-what)
                          " (Elapsed " (format "%0.3fms" (- last-when.ms start-time.ms)) ")\n"))))))))))

(defmethod jss-console-insert-io ((console jss-generic-console) io)
  "Insert a line in the buffer describing IO (and this should be the first time we've gotten an event related to IO."
  (jss-console-insert-io-line console io))

(defmethod jss-console-update-io ((console jss-generic-console) io)

  (with-current-buffer (jss-console-buffer console)
    (jss-delete-property-block 'jss-io-id (jss-io-id io) :error nil)
    (jss-console-insert-io-line console io)))

(defun jss-console-clear-buffer ()
  "Delete the contents of current console buffer and release any
browser side objects."
  (interactive)
  (let ((console (jss-current-console)))
    (jss-console-clear console)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (eql 'jss-io-mode major-mode)
                   (jss-current-io)
                   (eql (jss-console-tab console) (jss-io-tab (jss-current-io))))
          (kill-buffer buf))))
    
    (jss-before-last-prompt)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point)))
    (jss-prompt-next-input)))

(defun jss-console-reload-page ()
  "Tell the browser to reload the current tab."
  (interactive)
  (lexical-let ((tab (jss-current-tab)))
    (jss-deferred-add-backs
      (jss-tab-reload tab)
      (lambda (response)
        (jss-console-log-message (jss-tab-console tab) "Triggered page reload.")))))

(defvar jss-set-debugger-sensitivity/levels
  '(("all exceptions" . :all)
    ("uncaught exceptions" . :uncaught)
    ("never" . :never)))

(defun jss-set-debugger-sensitivity (level)
  "Set the debugger for the current tab to stop on nothing, all exceptions or only uncaught exceptions."
  (interactive (list (jss-completing-read "Break on: " (mapcar 'car jss-set-debugger-sensitivity/levels)
                                          :require-match t)))
  (jss-tab-set-debugger-sensitivity (jss-current-tab)
                                    (cdr (assoc level jss-set-debugger-sensitivity/levels))))

(defun jss-toggle-network-monitor (prefix)
  (interactive "P")
  (if (jss-current-tab)
      (if prefix
          (jss-tab-disable-network-monitor (jss-current-tab))
        (jss-tab-enable-network-monitor (jss-current-tab)))
    (error "No current tab.")))

(provide 'jss-console)
