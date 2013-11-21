;;; jss-browser-api.el -- definition and support code for jss's interface to a specific browser
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

(eval-when-compile
  (require 'cl))
(require 'eieio)
(require 'jss-utils)
(require 'jss-deferred)
(require 'jss-super-mode)

(defclass jss-generic-browser ()
  ((host :initarg :host :accessor jss-browser-host)
   (port :initarg :port :accessor jss-browser-port)
   (tabs :initform '())
   (buffer :accessor jss-browser-buffer))
  (:documentation "A specific browswer running somewhere, that we
can communicate with, and which, hopefully, has tabs we can
attach a console to."))

(defgeneric jss-browser-connected-p (browser)
  "Returns T if we are currently connected to `browser`.")

(defgeneric jss-browser-connect (browser)
  "Connect to `browser`. Returns a deferred that will complete
when the connection has been established.")

(defgeneric jss-browser-disconnect (browser)
  "Disconnect from `browser`. Returns a deferred that will
complete when the connection has been broken.")

(defgeneric jss-browser-get-tabs (browser)
  "Gets, and stores for later retrevial via `jss-browser-tabs`,
the list of currently open tabs in in `brower`.

Since we store references to tab objects in various buffers it is
important that this method modify, but not recreate, any already
existing tab objects.")

(defgeneric jss-browser-description (browser)
  "Gets a human readable description of this browser. This string
is used, as is, in the *jss-browser* buffer to tell the user what
browser they're connected to.")

(defgeneric jss-browser-tabs (browser)
  "Returns a list of jss-generic-tab objects, one for each tab
that was available when `jss-browser-get-tabs` was called.")

(defgeneric jss-browser-find-tab (browser tab-id)
  "Given `tab-id`, an arbitrary opaque object returned by a
previous call to jss-tab-id, returns the corresponding tab
object.

We will sometimes need to store tab IDs and not tab objects
directly, this method server to map back from the ID to original
object.

No assumptions are made about the id objects themselves, except
that they are globally unique.")

(defgeneric jss-browser-cleanup (browser)
  "Releases any state held by `browser`.")

(defmethod jss-browser-cleanup ((browser jss-generic-browser))
  t)

(defclass jss-generic-tab ()
  ((browser :initarg :browser :accessor jss-tab-browser)
   (console :initform nil :accessor jss-tab-console)
   (ios :initform (make-hash-table :test 'equal)
        :accessor jss-tab-ios)
   (scripts :initform (make-hash-table :test 'equal)
            :accessor jss-tab-scripts))
  (:documentation "A tab in a browser."))

(make-variable-buffer-local
 (defvar jss-current-tab-instance nil
   "The current tab that should be used if we need to interact
with the browser."))

(defgeneric jss-tab-available-p (tab)
  "Returns T if `tab' can be debugged, which means we'll try to
attach a console to it, returns NIL otherwise (which usually, but
not always, means there's already an in-browser debugger attached
to `tab`.")

(defgeneric jss-tab-id (tab)
  "Returns a globally unique identifier for the object `tab`. The
returned value will be compared to other tabs with equal and
never be used within the same emacs session.")

(defgeneric jss-tab-title (tab)
  "Returns the current title (a string) of the tab. Used to
inform the user about the state of the page being viewed.

As much as possible this should stay synchronized with the
current state of the browser, but jss itself doesn't depend an
the accuracy of this method (though the user would appreciate it
if it was up to date).")

(defgeneric jss-tab-url (tab)
  "Returns the current url of the tab. This is used both to
inform the user what url the tab is currently viewing and by
jss's debugger's auto-resume-points.

As much as possible this should reflect the current state of the
browser, nothing will break if this returns a stale url, but some
functionality will not work as expected.")

(defgeneric jss-tab-connected-p (tab)
  "Returns T if jss has an open connection to `tab`. This
usually, but not always, means there's a console buffer for
`tab` (though sometimes there will be a console buffer but
jss-tab-connected-p will return nil)")

(defgeneric jss-tab-connect (tab)
  "Creates a connection to `tab`, returns a deferred object which
will complete when the connection has been established.")

(defgeneric jss-tab-reload (tab)
  "Tell the browser to reload the contents of `tab`.")

(defgeneric jss-tab-make-console (tab &rest initargs)
  "Creates a console instance for `tab`, passing make-instance
`initargs`. This method is basically a factory for browsers
specific console implementations.")

(defgeneric jss-tab-disable-network-monitor (tab)
  "Disables logging and tracking of network IO for `tab`.")

(defgeneric jss-tab-enable-network-monitor (tab)
  "Enables logging and tracking of network IO.")

(defgeneric jss-tab-object-properties (tab object-id)
  "Returns an alist of proerty names and values for the remote
with id `object-id` in the current context of `tab`.

The keys of the plist are strings (simple elisp strings) and the
values are remote object instances (both primitive and non).")

(defgeneric jss-tab-ensure-console (tab)
  "If `tab` doesn't already have a console object, then create it (and initialize its buffer).

Either way, returns `tabs`'s console.")

(defclass jss-generic-script ()
  ((tab :initarg :tab :accessor jss-script-tab)
   (buffer :initform nil :accessor jss-script-buffer)
   (body :initform nil :accessor jss-script-body))
  (:documentation "Represents a single piece of javascript source
code where errors can occur.

A script object usually, but not neccessarily, corrseponds to a
url or a <script> tag (the main exception being code internal to
the browser itself."))

(defgeneric jss-script-id (script)
  "Returns a globally unique identifier for the script
`script`. This is an object we store and later use to retrieve
`script` and also an indetifier we can present to the user to
distinguish scripts that have no other natural identifier.

It can happen that a give url or script tag is changed and
reloaded, in that case we may have multiple script objects which
map back to the same url or file, but which are in fact
different (different source text, different id).")

(defgeneric jss-script-url (script)
  "Return a url, or as close to one as possible, describing where
the text for `script` came from. The returned url should help the
user understand, as much as possible, where to find the source of
`script`.")

(defgeneric jss-script-get-body (script)
  "Returns a deferred which, when it completes, will pass the
source code, as an elisp string, of the script `script`.")

(defgeneric jss-evaluate (context text)
 "run the javascript code `text` and return a deferred which,
after the code has run, will complete with the returned value (a
remote value instance.)

The context is the environment, either a tab or a frame, within
which to run `text`.")

(defgeneric jss-tab-get-script (tab script-id)
  "Gets the script object with id `script-id` from `tab`.")

(defmethod jss-tab-get-script ((tab jss-generic-tab) script-id)
  (gethash script-id (jss-tab-scripts tab)))

(defgeneric jss-tab-set-script (tab script-id script))

(defmethod jss-tab-set-script ((tab jss-generic-tab) script-id script)
  (setf (jss-script-tab script) tab
        (gethash script-id (jss-tab-scripts tab)) script))

(defsetf jss-tab-get-script jss-tab-set-script)

(defclass jss-generic-console ()
  ((tab :initarg :tab
        :initform nil
        :accessor jss-console-tab))
  (:documentation "Represents a console attached to a tab.

A console is an object which servers two pruposes:

1. It can log events that have occured in a specific tab (network
IO, DOM changes, exceptions, etc.

2. It con excute code, as javascript source strings, within the
state of a specific web page."))

(make-variable-buffer-local
 (defvar jss-current-console-instance nil))

(defun jss-current-console ()
  jss-current-console-instance)

(defun jss-current-tab ()
  (or jss-current-tab-instance
      (if (jss-current-console)
          (jss-console-tab (jss-current-console))
        nil)))

(defgeneric jss-console-mode* (console)
  "Initialize the current buffer with `console`.")

(defmethod jss-tab-ensure-console ((tab jss-generic-tab))
  (or (jss-tab-console tab)
      (let ((console (jss-tab-make-console tab :tab tab)))
        (setf (jss-tab-console tab) console)
        (with-current-buffer (jss-console-buffer console)
          (jss-console-mode* console))
        console)))

(defgeneric jss-console-clear (console)
  "Clears, removes from the buffer and releases stored memory,
all the objects (log messages, network io and evaluation
results) currently attached to `console`.

This causes references to the IO, debugger and script items
attached to `console` to be released within emacs and also on the
browser (if applicable)")

(defgeneric jss-console-buffer (console)
  "Returns the current buffer where `console`'s events are logged
and where its prompt lives.")

(defmethod jss-console-buffer ((console jss-generic-console))
  (get-buffer-create
   (format "*JSS Console/%s*" (jss-tab-id (jss-console-tab console)))))

(defgeneric jss-console-disconnect (console)
  "Close the connection between jss and the console `console`.

Returns a deferred which will complete when the connection has
been closed.")

(defgeneric jss-console-insert-io (console io)
  "Insert into `console`'s log a link to the network io `io`")

(defgeneric jss-console-update-io (console io)
  "Find the line in the current buffer (a console buffer)
corresponding to `io` and replace it with a line describing the
current state of `io`.")

(defgeneric jss-console-insert-message-objects (console level objects)
  "Given a list of remote objects, such as those passed to jss by
the browser when code calls window.console.log, insert the
corresponding remote-value objects into the current buffer using
the face and label corresponding to `level`.")

(defgeneric jss-console-debug-message (console format-control &rest format-args))
(defgeneric jss-console-log-message   (console format-control &rest format-args))
(defgeneric jss-console-warn-message  (console format-control &rest format-args))
(defgeneric jss-console-error-message (console format-control &rest format-args))

(defclass jss-generic-io ()
  ((tab :accessor jss-io-tab :initform nil)
   (start-time :accessor jss-io-start :initarg :start-time)
   (lifecycle :initform '() :accessor jss-io-lifecycle :initarg :lifecycle
              :documentation "A list of (EVENT WHEN) describing,
              if possible genericly, the events that have occured
              for this IO. Must be kept in chronological
              order (oldest first).")
   (buffer :initform nil :accessor jss-io-buffer))
  (:documentation "An object that describes a single
request/response between the browser and a server."))

(defmacro* with-existing-io ((tab io-id) &rest body)
  `(let ((io (jss-tab-get-io ,tab ,io-id)))
     (if io
         (progn ,@body)
       (jss-log-event (list :io :unknown-io-io ,io-id)))))
(put 'with-existing-io 'lisp-indent-function 1)

(defgeneric jss-io-id (io)
  "Returns a globally unique id identifying `io`.")

(defgeneric jss-io-request-method (io)
  "Returns the HTTP request method (a string) used by`io`.")

(defgeneric jss-io-request-url (io)
  "The url requested by `io`.")

(defgeneric jss-io-request-data (io)
  "The POST data sent with `io`.")

(defgeneric jss-io-request-headers (io)
  "Returns the HTTP request headers sent by `io` as an alist
whose keys and values are strings.")

(defgeneric jss-io-raw-request-headers (io)
  "Returns the HTTP request headers sent by `io` as a
string (really a sequence of bytes)")

(defgeneric jss-io-response-headers (io)
  "Returns the HTTP response headers sent by `io` as an alist
whose keys and values are strings.")

(defgeneric jss-io-raw-response-headers (io)
  "Returns the HTTP response headers sent by `io` as a string (a
sequence of bytes)")

(defgeneric jss-io-response-status (io)
  "Either an integer specifying the status code or nil specifying
that we're still waiting for the response.")

(defgeneric jss-io-response-content-type (io)
  "The normalized content type returned by `io`")

(defgeneric jss-io-response-content-length (io)
  "the length, in bytes (not characters) of data recevied by
`io`")

(defgeneric jss-io-response-data (io)
  "The data, as a string (without encoding).")

(defgeneric jss-tab-get-io (tab io-id)
  "returns the IO object in `tab` whose id is `io-id` (which is a
value as returned by `jss-io-id`")

(defmethod jss-tab-get-io ((tab jss-generic-tab) io-id)
  (gethash io-id (jss-tab-ios tab)))

(defmethod jss-tab-set-io ((tab jss-generic-tab) io-id io-object)
  (if (null (jss-io-tab io-object))
      (setf (jss-io-tab io-object) tab
            (gethash io-id (jss-tab-ios tab)) io-object)
    (unless (eq tab (jss-io-tab io-object))
      (error "Attempt to add IO %s to tab %s, but it's already registered with %s."
             io-object tab (jss-io-tab io-object)))))

(defsetf jss-tab-get-io jss-tab-set-io)

(defclass jss-generic-debugger ()
  ((buffer :accessor jss-debugger-buffer)
   (tab    :accessor jss-debugger-tab :initarg :tab))
  (:documentation "Represents some exception, and its state, on
the browser. "))

(defgeneric jss-debugger-mode* (debugger)
  "Initializes the buffer for the debugger `debugger`.")

(defgeneric jss-debugger-stack-frames (debugger)
  "Returns a list, in order from bottom (closest to the
exception) to top (farthest from the error, usually an event
handler in the browser) of jss-frame objects.")

(defgeneric jss-debugger-exception (debugger)
  "Returns the exception, as a remote-value, describing what went
wrong with `debugger`")

(defgeneric jss-debugger-resume    (debugger)
  "resume, continue or play depending on the terminology, from
exception.")

(defgeneric jss-debugger-step-into (debugger)
  "Step into the next function call. Resumes the current debugger
and triggers a new one at the next function call in the current
stack.")

(defgeneric jss-debugger-step-over (debugger)
    "Step over the next function call. Resumes the current
debugger and triggers a new one before the next function call.")

(defgeneric jss-debugger-step-out  (debugger)
    "Step into the next function call. Resumes the current
debugger and triggers a new one in the next function called by
the function currently paused.")

(defgeneric jss-tab-open-debugger (tab debugger)
  "Creates, and switches to, a new debugger buffer given the tab
instance `tab` and the debugger obejct `debugged`.")

;;; nb: do NOT name the debugger parameter debugger. it messes with emacs in strange ways.
(defmethod jss-tab-open-debugger ((tab jss-generic-tab) dbg)
  (setf (jss-debugger-buffer dbg) (get-buffer-create (generate-new-buffer-name "*JSS Debugger*"))
        (jss-debugger-tab dbg) tab)
  (with-current-buffer (jss-debugger-buffer dbg)
    (jss-debugger-mode* dbg)
    (when (buffer-live-p (jss-debugger-buffer dbg)) 
      (switch-to-buffer (jss-debugger-buffer dbg)))))

(defgeneric jss-debugger-cleanup (debugger)
  "Releases all objects, in emacs and the remote browser, tied to
`debugger`")

(defmethod jss-debugger-cleanup ((debugger jss-generic-debugger))
  t)

(defgeneric jss-tab-set-debugger-sensitivity (tab sensitivity)
  "Set the break level of `tab`'s debugger to `sensitivity` (:all, :uncaught or :never)")

(defgeneric jss-debugger-insert-message (debugger)
  "Insert, at the current point, text describing why `debugger` has been opened (the ecxeption, the source location, etc.).")

(defclass jss-generic-stack-frame ()
  ((debugger :initarg :debugger :accessor jss-frame-debugger))
  (:documentation "Represents one stack frame, a function/method
call in some environment, that lead to a particulare execption
being signaled."))

(defgeneric jss-frame-function-name (frame)
  "The name of the function enclosing this stack frame.")

(defgeneric jss-frame-source-hint (frame)
  "A human readable string decribing, to the user, where this
frame \"is\". This string will be displayed but is not used
internally.")

(defgeneric jss-frame-get-source-location (frame)
  "Return a deferred which completes with a list of (script
line-number column-number) which jss can use to open a buffer and
position point of the exact spot where this frame's exception
started.")

(defgeneric jss-frame-restart (frame)
  "Restart execution from this frame, taking into effect any
changes to the global or local state that have been made. If this
is not possible signal an error.")

(eval-when (compile load eval)
  (defvar jss-remote-value-counter 0))

(defclass jss-generic-remote-value ()
  ((id :accessor jss-remote-value-id
       :initform (incf jss-remote-value-counter)
       :initarg :id))
  (:documentation "Represents some value in the browser."))

(defgeneric jss-remote-value-description (remote-object)
  "Returns a human readable string describing, briefly and not
necessarily precisely, `remote-object`.")

(defgeneric jss-remote-value-insert-description (remote-object)
  "Insert into the current buffer `remote-sbject`'s
description. Should, but need not, call
jss-remote-value-description.")

(defmethod jss-remote-value-insert-description ((o jss-generic-remote-value))
  (insert (jss-limit-string-length (jss-remote-value-description o) 60)))

(defclass jss-generic-remote-primitive (jss-generic-remote-value)
  ((value :initarg :value :accessor jss-remote-primitive-value))
  (:documentation "A primitive, non divisible, remote object."))

(defclass jss-generic-remote-boolean (jss-generic-remote-primitive) ())

(defclass jss-generic-remote-true (jss-generic-remote-boolean) ())
(defmethod jss-remote-value-description ((object jss-generic-remote-true)) "true")

(defclass jss-generic-remote-false (jss-generic-remote-boolean) ())
(defmethod jss-remote-value-description ((object jss-generic-remote-false)) "false")

(defclass jss-generic-remote-string (jss-generic-remote-primitive) ())
(defmethod jss-remote-value-description ((string jss-generic-remote-string))
  (prin1-to-string (jss-remote-primitive-value string)))

(defmethod jss-remote-value-insert-description ((o jss-generic-remote-string))
  (insert (jss-remote-value-description o)))

(defclass jss-generic-remote-number (jss-generic-remote-primitive) ())
(defmethod jss-remote-value-description ((number jss-generic-remote-number))
  (let ((value (jss-remote-primitive-value number)))
    (if (integerp value)
        (format "%d" value)
      (format "%g" value))))

(defclass jss-generic-remote-NaN (jss-generic-remote-primitive) ())
(defmethod jss-remote-value-description ((object jss-generic-remote-NaN)) "NaN")

(defclass jss-generic-remote-plus-infinity (jss-generic-remote-primitive) ())
(defmethod jss-remote-value-description ((object jss-generic-remote-plus-infinity)) "+Inf")

(defclass jss-generic-remote-minus-infinity (jss-generic-remote-primitive) ())
(defmethod jss-remote-value-description ((object jss-generic-remote-minus-infinity)) "-Inf")

(defclass jss-generic-remote-undefined (jss-generic-remote-primitive) ())
(defmethod jss-remote-value-description ((object jss-generic-remote-undefined)) "undefined")

(defclass jss-generic-remote-no-value (jss-generic-remote-primitive) ())
(defmethod jss-remote-value-description ((object jss-generic-remote-no-value)) "no value.")

(defclass jss-generic-remote-null (jss-generic-remote-primitive) ())
(defmethod jss-remote-value-description ((object jss-generic-remote-null)) "null")

(defclass jss-generic-remote-non-primitive (jss-generic-remote-value) ()
  (:documentation "A remote value that has properites."))

(defclass jss-generic-remote-object (jss-generic-remote-non-primitive) ())

(defgeneric jss-remote-object-class-name (object))

(defgeneric jss-remote-object-label (object))

(defmethod jss-remote-value-description ((object jss-generic-remote-object))
  (let ((class-name (jss-remote-object-class-name object))
        (label  (jss-remote-object-label object)))
    (if (string= label class-name)
        (format "[%s]" label)
      (format "[%s %s]" class-name label))))

(defgeneric jss-remote-object-get-properties (object tab))

(defclass jss-generic-remote-function (jss-generic-remote-non-primitive) ())
(defgeneric jss-remote-function-get-source-location (function))

(defclass jss-generic-remote-array (jss-generic-remote-object) ())

(provide 'jss-browser-api)
