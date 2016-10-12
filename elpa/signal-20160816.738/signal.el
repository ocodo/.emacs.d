;;; signal.el --- Advanced hook  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2015-2016 Mola-T
;; Author: Mola-T <Mola@molamola.xyz>
;; URL: https://github.com/mola-T/signal
;; Package-Version: 20160816.738
;; Version: 1.0
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; Keywords: internal, lisp, processes, tools
;;
;;; License:
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; Signal is a library offering enriched hook-like features.
;; With signal, you are able to handle condition-based function
;; calls with arguments.
;; You can also make delayed condition-based function calls.
;;
;; You can see full introduction on
;; https://github.com/mola-T/signal for introduction
;;
;;; code:

(require 'cl-lib)

(defmacro defsignal (name &optional docstring)
  "Defining a signal.
Connect a signal to a worker function by `signal-connect'.
Use `signal-emit' to emit the signal and the worker function will be called.

Example:
\(defsignal my-signal
\"This a docmentation\"\)"
  (declare (doc-string 1) (indent 1))
  `(defvar ,name nil
     ,(concat "It is signal.
`signal-connect' or `signal-disconnect' is the only approciate way
to change the value of a signal. Using other setter like `setq', `let'
or etc. ruins the signal mechanism.\n\n" docstring)))

(defmacro undefsignal (name)
  "Undefining a signal."
  `(unintern ,name))

(cl-defun signal-connect (&key signal arg worker)
  "Connect a SIGNAL to its WORKER function.
Use `signal-emit' to emit a SIGNAL.

After a signal is emitted, the WORKER function is
called with arguments ARG.

If mutiple connections have been made, the WORKER functions
are called in order or making connection.

Example:
\(signal-connect :signal 'my-signal
                :worker 'message
                :arg '(\"To print a message with %d %s.\" 100 \"words\"))"

(unless (and signal worker)
  (error "Signal and worker must be provided."))

(push (or (and arg (list worker arg))
          (list worker))
      (symbol-value signal)))


(cl-defun signal-disconnect (signal worker)
  "Disconnect a SIGNAL form its WORKER function.
If multiple connections of same worker have been made,
all of them are disconnected

Example:
\(signal-disconnect :signal 'my-signal
                   :worker 'message"
  (while (assoc worker (symbol-value signal))
    (set signal (delete (assoc worker (symbol-value signal)) (symbol-value signal)))))


(cl-defun signal-emit (signal &key delay arg)
  "Emit a SIGNAL. The worker function(s) will be invoked.

DELAY is the second the worker functions delayed to run after
the signal has been emitted. It can be a floating point number
which specifies a fractional number of seconds to wait.
By default, it is 0 second.

ARG provides emit-time argument passing to the worker funcitons.

Example:
\(signal-emit 'my-signal)"
  (when (boundp signal)
    (run-with-timer (or delay 0) nil 'signal-emitb signal :arg arg)
    t))

(cl-defun signal-emitb (signal &key arg)
  "Emit a blocking SIGNAL. The worker function(s) will be invoked.

ARG provides emit-time argument passing to the worker funcitons

Example:
\(signal-emitb 'my-signal)"
  (when (boundp signal)
    (dolist (signal-1 (nreverse (copy-sequence (symbol-value signal))))
      (when (fboundp (car signal-1))
        (ignore-errors (apply (car signal-1) (or arg (cadr signal-1))))))
    t))


(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(defsignal\\)\\_>[ 	'(]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
                           (1 font-lock-keyword-face)
                           (2 font-lock-type-face nil t)
                           )
                          ("(\\(undefsignal\\|signal-connect\\|signal-disconnect\\|signal-emit\\|signal-emitb\\)\\_>[ 	'(]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
                           (1 font-lock-warning-face nil))))

(provide 'signal)
;;; signal.el ends here
