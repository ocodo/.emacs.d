;;; jss-deferred.el -- jss's implementation of the deferred/async/future idea
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

(defclass jss-deferred ()
  ((callbacks :initarg :callbacks :accessor jss-deferred-callbacks)
   (errorbacks :initarg :errorbacks :accessor jss-deferred-errorbacks)
   (state :initform (cons :waiting nil) :accessor jss-deferred-state))
  (:documentation "A deferred is some action the will complete,
either succeffuly or not, at some point in the future.

the deferred's action generates a value, this value is passed to
the deferred's callbacks (if the action completed succeffuly) or
to its errorbacks (if the action failed).

There are two ways of dealing with asynchronous actions in an
inherintly sychronous language (such as elisp), either you pass
callbacks to the function which triggers an action, or the action
triggering function returns an obbect whose state will change
when the action is complete. deferrreds are one, of many,
possible implementations of the latter approach."))

(defun make-jss-deferred (&optional callback errorback)
  (make-instance 'jss-deferred
                 :callbacks (if callback
                                (list callback)
                              '())
                 :errorbacks (if errorback
                                 (list errorback)
                               '())))

(defun* make-jss-completed-deferred (&optional callback errorback)
  "Create a deferred object which has already completed.

Sometime we have to return a deferred but we've already done the
required computation, this helper function creates a trivial
deferred which will call its callbacks/errorbacks as soon as
possible since its value has alreay been computed."
  (let ((d (make-jss-deferred)))
    (cond
     ((and callback (not errorback))
      (jss-deferred-callback d callback))
     ((and errorback (not callback))
      (jss-deferred-errorback d errorback))
     (t
      (error "Invalid arguments to make-jss-completed-deferred. Exactly one of :callback, :errorback must be specified.")))
    d))

(defmacro appendf (place &rest elements)
  `(setf ,place (append ,place ,@elements)))

(defun jss-deferred-funcall (back value)
  "Trivial wrapper around funcall so that we can, when
neccessary, add extra error handling to a deferred's callback."
  (funcall back value)
  ;(condition-case e
  ;    (funcall back value)
  ;  (error (message "got error.")))
  )

(defmethod jss-deferred-add-callback ((d jss-deferred) callback)
  "Add a function to be called when `d` completes."
  (if (eql :ok (car (jss-deferred-state d)))
      (jss-deferred-funcall callback (cdr (jss-deferred-state d)))
    (appendf (jss-deferred-callbacks d) (list callback)))
  d)

(defmethod jss-deferred-add-errorback ((d jss-deferred) errorback)
  "Add a function to be called when `d` fails."
  (if (eql :fail (car (jss-deferred-state d)))
      (jss-deferred-funcall errorback (cdr (jss-deferred-state d)))
    (appendf (jss-deferred-errorbacks d) (list errorback)))
  d)

(defmethod jss-deferred-add-backs ((d jss-deferred) &optional callback errorback)
  (lexical-let ((new-deferred (make-jss-deferred)))
    (when callback  (jss-deferred-add-callback d callback))
    (when errorback (jss-deferred-add-errorback d errorback)))  
  d)

(defmethod jss-deferred-callback ((d jss-deferred) value)
  "Successffully compete the deferred `d` with value
`value`. Will immediatly call all of `d`'s callbacks."
  (while (jss-deferred-callbacks d)
    (jss-deferred-funcall (pop (jss-deferred-callbacks d)) value))
  (setf (jss-deferred-state d) (cons :ok value))
  value)

(defmethod jss-deferred-errorback ((d jss-deferred) value)
  "Unsuccessfully complete the deferred `d` with value
`value`. Will immediatly call all of `d`'s errorbacks."
  (while (jss-deferred-errorbacks d)
    (jss-deferred-funcall (pop (jss-deferred-errorbacks d)) value))
  (setf (jss-deferred-state d) (cons :fail value))
  value)

(defun jss-deferred-then (before callback &optional errorback)
  "Creates a new deferred which is triggered after `before`. 

after, the returned deferred, will be passed the result of
applying callback to `before`'s value."
  (lexical-let ((after (make-jss-deferred))
                (callback callback)
                (errorback errorback))
    (jss-deferred-add-callback before
                               (lambda (value)
                                 (jss-deferred-callback after (funcall callback value))))
    (if errorback
        (jss-deferred-add-errorback before
                                    (lambda (value)
                                      (jss-deferred-errorback after (funcall errorback value))))
      (jss-deferred-errorback before (lambda (value) (jss-deferred-errorback after value))))
    after))

(defun jss-deferred-wait-on-all (&rest deferreds)
  "given a list of deferred return a new deferred which will
complete when every one of `deferred` has completed.

If all of `deferreds` succeed then the return deferred`s callback
will be called with a single argument, the list of values (in an
arbitrary order) of the deferredse If any of `deferreds` failes
the returned deferred's errorback will be called with two values:
the list or error values and the list of success values (which
may be null)"
  (lexical-let ((after (make-jss-deferred))
                (successes '())
                (failures  '())
                (pending   '()))
    (dolist (this deferreds)
      (lexical-let ((this this))
        (jss-deferred-add-backs this
                                (lambda (value)
                                  (setf pending (delq this pending)
                                        successes (cons this value))
                                  (when (null pending)
                                    (if failures
                                        (jss-deferred-errorback after (list failures successes))
                                      (jss-deferred-callback after (list successes)))))
                                (lambda (value)
                                  (setf pending (delq this pending)
                                        failures (cons this value))
                                  (when (null pending)
                                    (jss-deferred-errorback after (list failures successes)))))))
    after))

(provide 'jss-deferred)
