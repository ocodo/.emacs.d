;;; jss-remote-value.el -- code for in-emacs printing and expanding of browser side objects
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
(require 'jss-browser-api)

(defgeneric jss-insert-remote-value (value)
  "Insert a link in an emacs buffer to a browser side object.

A remote value is either primitive, or non-primitive. A primitive
value is any object where we can create a new one, which is
however === to another one, by typing in something on the
prompt. This means numbers, strings, and the constants true,
false, null and undefined. Everything else is a non-primitive object.

Non-primitives are mode up of a number of propreties. Depending
on the 'kind' of object, using suggestions provided by the
browser since they're all javascript objects underneath, we have
specific code for rendering:

* dates -  just show the is8601 timestamp

* regular expression - pretty print the regexp itself

* arrays - show it as a bracket list of values (the current
  visualization could be a problem for space arrays (there's no
  indication that certain indexes have been skipped)).

* functions - we just show a truncated piece of the source
  code (the first 40 and last 40 characters) as a button linking
  to the sourec code itself (when we have it).

* objects - for normal objects we just list the properties.

Auto expanding small objects: when the object has less than
jss-remote-value-auto-expand-property-limit (including the
__proto__ field) we'll automatically, but asynchronously, expand
the object in place. This is convient for small objects created
in the prompt and for viewing the contents of exceptions, but can
cause some buffer flicker due to the asynchronous-ness of it.")

(defmethod jss-insert-remote-value ((value jss-generic-remote-primitive))
  (jss-remote-value-insert-description value))

(defmethod jss-insert-remote-value ((value jss-generic-remote-non-primitive))
  (let ((start (point)))
    (jss-wrap-with-text-properties (list 'jss-remote-value value 'jss-remote-value-collapsed t)
      (jss-remote-value-insert-description value))
    (jss-add-text-button start (point) 'jss-remote-value-expand-at-point)))

(defvar jss-remote-value-auto-expand-property-limit 6)

(defmethod jss-insert-remote-value :after ((value jss-generic-remote-object))
  (jss-autoexpand-small-remote-object value jss-remote-value-auto-expand-property-limit))

(defmethod jss-autoexpand-small-remote-object ((object jss-generic-remote-object) max-property-length)
  (when max-property-length
    (lexical-let ((object object)
                  (max-property-length max-property-length)
                  (buffer (current-buffer)))
      (jss-deferred-add-callback
       (jss-remote-object-get-properties object (jss-current-tab))
       (lambda (properties)
         (when (and max-property-length
                    (<= (length properties) max-property-length))
           (jss-remote-value-replace-with-properties object properties buffer)))))))

(put 'jss-replace-with-default-property 'lisp-indent-function 1)

(defun jss-add-text-property-unless-exists (start end property-name property-value)  
  (save-excursion
    (block nil
      (goto-char start)
      (while (< (point) end)
        (setf start (point))
        (while (not (get-text-property (point) property-name))
          (forward-char 1)
          (when (= (point) end)
            (add-text-properties start (point) (list property-name property-value))
            (return)))
        (add-text-properties start (point) (list property-name property-value))
        (when (get-text-property (point) property-name)
          (forward-char 1)
          (when (= (point) end)
            (return)))))))

(defmethod jss-remote-value-collapsed ((value jss-generic-remote-object))
  "Returns T if `value` is current collapsed."
  (let ((loc (jss-find-property-block 'jss-remote-value value)))
    (save-excursion
      (goto-char (car loc))
      (get-text-property (point) 'jss-remote-value-collapsed))))

(defmethod jss-remote-value-expand ((value jss-generic-remote-object))
  "Inserts the properties of `value` into the current buffer (and
sets `value` as non-collapsed)."
  (when (jss-remote-value-collapsed value)

    (jss-replace-with-default-property
        (jss-remote-value value :test 'eq)
      (jss-wrap-with-text-properties (list 'jss-remote-value-collapsed t)
        (insert "{expanding ")
        (jss-remote-value-insert-description value)
        (insert "}")))
    
    (lexical-let ((buffer (current-buffer))
                  (value value))

      (jss-deferred-add-callback
       (jss-remote-object-get-properties value (jss-current-tab))
       (lambda (properties)
         (jss-remote-value-replace-with-properties value properties buffer))))))

(defvar jss-remote-value-left-column 0
  "Leftmost column to use when indenting remote values.")

(defun jss-remote-value-insert-as-object-properties (alist separator identp)
  "Insert the list of properties `alist` into the current buffer
and put `separator` between each name/value pair. If `identp` is
non-NIL then ident each line before inserting the key (each key,
value pair goes on the same line)"
  (loop for first = t then nil
        for (prop . more) on alist
        when identp
          do (indent-to-column jss-remote-value-left-column)
        do (jss-insert-with-highlighted-whitespace (car prop))
        do (insert ": ")
        do (jss-insert-remote-value (cdr prop))
        when more
        do (insert separator)))

(defmethod jss-remote-value-replace-with-properties ((value jss-generic-remote-object) properties buffer)
  "Delete the current representation of `value` and put its
properties in its place."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (jss-remote-value-collapsed value) 
        (jss-replace-with-default-property
            (jss-remote-value value :test 'eq)
          (let ((jss-remote-value-left-column (+ 2 (current-column)))
                (jss-remote-value-auto-expand-property-limit
                 ;; resetting
                 ;; jss-remote-value-auto-expand-property-limit here
                 ;; prevents use from sending off mny (possibly
                 ;; hundreds) of outaexpand requests when expanding a
                 ;; huge object.
                 ;;
                 ;; the justificiation is that if you're expanding a
                 ;; huge object you can go and find the specific
                 ;; properties you're interested in yourself,
                 ;; autoexpansion is only really useful at the outermost level
                 (if (<= (length properties) jss-remote-value-auto-expand-property-limit)
                     jss-remote-value-auto-expand-property-limit
                   nil)))
            (jss-remote-value-insert-description value)
            (insert " {\n")
            (jss-remote-value-insert-as-object-properties properties ",\n" t)
            (insert "}")))))))

(defmethod jss-remote-value-expand ((value jss-generic-remote-function))
  (jss-deferred-add-backs
   (jss-remote-function-get-source-location value (jss-current-tab))
   (lambda (location)
     (apply 'jss-script-display-at-position location))
   (lambda (error)
     (message "No source location for function."))))

(defmethod jss-remote-value-expand ((value jss-generic-remote-array))
  (lexical-let ((tab (jss-current-tab))
                (value value)
                (buffer (current-buffer)))
    (jss-replace-with-default-property (jss-remote-value value :test 'eq)
      (insert "[expanding]")
      (jss-deferred-add-callback
       (jss-remote-object-get-properties value (jss-current-tab))
       (lambda (properties)
         (jss-remote-value-replace-with-properties value properties buffer))))))

(defmethod jss-remote-value-replace-with-properties ((value jss-generic-remote-array) properties buffer)
  "Replace the array `value` with its elements."
  (let ((integer-properties '())
        (other-properties '()))
    (dolist (prop properties)
      (destructuring-bind (key . value) prop
        (if (save-match-data (string-match "^[[:digit:]]+$" key))
            (push (cons (string-to-number key) value) integer-properties)
          (push prop other-properties))))
    (jss-replace-with-default-property (jss-remote-value value :test 'eq)
      (with-current-buffer buffer
        (loop
         initially (insert "[")
         for item in (cl-sort integer-properties '< :key 'car)
         for first = t then nil
         unless first do (insert ", ")
         do (jss-insert-remote-value (cdr item))
         finally (insert "]"))
        (insert " {")
        (jss-remote-value-insert-as-object-properties other-properties ", " nil)
        (insert "}")))))

(defun jss-remote-value-expand-at-point ()
  "Expand the remote value currently at point."
  (interactive)
  (let ((value (get-text-property (point) 'jss-remote-value)))
    (unless value
      (error "No value at point."))
    (jss-remote-value-expand value)))

(defun jss-expand-nearest-remote-value ()
  "Find the closes remote value to point and expand it."
  (interactive)
  (let ((nearest-before nil)
        (nearest-before-distance 0)
        (nearest-after nil)
        (nearest-after-distance 0))
    (save-excursion
      (while (and (not (get-text-property (point) 'jss-remote-value-collapsed))
                  (< (point-min) (point)))
        (backward-char 1)
        (incf nearest-before-distance))
      (if (get-text-property (point) 'jss-remote-value-collapsed)
          (setf nearest-before (get-text-property (point) 'jss-remote-value))
        (setf nearest-before-distance nil)))
    (save-excursion
      (while (and (not (get-text-property (point) 'jss-remote-value-collapsed))
                  (< (point) (point-max)))
        (forward-char 1)
        (incf nearest-after-distance))
      (if (get-text-property (point) 'jss-remote-value-collapsed)
          (setf nearest-after (get-text-property (point) 'jss-remote-value))
        (setf nearest-after-distance nil)))
    (cond
     ((and nearest-before-distance nearest-after-distance)
      (jss-remote-value-expand (if (<= nearest-before-distance nearest-after-distance)
                                   nearest-before
                                 nearest-after)))
     (nearest-before-distance (jss-remote-value-expand nearest-before))
     (nearest-after-distance  (jss-remote-value-expand nearest-after))
     (t (message "No expandable values around point.")))))

(provide 'jss-remote-value)
