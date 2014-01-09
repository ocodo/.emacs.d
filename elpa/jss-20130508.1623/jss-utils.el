;;; jss-utils.el -- generic utilities. buttons, labels, sections, a few macros and utility functions.
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

(defface jss-button-face '((t :underline t))
  "Face used for jss-buttons."
  :group 'jss)

(defvar jss-button-map (let ((map (make-sparse-keymap)))
                         (define-key map (kbd "RET") 'jss-invoke-primary-action)
                         (define-key map (kbd "SPC") 'jss-invoke-secondary-action)
                         map))

(defun jss-insert-button (label &rest jss-add-text-button-args)
  "Insert a jss-button with text `label` at the current point in
the current buffer. Pass `jss-add-text-button-args` to
`jss-add-text-button`."
  (let ((start (point)))
    (insert-and-inherit label)
    (apply 'jss-add-text-button start (point) jss-add-text-button-args)
    label))

(defun* jss-add-text-button (start end primary-action &key secondary-action other-properties)
  "Create a jss-button, whose primary (RET) action is
`primary-action´ and whose secondary action (SPC) is
`secondary-action` from the positions `start` to `end`.

`other-properties`, if specified, are added as text properties
from `start` to `end`."
  (let ((o (make-overlay start end (current-buffer))))
    (overlay-put o 'face 'jss-button-face)
    (overlay-put o 'keymap jss-button-map)
    (add-text-properties start end
                         (append (list 'jss-button t
                                       'read-only t
                                       'rear-nonsticky t)
                                 (when primary-action
                                   (list 'jss-primary-action primary-action))
                                 (when secondary-action
                                   (list 'jss-secondary-action secondary-action))
                                 other-properties))))

(defun jss-invoke-property (property-name)
  (when (get-text-property (point) property-name)
    (call-interactively (get-text-property (point) property-name))
    t))

(defun jss-invoke-primary-action ()
  (interactive)
  (or (jss-invoke-property 'jss-primary-action)
      (call-interactively 'self-insert-command)))

(defun jss-invoke-secondary-action ()
  (interactive)
  (or (jss-invoke-property 'jss-secondary-action)
      (call-interactively 'self-insert-command)))

(defun jss-next-button ()
  "Move point to the start the next jss-button after point."
  (interactive)
  (let ((target nil))
    (save-excursion
      (when (get-text-property (point) 'jss-button)
        (goto-char (jss-end-of-current-property-block 'jss-button)))
      (let ((next (jss-start-of-next-property-block 'jss-button nil)))
        (if next
            (setf target next)
          (goto-char (point-min))
          (setf target (jss-start-of-next-property-block 'jss-button nil)))))
    (when target
      (goto-char target))))

(defun jss-previous-button ()
  "Move point to the first buton before point."
  (interactive)
  (let ((target nil))
    (save-excursion
      (when (get-text-property (point) 'jss-button)
        (goto-char (jss-start-of-current-property-block 'jss-button))
        (if (bobp)
            (goto-char (point-max))
          (backward-char 1)))
      (let ((prev (jss-end-of-previous-property-block 'jss-button nil)))
        (if prev
            (setf target prev)
          (goto-char (point-max))
          (setf target (jss-end-of-previous-property-block 'jss-button nil)))))
    (when target
      (goto-char target)
      (backward-char 1)
      (goto-char (jss-start-of-current-property-block 'jss-button)))))

(defun jss-log-event (event)
  "Debugger method, put some text describing `event` in the buffer *jss-events*"
  (let* ((buf-name " *jss-events*")
         (buf (get-buffer buf-name)))
    (when (null buf)
      (setf buf (get-buffer-create buf-name))
      (buffer-disable-undo buf))
    (with-current-buffer buf
      (insert (format ";; %s\n" (format-time-string "%Y-%m-%dT%T")))
      (dolist (event-part event)
        (insert (prin1-to-string event-part) "\n"))
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (goto-char (point-max)))))

(defface jss-whitespace-mark-face '((t :inherit font-lock-comment-face))
  "Face user to mark significant whitespace."
  :group 'jss)

(defun jss-insert-as-whitespace (string)
  (jss-wrap-with-text-properties (list 'face 'jss-whitespace-mark-face)
    (insert-and-inherit string)))

(defun jss-eol-mark ()
  (when (member (preceding-char) (list ?  ?\n ?\t ?\r))
    (jss-insert-as-whitespace "$"))
  (insert-and-inherit "\n"))

(defun jss-insert-with-highlighted-whitespace (string)
  (save-match-data
    (when (string= "" string)
      (jss-insert-as-whitespace "^$"))
    (when (string-match "^[ \t\r\n\f]" string)
      (jss-insert-as-whitespace "^"))
    (loop
     for char across string
     do (case char
          (?\s (jss-insert-as-whitespace "_"))
          (?\t (jss-insert-as-whitespace "\\t"))
          (?\n (jss-insert-as-whitespace "\\n"))
          (?\r (jss-insert-as-whitespace "\\r"))
          (?\f (jss-insert-as-whitespace "\\f"))
          (t
           (insert-and-inherit (char-to-string char))
           (remove-text-properties (1- (point)) (point) (list 'face t 'font-lock-face t)))))
    (when (string-match "[ \t\r\n\f]$" string)
      (jss-insert-as-whitespace "$"))))

(defun jss-section-marker ()
  (insert-and-inherit "--------------------------------\n"))

(defun jss-have-next-property-block (property-name)
  "Returns T if the current buffer has a block at or
after (point) with the text property `property-name`."
  (or (get-text-property (point) property-name)
      (next-single-property-change (point) property-name)))

(defun jss-have-previous-property-block (property-name)
  "Returns T if the current buffer has a block at or
before (point) with the text property `property-name`."
  (or (get-text-property (point) property-name)
      (previous-single-property-change (point) property-name)))

(defun* jss-start-of-next-property-block (property-name &optional (error t))
  "Moves point to the first char of the next block with property
`property-name`. If `error` is non-NIL signals and error if there is
no next block with the required property."
  (block nil
    (when (get-text-property (point) property-name)
      (return (jss-start-of-current-property-block property-name)))
    (let ((next-change (next-single-property-change (point) property-name)))
      (when next-change
        (return (goto-char next-change)))
      (while (not (get-text-property (point) property-name))
        (when (= (point) (point-max))
          (if error
              (error "Unable to find start of next block with property %s" property-name)
            (return nil)))
        (forward-char 1))
      (return (point)))))

(defun* jss-end-of-previous-property-block (property-name &optional (error t))
  "Moves point to the last char of the nexprevious block with
property `property-name`. If `error` is non-NIL signals and error
if there is no next block with the required property."
  (block
      nil
    (when (get-text-property (point) property-name)
      (return (jss-end-of-current-property-block property-name)))

    (let ((previous-change (if (eobp) ;; previous-single-property-change works differently at eobp, a char by char search is easier
                               nil
                             (previous-single-property-change (point) property-name))))
      (when previous-change
        (return (goto-char previous-change)))
      (while (not (get-text-property (point) property-name))
        (when (= (point) (point-min))
          (if error
              (error "Unable to find previous block with property %s" property-name)
            (return nil)))
        (backward-char 1))
      (return (point)))))

(defun jss-start-of-current-property-block (property-name)
  (unless (get-text-property (point) property-name)
    (error "Attempting to get start of current block with property %s, but point doesn't have this property." property-name))
  (block nil
    (while (get-text-property (point) property-name)
      (when (= (point) (point-min))
        (return (point)))
      (backward-char 1))
    (forward-char 1))
  (point))

(defun jss-end-of-current-property-block (property-name)
  (unless (get-text-property (point) property-name)
    (error "Attempting to get end of current block with property %s, but point doesn't have this property." property-name))
  (block nil
    (while (get-text-property (point) property-name)
      (when (= (point) (point-max))
        (return))
      (forward-char 1)))
  (point))

(defun* jss-find-property-block (property-name property-value &key (test 'equal) (error t))
  "Returns a cons of (start . end) of the proerty block, a
sequence fo char which all habe the property named
`property-name` whose value is `test` to `property-value` in the
current buffer.

Note: this function does not deal well when there are multiple
blocks with the same property name and value, make sure to use
rear-nonsticky to maintain blocks as continguous sequences of
chars."
  (block nil
    (save-excursion
      (goto-char (point-max))
      (let (block-start block-end)

        (while (not (funcall test (get-text-property (point) property-name) property-value))
          (when (= (point) (point-min))
            (if error
                (error "Unable to find block with property %s %s to %s in buffer %s." property-name test property-value (current-buffer))
              (return)))
          (backward-char 1))
        (setf block-end (min (1+ (point)) (point-max)))

        (block nil
          (while (funcall test (get-text-property (point) property-name) property-value)
            (when (= (point) (point-min))
              (return))
            (backward-char 1))
          (forward-char 1))
        
        (cons (point) block-end)))))

(defun* jss-delete-property-block (property-name property-value &key (test 'equal) (error t))
  (let ((location (jss-find-property-block property-name property-value :test test :error error))
        (inhibit-read-only t))
    (when location
      (delete-region (car location) (cdr location)))))

(defun jss-insert-with-properties (property-list format-control &rest format-args)
  (let ((start (point)))
    (insert-and-inherit (apply 'format format-control format-args))
    (add-text-properties start (point) property-list)))

(defmacro* jss-replace-with-default-property ((property-name property-value &key (test 'eq)) &body body)
  "Find the block in the current buffer with the text-property
`property-name` whose value is `property-value`, delete this
block, move point to where the block was, run `body` and then add
the text property `property-name` with value `property-value`
back (from the old start to where `body` left point)"
  (declare (indent 1))
  (let ((loc (cl-gensym)) (prop-val (cl-gensym)))
    `(let* ((,prop-val ,property-value)
            (,loc (jss-find-property-block ',property-name ,prop-val :test ,test))
            (inhibit-read-only t))
       (save-excursion
         (goto-char (car ,loc))
         (delete-region (car ,loc) (cdr ,loc))
         (let ((start (point)))
           (prog1
               (progn ,@body)
             (jss-add-text-property-unless-exists (car ,loc) (point)
                                                  ',property-name
                                                  ,prop-val)))))))

(defmacro jss-wrap-with-text-properties (properties &rest body)
  (declare (indent 1))
  (let ((start (cl-gensym)))
    `(let ((,start (point)))
       (prog1
           (progn ,@body)
         (let ((inhibit-read-only t))
           (add-text-properties ,start (point) ,properties))))))

(defun jss-limit-string-length (string max-length)
  "if `string` is longer than `max-length` returns the first (/
  max-length 2) chars and the last (/ max-length 2) chars with
  \"...[snip]...\" between them. "
  (if (< max-length (length string))
      (format "%s...[snip]...%s"
              (substring string 0 (/ max-length 2))
              (substring string (- (length string) (/ max-length 2)) (length string)))
      string))

(defun jss-funcall-or-insert (thing)
  (if (stringp thing)
      (insert-and-inherit thing)
    (funcall thing)))

(defun* jss-toggling-visibility (header body &key (initially-visibile nil))
  (let (header-start
        header-end
        body-start
        body-end)
    (setf header-start (point))
    (jss-funcall-or-insert header)
    (setf header-end (point))
    (setf body-start (point))
    (jss-funcall-or-insert body)
    (setf body-end (point))
    (lexical-let ((body-overlay (make-overlay body-start body-end (current-buffer) t nil)))
      (jss-add-text-button header-start header-end
                           (lambda ()
                             (interactive)
                             (jss-toggle-text-visibility body-overlay)))
      (let ((inhibit-read-only t))
        (overlay-put body-overlay 'invisible t)
        (overlay-put body-overlay 'before-string "...\n")
        (when initially-visibile
          (jss-toggle-text-visibility body-overlay))))))

(defun jss-toggle-text-visibility (body-overlay)
  (interactive)
  (jss-overlay-toggle-invisibile body-overlay)
  (overlay-put body-overlay 'before-string
               (when (overlay-get body-overlay 'invisible)
                 "...\n")))

(defun jss-overlay-toggle-invisibile (overlay)
  (overlay-put overlay 'invisible (not (overlay-get overlay 'invisible))))

(defun jss-toggling-sections (button-a body-a button-b body-b)
  (lexical-let (button-a-overlay
                body-a-overlay
                button-b-overlay
                body-b-overlay)
    (cl-flet ((make-overlay-around (thing)
                                   (let ((start (point)))
                                     (jss-funcall-or-insert thing)
                                     (make-overlay start (point) (current-buffer) t nil))))
      
      (setf button-a-overlay (make-overlay-around button-a)
            button-b-overlay (make-overlay-around button-b)
            body-a-overlay (make-overlay-around body-a)
            body-b-overlay (make-overlay-around body-b))
      
      (overlay-put button-a-overlay 'invisible nil)
      (overlay-put body-a-overlay   'invisible nil)
      (overlay-put button-b-overlay 'invisible t)
      (overlay-put body-b-overlay   'invisible t)

      (lexical-let ((toggle-function (lambda ()
                                       (interactive)
                                       (jss-overlay-toggle-invisibile button-a-overlay)
                                       (jss-overlay-toggle-invisibile button-b-overlay)
                                       (jss-overlay-toggle-invisibile body-a-overlay)
                                       (jss-overlay-toggle-invisibile body-b-overlay))))
        
        (jss-add-text-button (overlay-start button-a-overlay)
                             (overlay-end button-a-overlay)
                             toggle-function)

        (jss-add-text-button (overlay-start button-b-overlay)
                             (overlay-end button-b-overlay)
                             toggle-function)))))

(defun* jss-completing-read (prompt choices
                             &key (history nil) (require-match t)
                                  (initial-input nil))
  "Calls eiither ido-completing-read or completing-read depending
on the variable ido-mode."
  (funcall (if ido-mode
               'ido-completing-read
             'completing-read)
           prompt choices nil require-match initial-input history nil))

(defun jss-insert-read-only (&rest insert-args)
  (jss-wrap-with-text-properties (list 'read-only t)
    (let ((inhibit-read-only t))
      (apply 'insert insert-args))))

(defmacro* jss-when-bind ((var value) &rest body)
  `(jss-if-bind (,var ,value) (progn ,@body)))

(put 'jss-when-bind 'lisp-indent-function 1)

(defmacro* jss-if-bind ((var value) then &rest else)
  `(let ((,var ,value))
     (if ,var
         ,then
       ,@else)))

(put 'jss-if-bind 'lisp-indent-function 2)

(defun jss-yes-or-no-p (prompt)
  (let ((response (jss-completing-read prompt (list "yes" "no") :require-match nil)))
    (string= "yes" response)))

(defun jss-parse-integer (string)
  (save-match-data
    (and (string-match "^[0-9]+$" string)
         (string-to-number string))))

(defmacro* jss-with-alist-values ((&rest keys) alist-form &body body)
  (let ((alist-value (cl-gensym)))
    `(let ((,alist-value ,alist-form))
       (let ,(mapcar (lambda (key)
                       (list key `(cdr (assoc ',key ,alist-value))))
                     keys)
         ,@body))))
(put 'jss-with-alist-values 'lisp-indent-function 2)

(defclass jss-queue ()
  ((list :initform '())))

(defun make-jss-queue ()
  (make-instance 'jss-queue))

(defmethod jss-enqueue ((q jss-queue) item)
  (setf (slot-value q 'list) (append (slot-value q 'list) (list item))))

(defmethod jss-dequeue ((q jss-queue))
  (pop (slot-value q 'list)))

(defmethod jss-queue-empty-p ((q jss-queue))
  (null (slot-value q 'list)))

(provide 'jss-utils)
