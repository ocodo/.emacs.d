;;; jss-prompt.el -- code for reading and printing javascript code evaluations in a buffer
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

;;; the jss prompt is designed so that it can be embedded in multiple
;;; places (the console buffer and the debugger for now).

(eval-when-compile
  (require 'cl))
(require 'eieio)
(require 'jss-browser-api)
(require 'jss-remote-value)

(defvar jss-prompt-map
  (let ((map (make-sparse-keymap)))
    ;; (set-keymap-parent map jss)
    map))

(define-key jss-prompt-map (kbd "RET") 'jss-prompt-eval-or-newline)
(define-key jss-prompt-map (kbd "C-a") 'jss-prompt-beginning-of-line)
(define-key jss-prompt-map (kbd "M-p") 'jss-prompt-insert-previous-input)
(define-key jss-prompt-map (kbd "M-n") 'jss-prompt-insert-next-input)
(define-key jss-prompt-map (kbd "C-c C-i") 'jss-expand-nearest-remote-value)

(make-variable-buffer-local
 (defvar jss-prompt-input-history '()))

(make-variable-buffer-local
 (defvar jss-prompt-input-history/last-inserted nil))

(defvar jss-prompt-counter 0)

(defclass jss-prompt ()
  ((submit-function :initarg :submit-function :accessor jss-prompt-submit-function)
   (id :initform (incfo jss-prompt-counter) :reader jss-prompt-id)
   (buffer :initarg :buffer :reader jss-prompt-buffer)
   (active-p :initform t :accessor jss-prompt-active-p)
   (history :initarg :history :reader jss-prompt-history)
   (history-offset :initform nil :accessor jss-prompt-history-offset)

   (marker-overlay :initform nil :accessor jss-prompt-marker-overlay)
   (input-overlay  :initform nil :accessor jss-prompt-input-overlay)
   (output-overlay :initform nil :accessor jss-prompt-output-overlay)

   (local-map)
   (keymap)))

(defun* jss-insert-prompt (submit-function &key local-map keymap previous-prompt)
  "Insert a prompt which can be used to send javascript to the browser and view the resulting value.

The prompt is just a line of editable text (even if the rest of
the buffer is read-only). Hitting enter will check the code for
syntax errors and, if it's valid js according to js2-parse, send
it to the server (use a prefix arg to RET to avoid this syntax
check).

The return value is inserted after the propmt using `jss-insert-remote-value`.

Prompts also have history, using M-p and M-n (M-r not yet
implemented)."
  (unless (or (bobp) (= (point) (line-beginning-position)))
    (insert "\n"))
  (let ((prompt (make-instance 'jss-prompt
                               :submit-function submit-function
                               :buffer (current-buffer)
                               :history (if previous-prompt
                                            (cons (jss-prompt-input-text previous-prompt)
                                                  (jss-prompt-history previous-prompt))
                                          '())))
        (inhibit-read-only t))

    (jss-wrap-with-text-properties (list 'jss-prompt prompt)

      (let ((marker-start (point)))
        (jss-wrap-with-text-properties (list 'read-only t 'rear-nonsticky t)
          (insert "> "))
        (setf (jss-prompt-marker-overlay prompt) (make-overlay marker-start (point) (current-buffer) t)))
      
      (let ((input-start (point)))
        (jss-wrap-with-text-properties (list 'jss-prompt-input-end-marker t
                                             'read-only t
                                             'rear-nonsticky t)
          (insert "\n"))
        (setf (jss-prompt-input-overlay prompt) (make-overlay input-start (point)))
        (overlay-put (jss-prompt-input-overlay prompt) 'face 'highlight)

        (cl-labels ((make-parent-map (child)
                      (let ((map (copy-keymap child)))
                        (set-keymap-parent map jss-prompt-map)
                        map))
                    (set-local-map (map)
                      (setf (slot-value prompt 'local-map) (make-parent-map map))
                      (overlay-put (jss-prompt-input-overlay prompt) 'local-map (slot-value prompt 'local-map)))
                    (set-keymap (map)
                      (setf (slot-value prompt 'keymap) map)
                      (overlay-put (jss-prompt-input-overlay prompt) 'keymap  (slot-value prompt 'keymap))))
          (cond
           (local-map
            (set-local-map local-map))
           (keymap
            (set-keymap keymap))
           (previous-prompt
            (cond
             ((slot-boundp previous-prompt 'local-map)
              (set-local-map (slot-value previous-prompt 'local-map)))
             ((slot-boundp previous-prompt 'keymap)
              (set-keymap (slot-value previous-prompt 'keymap)))
             (t
              (set-keymap jss-prompt-map))))
           (t
            (set-keymap jss-prompt-map))))))
    prompt))

(defmethod jss-prompt-start-of-input ((prompt jss-prompt))
  (overlay-start (jss-prompt-input-overlay prompt)))

(defmethod jss-prompt-start-of-output ((prompt jss-prompt))
  (overlay-start (jss-prompt-output-overlay prompt)))

(defmethod jss-prompt-end-of-output ((prompt jss-prompt))
  (overlay-end (jss-prompt-output-overlay prompt)))

(defun jss-before-last-prompt ()
  (goto-char (point-max))
  (jss-end-of-previous-property-block 'jss-prompt)
  (let ((last-prompt (get-text-property (point) 'jss-prompt)))
    (goto-char (overlay-start (jss-prompt-marker-overlay last-prompt)))))

(defun jss-prompt-next-input ()
  (jss-start-of-next-property-block 'jss-prompt)
  (let ((next-prompt (get-text-property (point) 'jss-prompt)))
    (goto-char (overlay-start (jss-prompt-input-overlay next-prompt)))))

(defun* jss-prompt-current-prompt (&optional (warn t))
  "Returns the prompt object around point. Uses some heuristics
  to figure out what the current prompt is."
  (save-excursion
    (block nil
      (when (get-text-property (point) 'jss-prompt)
        ;; directly in a prompt
        (return (get-text-property (point) 'jss-prompt)))
      (unless (bobp)
        (backward-char 1)
        (when (get-text-property (point) 'jss-prompt)
          (return (get-text-property (point) 'jss-prompt))))
      (warn "Not currently in a prompt."))))

(defmethod jss-prompt-input-text ((prompt jss-prompt))
  (with-current-buffer (jss-prompt-buffer prompt)
    (let* ((overlay (jss-prompt-input-overlay prompt))
           (start (overlay-start overlay))
           (end   (overlay-end   overlay))
           (input ""))
      (save-excursion
        (goto-char start)
        (while (and (< (point) end) (not (eobp)))
          (unless (get-text-property (point) 'jss-prompt-input-end-marker)
            (setf input (concat input (buffer-substring-no-properties (point) (1+ (point))))))
          (forward-char 1)))
      input)))

(defun jss-prompt-eval-or-newline (force-eval)
  "Evaluate the current input or insert a newline if js2 thinks
there are syntax errors in teh input.

Supply a prefix arg to force sending the current text"
  (interactive "P")
  (block nil
    (let ((prompt (jss-prompt-current-prompt)))

      (when force-eval
        (return (jss-prompt-submit prompt)))
      
      (let ((js2-parse-errors
             (if (fboundp 'js2-parse)
                 (with-temp-buffer
                   (insert (jss-prompt-input-text prompt))
                   (funcall 'js2-ast-root-errors (js2-parse)))
               ;; don't have js2-parse :( send the input and let the browser syntax check it
               '())))
        
        (if js2-parse-errors
            (progn
              (message "Input has errors: %s" js2-parse-errors)
              (insert-and-inherit "\n")
              (when (fboundp 'js2-indent-line)
                (funcall 'js2-indent-line))
              (return))
          (jss-prompt-submit prompt))))))

(defmethod jss-prompt-submit ((prompt jss-prompt))
  (lexical-let ((input-text (jss-prompt-input-text prompt)))
    (message "Submitting %s" input-text)
    (push input-text jss-prompt-input-history)
    (setf jss-prompt-input-history/last-inserted jss-prompt-input-history)
    (let ((inhibit-read-only t)
          (overlay (jss-prompt-input-overlay prompt)))
      (lexical-let ((current-buffer (current-buffer))
                    (prompt prompt))
        (add-text-properties (overlay-start overlay) (overlay-end overlay)
                             (list 'read-only t))
        (goto-char (overlay-end overlay))
        (let ((output-start (point)))
          (insert "// Evaluating...")
          (setf (jss-prompt-output-overlay prompt) (make-overlay output-start (point))))        

        (setf (jss-prompt-active-p prompt) nil)
        (jss-deferred-add-backs
         (funcall (jss-prompt-submit-function prompt) input-text)
         (lambda (remote-object)
           (with-current-buffer current-buffer
             (jss-prompt-update-output prompt remote-object)))
         (lambda (error)
           (with-current-buffer current-buffer
             (jss-prompt-update-output prompt error))))

        (goto-char (jss-prompt-end-of-output prompt))

        (goto-char (jss-prompt-start-of-input
                    (jss-insert-prompt (jss-prompt-submit-function prompt)
                                       :previous-prompt prompt)))))))

(defmethod jss-prompt-update-output ((prompt jss-prompt) remote-object)
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (jss-prompt-start-of-output prompt))
      (delete-region (point) (jss-prompt-end-of-output prompt))
      (jss-wrap-with-text-properties (list 'read-only t
                                           'jss-prompt prompt
                                           'jss-prompt-output t)
        (jss-insert-remote-value remote-object)))))

(defun jss-prompt-beginning-of-line (&optional n)
  (interactive "P")
  (beginning-of-line n)
  (when (get-text-property (point) 'jss-prompt-marker)
    (let ((past-prompt-marker (next-single-property-change (point)' jss-prompt-marker)))
      (if past-prompt-marker
          (goto-char (min (1+ past-prompt-marker) (point-max)))
        (goto-char (point-max))))))

(defmethod jss-prompt-set-input-text ((prompt jss-prompt) input-text)
  (let ((overlay (jss-prompt-input-overlay prompt)))
    (goto-char (overlay-start overlay))
    (insert input-text)
    (delete-region (point) (overlay-end overlay))))

(defsetf jss-prompt-input-text jss-prompt-set-input-text)

(defun jss-active-prompt ()
  (let* ((prompt (jss-prompt-current-prompt)))
    (unless prompt
      (error "No prompt at point."))
    (unless (jss-prompt-active-p prompt)
      (error "Current prompt is no longer active."))
    prompt))

(defun jss-prompt-insert-from-history (prompt history-delta first-time)
  (when (jss-prompt-history prompt)
    (if (null (jss-prompt-history-offset prompt))
        (setf (jss-prompt-history-offset prompt) first-time)
      (setf (jss-prompt-history-offset prompt) (+ history-delta (jss-prompt-history-offset prompt))))
    (setf (jss-prompt-history-offset prompt) (mod (jss-prompt-history-offset prompt)
                                                  (length (jss-prompt-history prompt)))
          (jss-prompt-input-text prompt) (nth (jss-prompt-history-offset prompt) (jss-prompt-history prompt)))))

(defun jss-prompt-insert-previous-input ()
  (interactive)
  (jss-prompt-insert-from-history (jss-active-prompt)
                                  +1
                                  0))

(defun jss-prompt-insert-next-input ()
  (interactive)
  (let ((prompt (jss-active-prompt)))
    (jss-prompt-insert-from-history prompt
                                    -1
                                    (1- (length  (jss-prompt-history prompt))))))

(provide 'jss-prompt)
