;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'gnu-apl-util)
(require 'gnu-apl-network)

(defun gnu-apl-edit-function (name)
  "Open the function with the given name in a separate buffer.
After editing the function, use ‘gnu-apl-save-function’ to save
the function and set it in the running APL interpreter."
  (interactive (list (gnu-apl--choose-variable "Function name" :function (gnu-apl--name-at-point))))
  (gnu-apl--get-function name))

(defun gnu-apl--get-function (function-definition)
  (let ((function-name (gnu-apl--parse-function-header function-definition)))
    (unless function-name
      (error "Unable to parse function definition: %s" function-definition))
    (with-current-buffer (gnu-apl--get-interactive-session)
      (gnu-apl--send-network-command (concat "fn:" function-name))
      (let* ((reply (gnu-apl--read-network-reply-block))
             (content (cond ((string= (car reply) "function-content")
                             (cdr reply))
                            ((string= (car reply) "undefined")
                             (list function-definition))
                            (t
                             (error "Not an editable function: %s" function-name)))))
        (gnu-apl--open-function-editor-with-timer content)))))

(defun gnu-apl-interactive-send-buffer ()
  "Send the entire content of the current buffer to the active
GNU APL interpreter."
  (interactive)
  (gnu-apl-interactive-send-string (buffer-string) buffer-file-name 0))

(defun gnu-apl-interactive-send-region (start end)
  "Send the region to the active GNU APL interpreter."
  (interactive "r")
  (gnu-apl-interactive-send-string (buffer-substring start end)
                                   buffer-file-name (1- (gnu-apl--current-line-number (min start end))))
  (message "Region sent to APL"))

(defun gnu-apl--function-definition-to-list (content)
  "Given a function definition as returned by ⌷CR 'function',
split the definition into lines, where the first line has
normalised to a normal function definition form."
  (let ((rows (split-string content "\r?\n")))
    (let ((definition (gnu-apl--trim-spaces (car rows)))
          (body (cdr rows)))
      (unless (string= (subseq definition 0 1) "∇")
        (error "When splitting function, header does not start with function definition"))
      (cons (subseq definition 1) body))))

(defun gnu-apl--make-tag (filename line)
  "Creates a tag appropriate for sending to the APL interpreter
using the def command."
  (format "%s&3A;%d" filename line))

(defun gnu-apl-interactive-send-current-function ()
  "Send the function definition at point to the running GNU APL interpreter.
The block is bounded by a function definition of the form
∇definition on the top, and ending with a single ∇ character."
  (interactive)

  (save-excursion
    (beginning-of-line)
    (let ((start (loop for line = (gnu-apl--trim-spaces (gnu-apl--current-line-string))
                       when (gnu-apl--full-function-definition-p line t)
                       return (point)
                       when (plusp (forward-line -1))
                       return nil)))
      (unless start
        (user-error "Can't find function definition above cursor"))

      (unless (zerop (forward-line 1))
        (user-error "No end marker found"))

      (let ((end (loop for line = (gnu-apl--trim-trailing-newline
                                   (gnu-apl--trim-spaces (gnu-apl--current-line-string)))
                       when (string= line "∇")
                       return (progn (forward-line -1) (end-of-line) (point))
                       when (plusp (forward-line 1))
                       return nil)))
        (unless end
          (user-error "No end marker found"))
        (let ((overlay (make-overlay start end)))
          (overlay-put overlay 'face '(background-color . "green"))
          (run-at-time "0.5 sec" nil #'(lambda () (delete-overlay overlay))))
        (let ((function-lines (gnu-apl--function-definition-to-list (buffer-substring start end))))
          (gnu-apl--send-si-and-send-new-function function-lines
                                                  (gnu-apl--make-tag (buffer-file-name)
                                                                     (line-number-at-pos start))))))))

(defun gnu-apl--send-new-function (content &optional tag)
  (gnu-apl--send-network-command (concat "def" (if tag (concat ":" tag) "")))
  (gnu-apl--send-block content)
  (let ((return-data (gnu-apl--read-network-reply-block)))
    (cond ((null return-data)
           (error "Got nil result from def command"))
          ((string= (car return-data) "function defined")
           t)
          ((string= (car return-data) "error")
           (if (string= (second return-data) "parse error")
               (let ((error-msg (third return-data))
                     (line (string-to-number (fourth return-data))))
                 (gnu-apl--move-to-line line)
                 (let ((overlay (make-overlay (save-excursion
                                                (beginning-of-line)
                                                (point))
                                              (save-excursion
                                                (end-of-line)
                                                (point)))))
                   (overlay-put overlay 'face '(background-color . "yellow"))
                   (run-at-time "0.5 sec" nil #'(lambda () (delete-overlay overlay))))
                 (message "Error on line %d: %s" line error-msg)
                 nil)
             (error "Unexpected error: %s" (second return-data))))
          (t
           (gnu-apl--display-error-buffer (format "Error second function: %s" (car content))
                                          (cdr return-data))
           nil))))

(defun gnu-apl--send-si-and-send-new-function (parts &optional tag)
  "Send an )SI request that should be checked against the current
function being sent. Returns non-nil if the function was sent
successfully."
  (let* ((function-header (gnu-apl--trim-spaces (car parts)))
         (function-name (gnu-apl--parse-function-header function-header)))
    (unless function-name
      (error "Unable to parse function header"))
    (gnu-apl--send-network-command "si")
    (let ((reply (gnu-apl--read-network-reply-block)))
      (if (cl-find function-name reply :test #'string=)
          (ecase gnu-apl-redefine-function-when-in-use-action
            (error (error "Function already on the )SI stack"))
            (clear (gnu-apl--send-network-command "sic")
                   (gnu-apl--send-new-function parts tag))
            (ask (when (y-or-n-p "Function already on )SI stack. Clear )SI stack? ")
                   (gnu-apl--send-network-command "sic")
                   (gnu-apl--send-new-function parts tag)
                   t)))
        (gnu-apl--send-new-function parts tag)))))

(defun gnu-apl--remove-final-endfn (strings)
  "If the last element is ∇, return a new list with that element removed, else return the original list."
  (if (and strings
           (equal (gnu-apl--trim-spaces (car (last strings))) "∇"))
      (butlast strings)
    strings))

(defun gnu-apl-save-function ()
  "Save the currently edited function."
  (interactive)
  (goto-char (point-min))
  (let ((definition (gnu-apl--trim-spaces (gnu-apl--trim-trailing-newline (gnu-apl--current-line-string)))))
    (unless (string= (subseq definition 0 1) "∇")
      (user-error "Function header does not start with function definition symbol"))
    (unless (zerop (forward-line))
      (user-error "Empty function definition"))
    (let* ((function-header (subseq definition 1))
           (function-name (gnu-apl--parse-function-header function-header)))
      (unless function-name
        (user-error "Illegal function header"))

      (let* ((buffer-content (gnu-apl--trim-trailing-newline (buffer-substring (point) (point-max))))
             (content (list* function-header
                             (gnu-apl--remove-final-endfn (split-string buffer-content "\r?\n")))))

        (when (gnu-apl--send-si-and-send-new-function content)
          (let ((window-configuration (if (boundp 'gnu-apl-window-configuration)
                                          gnu-apl-window-configuration
                                        nil)))
            (kill-buffer (current-buffer))
            (when window-configuration
              (set-window-configuration window-configuration))))))))

(define-minor-mode gnu-apl-interactive-edit-mode
  "Minor mode for editing functions in the GNU APL function editor."
  nil
  " APLFunction"
  (list (cons (kbd "C-c C-c") 'gnu-apl-save-function))
  :group 'gnu-apl)

(defun gnu-apl--open-function-editor-with-timer (lines)
  (run-at-time "0 sec" nil #'(lambda () (gnu-apl-open-external-function-buffer lines))))

(defun gnu-apl-open-external-function-buffer (lines)
  "Open a buffer for editing the function whose is a list of
strings in LINES. The input should not contain any nabla
characters."
  (let ((window-configuration (current-window-configuration))
        (buffer (get-buffer-create "*gnu-apl edit function*")))
    (pop-to-buffer buffer)
    (delete-region (point-min) (point-max))
    (insert "∇")
    (dolist (line lines)
      (insert (gnu-apl--trim-spaces line nil t))
      (insert "\n"))
    (goto-char (point-min))
    (forward-line 1)
    (gnu-apl-mode)
    (gnu-apl-interactive-edit-mode 1)
    (setq-local gnu-apl-window-configuration window-configuration)
    (message "To save the buffer, use M-x gnu-apl-save-function (C-c C-c)")))

(defun gnu-apl--choose-variable (prompt &optional type default-value)
  (gnu-apl--send-network-command (concat "variables"
                                         (ecase type
                                           (nil "")
                                           (:function ":function")
                                           (:variable ":variable"))))
  (let ((results (gnu-apl--read-network-reply-block)))
    (let ((initial (if (and default-value
                            (cl-find default-value results :test #'string=))
                       default-value
                     nil)))
      (let ((response (completing-read (if initial
                                           (format "%s (default \"%s\"): " prompt initial)
                                         (format "%s: " prompt))
                                       results ; options
                                       nil ; predicate
                                       nil ; require-match
                                       nil ; initial-input
                                       'gnu-apl-variables-history  ; hist
                                       initial ; def
                                       t ; inherit input method
                                       )))
        (let ((trimmed (gnu-apl--trim-spaces response)))
          (when (string= trimmed "")
            (user-error "Illegal variable"))
          trimmed)))))

(defun gnu-apl--display-error-buffer (title messages)
  "Open an error buffer with the given title and content"
  (let ((buffer (get-buffer-create "*gnu-apl error*")))
    (pop-to-buffer buffer)
    (delete-region (point-min) (point-max))
    (insert title)
    (insert "\n")
    (dolist (row messages)
      (insert row)
      (insert "\n"))
    (goto-char (point-min))
    (read-only-mode 1)))

(provide 'gnu-apl-editor)
