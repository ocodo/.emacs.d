;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'gnu-apl-util)
(require 'gnu-apl-network)
(require 'ses)

(defun gnu-apl--string-to-apl-expression (string)
  "Escape quotes in an APL string. If the string contains
non-printable characters, generate a full expression. For now,
non-printable means CR or NL characters."
  (if (string-match "[\n\r]" string)
      ;; The string must be formatted as an APL expression
      (with-output-to-string
        (princ "(⎕UCS")
        (loop for char across string
              do (princ (format " %d" char)))
        (princ ")"))
    ;; We can simply use plain string
    (with-output-to-string
      (princ "'")
      (loop for char across string
            do (if (= char ?\')
                   (princ "''")
                 (princ (char-to-string char))))
      (princ "'"))))

(defun gnu-apl-edit-variable (name)
  "Open the variable editor for the APL variable NAME.
Currently only two-dimensional arrays of depth 1 are supported.
These variables will be edited in a spreadsheet. After editing,
press C-c C-c to update the variable in the active interpreter."
  (interactive (list (gnu-apl--choose-variable "Variable" :variable (gnu-apl--name-at-point))))
  (gnu-apl--send-network-command (concat "getvar:" name))
  (let ((result (gnu-apl--read-network-reply-block)))
    (unless (string= (car result) "content")
      (error "Unable to read variable. Response: %s" (car result)))
    (let ((value (car (read-from-string (apply #'concat (cdr result))))))
      (cond ((and (listp value)
                  (eq (car value) :vector)
                  (= (length (cadr value)) 2))
             (gnu-apl--edit-value-in-spreadsheet name value))
            (t
             (error "Unable to edit values of this type"))))))

(defun gnu-apl-spreadsheet-send-to-variable (varname)
  "Send the content of the spreadsheet to the variable VARNAME in
the active interpreter."
  (interactive (list (or (and (boundp 'gnu-apl-var-name) gnu-apl-var-name)
                         (read-from-minibuffer "Variable name: "))))
  (let* ((variable-name varname)
         (buffer (gnu-apl--get-interactive-session))
         (s (gnu-apl-make-array-loading-instructions variable-name))
         (lines (split-string s "\n")))
    (dolist (line lines)
      (gnu-apl--send buffer line))
    (let ((window-configuration (if (boundp 'gnu-apl-window-configuration)
                                    gnu-apl-window-configuration
                                  nil)))
      (bury-buffer)
      (when window-configuration
        (set-window-configuration window-configuration)))
    (message "Variable %s updated" variable-name)))

(define-minor-mode gnu-apl-spreadsheet-mode
  "A variation of ‘ses-mode’ to be used for editing APL matrices."
  nil
  " ≡"
  (list (cons (kbd "C-c C-c") 'gnu-apl-spreadsheet-send-to-variable)
        (cons [menu-bar gnu-apl] (cons "APL" (make-sparse-keymap "APL")))
        (cons [menu-bar gnu-apl send-this-document] '("Send document" . gnu-apl-spreadsheet-send-this-document))
        (cons [menu-bar gnu-apl copy-spreadsheet-as-apl-function] '("Copy document as function". gnu-apl-copy-spreadsheet-to-kill-ring)))
  :group 'gnu-apl)

(defun gnu-apl--edit-value-in-spreadsheet (backend-variable-name value)
  (let ((window-configuration (current-window-configuration)))
    (unless (and (eq (car value) :vector)
                 (= (length (cadr value)) 2))
      (error "Only two-dimensional arrays can be edited"))
    (let* ((buffer-name (format "*gnu-apl array %s*" backend-variable-name))
           (buffer (get-buffer buffer-name)))
      (when buffer
        (kill-buffer buffer))
      (setq buffer (get-buffer-create buffer-name))
      (pop-to-buffer buffer)
      (ses-mode)
      (gnu-apl-spreadsheet-mode)
      (let* ((dimensions (cadr value))
             (rows (car dimensions))
             (cols (cadr dimensions)))
        (when (> rows 1)
          (ses-insert-row (1- rows)))
        (when (> cols 1)
          (ses-insert-column (1- cols)))
        (loop for row-index from 0 below rows
              for row-values in (caddr value)
              do (loop for col-index from 0 below cols
                       for col-content in row-values
                       do (let ((v (etypecase col-content
                                     (integer col-content)
                                     (float col-content)
                                     (string col-content)
                                     (list (etypecase (car col-content)
                                             (symbol (case (car col-content)
                                                       (:unicode (char-to-string (cadr col-content)))
                                                       (t (format "!%s" (car col-content)))))
                                             (list "!list")))
                                     (t (error "Illegal cell content: %S" col-content)))))
                            (ses-edit-cell row-index col-index v))))
        (setq-local gnu-apl-var-name backend-variable-name))
      (setq-local gnu-apl-window-configuration window-configuration)
      (message "To save the buffer, use M-x gnu-apl-spreadsheet-send-this-document (C-c C-c)"))))

(defun gnu-apl-copy-spreadsheet-to-kill-ring (varname)
  "Copy APL code representing the data in the active SES
spreadhsheet into the kill ring. When executed, this sets the
value of VARNAME to the content of the spreadsheet."
  (interactive "sVariable name: ")
  (kill-new (concat (gnu-apl-make-function-from-spreadsheet-data varname) "\n")))

(defun gnu-apl-make-function-from-spreadsheet-data (varname)
  "Return the APL code that sets VARNAME to an array with the same
values as the spreadsheet in the current buffer."
  (concat (gnu-apl-make-array-loading-instructions varname)))

(defun gnu-apl-make-array-loading-instructions (var-name)
  "Return APL instructions that sets variable VAR-NAME to the
content of the spreadsheet in this buffer."
  (with-output-to-string
    (let ((rows ses--numrows)
          (cols ses--numcols))
      (princ (format "%s←%d⍴0\n" var-name (* rows cols)))
      (loop for row from 0 below rows
            do (progn
                 (princ (format "%s[%d+⍳%d]←" var-name (* row cols) cols))
                 (loop for col from 0 below cols
                       do (let ((item (ses-cell-value row col)))
                            (typecase item
                              (null (princ "(0⍴0)"))
                              (number (if (minusp item) (princ (format "¯%f" (- item))) (princ item)))
                              (string (princ (gnu-apl--string-to-apl-expression item)))
                              (t (ses-goto-print row col) (error "Invalid content in cell %d,%d" row col)))
                            (if (< col (1- cols))
                                (princ " ")
                              (princ "\n"))))))
      (princ (format "%s←(%d %d)⍴%s" var-name rows cols var-name)))))

(provide 'gnu-apl-spreadsheet)
