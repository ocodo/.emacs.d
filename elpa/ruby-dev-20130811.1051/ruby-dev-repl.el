;;; ruby-dev-repl.el — A Ruby REPL

(require 'ruby-dev-core)
(require 'ruby-dev-utils)
(require 'ruby-dev-error)

(require 'ansi-color)

;;;###autoload
(defgroup ruby-dev-repl nil
  "Module to handle the Ruby REPL"
  :group 'ruby-dev)

(defface ruby-dev-repl-prompt-face
  '((t (:inherit default
                 :underline t)))
  "Face of the prompt in the REPL."
  :group 'ruby-dev-faces
  :group 'ruby-dev-repl)

(defface ruby-dev-repl-output-face
  '((t (:inherit default)))
  "Face of the result of commands in the REPL."
  :group 'ruby-dev-faces
  :group 'ruby-dev-repl)

(defvar ruby-dev-repls (make-hash-table :test 'equal)
  "Buffers for each REPL.")

(defvar ruby-dev-repl-line-location nil
  "Position of the beginning of the current line for the REPL.")

(defvar ruby-dev-repl-id nil
  "ID of the REPL used by this buffer, so that it can send line back to it.")

(defvar ruby-dev-repl-history nil
  "History of the REPL.")

(defvar ruby-dev-repl-modified-history nil
  "Copy of the history that gets iterated over by the REPL.

This is done so that when the user presses up, changes something, then down and
up again, his changes are preserved.")

(defvar ruby-dev-repl-history-pos 0
  "Position in the history.")

(defun ruby-dev-repl-get (id)
  "Returns the buffer REPL for a certain id."
  (let ((buffer (gethash id ruby-dev-repls)))
    (when (and buffer (buffer-live-p buffer))
      buffer)))

(defun ruby-dev-handle-repl-instruction (response)
  "Handler for the results of REPL-related commands."
  (with-ruby-dev-data (type repl-id success string prompt) response
    (cond
     ((eql success :json-false) (ruby-dev-show-error response))
     ((equal type "write")
      (with-current-buffer (ruby-dev-repl-get repl-id)
        (ruby-dev-repl-write-response string 'ruby-dev-repl-output-face)))
     ((equal type "read")
      (with-current-buffer (ruby-dev-repl-get repl-id)
        (ruby-dev-repl-start-read prompt))))))

(add-to-list 'ruby-dev-special-handlers '(repl-id . ruby-dev-handle-repl-instruction))

(defun ruby-dev-handle-repl (id &optional argument)
  "Starts receiving input form the REPL called ID, until it asks us for a new line.

Except for the first call, this should always be called with ARGUMENT, the line to
send to the process.

Results are handled asynchronously by `ruby-dev-handle-repl-instruction'."
  (ruby-dev-send-request "repl-handle" :id id :argument argument))

(defun ruby-dev-create-repl-buffer (id)
  "Creates a buffer running `ruby-dev-repl-mode', using the right mode."
  (let ((buffer (generate-new-buffer (concat "*ruby-dev repl:" id "*"))))
    (with-current-buffer buffer
      (ruby-dev-repl-mode)
      (setq ruby-dev-repl-id id))
    buffer))

;;;###autoload
(defun ruby-dev-start-repl (id object)
  "Starts the REPL.

ID is a unique identifier used for communication with the server.
OBJECT is a ruby expression, used to start pry into."
  (interactive
   (reverse
    (list
     (read-string "REPL in (default: TOPLEVEL_BINDING): " nil nil "TOPLEVEL_BINDING")
     (read-string "REPL id (default: main): " nil nil "main"))))
  (ruby-dev-ensure)
  (let ((buffer (ruby-dev-create-repl-buffer id)))
    (puthash id buffer ruby-dev-repls)
    (ruby-dev-send-request "repl-start" :id id :object object)
    (let ((response (ruby-dev-read-response)))
      (with-ruby-dev-data (success) response
        (if (eql success :json-false)
            (progn
              (ruby-dev-show-error response)
              (kill-buffer buffer)
              (remhash id ruby-dev-repls))
          (ruby-dev-handle-repl id)
          (switch-to-buffer-other-window buffer)
          (move-end-of-line nil))))))

;;;###autoload
(defun ruby-dev-start-main-repl ()
  "Starts a top-level REPL with main as its identifier.

If there already is such a REPL, just switch buffer"
  (interactive)
  (ruby-dev-ensure)
  (let ((buffer (ruby-dev-repl-get "main")))
    (if buffer (switch-to-buffer-other-window buffer)
      (ruby-dev-start-repl "main" "TOPLEVEL_BINDING"))))

(defun ruby-dev-repl-current-line (&key without-properties)
  "Returns the current line of input.

If WITHOUT-PROPERTIES is true, text properties won't be fetched."
  (funcall (if without-properties
               'buffer-substring-no-properties
             'buffer-substring)
           ruby-dev-repl-line-location (point-max)))

(defun ruby-dev-repl-start-read (prompt)
  "Prompts the user to enter a new line in the REPL."
  (ruby-dev-repl-write-response prompt 'ruby-dev-repl-prompt-face)
  (toggle-read-only -1)
  (setq ruby-dev-repl-line-location (point)))

(defun ruby-dev-repl-send-input ()
  "Sends the current line of input to the server."
  (interactive)
  (unless buffer-read-only
    (let ((line (ruby-dev-repl-current-line :without-properties t)))
      (goto-char (point-max))
      (insert "\n")
      (add-text-properties (1- (point)) (point) '(rear-nonsticky t))
      (add-text-properties ruby-dev-repl-line-location (point) '(read-only t
                                                                 front-sticky t
                                                                 inhibit-line-move-field-capture
                                                                 t))
      (ruby-dev-handle-repl ruby-dev-repl-id line)
      (ruby-dev-repl-store-line line))
    (setq ruby-dev-repl-line-location nil)
    (toggle-read-only 1)))

(defun ruby-dev-repl-write-response (string face)
  "Writes a response from the server to the buffer.

FACE is the face to use for that (this can be used to distinguish the prompt
from the output of a command)."

  ;; FIXME: Better color support
  ;;   - Don't highlight strings in REPL output
  ;;   - Do highlight ansi color codes in it
  (unless (zerop (length string))
    (toggle-read-only -1)
    (let ((colored-string (ansi-color-apply string)))
      (add-text-properties 0 (length colored-string)
                           `(font-lock-face ,face)
                           colored-string)
      (add-text-properties 0 (length colored-string) '(read-only t
                                                       front-sticky t
                                                       field
                                                       inhibit-line-move-field-capture)
                           colored-string)
      (add-text-properties (1- (length colored-string)) (length colored-string)
                           '(rear-nonsticky t)
                           colored-string)
      (insert colored-string))
    (toggle-read-only 1)))

(defun ruby-dev-repl-previous ()
  "Moves to the previous history line."
  (interactive)
  (ruby-dev-repl-goto-history-line (1+ ruby-dev-repl-history-pos)))

(defun ruby-dev-repl-next ()
  "Moves to the next history line."
  (interactive)
  (ruby-dev-repl-goto-history-line (1- ruby-dev-repl-history-pos)))

(defun ruby-dev-repl-goto-history-line (id)
  "Go to the IDth history line.

If ID is out of bounds, nothing happens."
  (unless (or buffer-read-only
              (<  id 0)
              (>= id (length ruby-dev-repl-modified-history)))
    (ruby-dev-repl-set-line (ruby-dev-repl-current-line :without-properties t))
    (setq ruby-dev-repl-history-pos id)
    (ruby-dev-repl-change-line (nth id ruby-dev-repl-modified-history))))

(defun ruby-dev-repl-store-line (line)
  "Stores LINE in history and resets modified history and history position."
  (push line ruby-dev-repl-history)
  (setq ruby-dev-repl-modified-history
        (copy-list ruby-dev-repl-history))
  (push "" ruby-dev-repl-modified-history)
  (setq ruby-dev-repl-history-pos 0))

(defun ruby-dev-repl-set-line (value)
  "Changes the select item in the modified history to VALUE."
  (setf (nth ruby-dev-repl-history-pos ruby-dev-repl-modified-history)
        value))

(defun ruby-dev-repl-change-line (value)
  "Replaces the current line in the buffer with with VALUE."
  (delete-region ruby-dev-repl-line-location (point-max))
  (insert value)
  (goto-char (point-max)))

;;;###autoload
(defvar ruby-dev-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'ruby-dev-repl-send-input)
    (define-key map "\ep" 'ruby-dev-repl-previous)
    (define-key map "\en" 'ruby-dev-repl-next)
    (define-key map (kbd "<C-up>") 'ruby-dev-repl-previous)
    (define-key map (kbd "<C-down>") 'ruby-dev-repl-next)
    map)
  "Key bindings for `ruby-dev-repl-mode'.")

;;;###autoload
(define-derived-mode ruby-dev-repl-mode fundamental-mode "Ruby-REPL"
  "Major mode for interacting with a Ruby REPL.

\\{ruby-dev-repl-mode-map}"
  (toggle-read-only 1)
  (set (make-local-variable 'ruby-dev-repl-line-location) nil)
  (set (make-local-variable 'ruby-dev-repl-id) nil)
  (set (make-local-variable 'ruby-dev-repl-history) nil)
  (set (make-local-variable 'ruby-dev-repl-modified-history) (list ""))
  (set (make-local-variable 'ruby-dev-repl-history-pos) 0)

  ;; Code to handle syntax highlighting, copied from ruby-mode.
  (set (make-local-variable 'font-lock-defaults)
       '((ruby-font-lock-keywords) nil nil))
  (set (make-local-variable 'font-lock-keywords)
       ruby-font-lock-keywords)
  (set (make-local-variable 'font-lock-syntax-table)
       ruby-font-lock-syntax-table)
  (if (eval-when-compile (fboundp 'syntax-propertize-rules))
      (set (make-local-variable 'syntax-propertize-function)
           #'ruby-syntax-propertize-function)
    (set (make-local-variable 'font-lock-syntactic-keywords)
         ruby-font-lock-syntactic-keywords)))

;;; FIXME: code completion?

(provide 'ruby-dev-repl)
