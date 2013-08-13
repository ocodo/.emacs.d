;;; ruby-dev-error.el — Module to display Ruby exceptions for ruby-dev.el

(require 'ruby-dev-core)
(require 'ruby-dev-utils)

(defvar ruby-dev-error-buffer nil
  "Buffer used to show errors to the user.")

(defvar ruby-dev-error-backtrace-line nil)

(defun ruby-dev-create-error-buffer ()
  "Creates a new buffer to use to display errors, and returns it.

This buffer runs using `ruby-dev-error-mode'."
  (setq ruby-dev-error-buffer (generate-new-buffer "*ruby-dev error*"))
  (with-current-buffer ruby-dev-error-buffer
    (ruby-dev-error-mode))
  ruby-dev-error-buffer)

(defun ruby-dev-open-backtrace-entry (entry)
  "Attemps to open a link that's a backtrace entry.

This assumes the string (ENTRY) starts with the following form:
'FILENAME:LINE:'."
  (when (= 0 (string-match "\\(.+\\):\\([0-9]+\\):" entry))
    (destructuring-bind (start end
                         file-start file-end
                         line-start line-end) (match-data)
      (let ((file (subseq entry file-start file-end))
            (line (subseq entry line-start line-end)))
        (ruby-dev-open-source file (string-to-number line))))))

(defun ruby-dev-write-error (response)
  "Writes an error into the error buffer.

This displays the error message at the top, followed by its backtrace."
  (with-current-buffer ruby-dev-error-buffer
    (toggle-read-only -1)
    (erase-buffer)
    (with-ruby-dev-data (success error backtrace) response
      (if (eql success t)
          (insert "No error. I must have done something wrong in ruby-dev.el, sorry.")
        (insert error "\n\nBacktrace: \n")
        (setq ruby-dev-error-backtrace-line (line-number-at-pos))
        (dotimes (i (length backtrace))
          (lexical-let ((entry (aref backtrace i)))
            (insert-text-button entry
                                'action (lambda (b)
                                          (ruby-dev-open-backtrace-entry entry))))
            (insert "\n")))
      (toggle-read-only 1))))

(defun ruby-dev-show-error (response)
  "Shows the error signaled by RESPONSE.

RESPONSE is a response where success should be false (otherwise, this will print
a usual 'Error: success' message).

This returns nil, for convenience."
  (unless (and ruby-dev-error-buffer (buffer-live-p ruby-dev-error-buffer))
    (ruby-dev-create-error-buffer))
  (ruby-dev-write-error response)
  (switch-to-buffer-other-window ruby-dev-error-buffer)
  (goto-line ruby-dev-error-backtrace-line)
  nil)

;;;###autoload
(defvar ruby-dev-error-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "TAB" 'forward-button)
    (define-key map "c" 'delete-window)
    map)
  "Keymap for `ruby-dev-error-mode'.")

;;;###autoload
(define-derived-mode ruby-dev-error-mode special-mode "Ruby-Error"
  "Major mode for viewing Ruby exceptions and jumping through their backtrace, in
the buffer shown by `ruby-dev-show-error'.

Commands:
\\{ruby-dev-error-mode-map}"
  (toggle-read-only 1)
  (set (make-local-variable 'ruby-dev-error-backtrace-line) nil))

(provide 'ruby-dev-error)
