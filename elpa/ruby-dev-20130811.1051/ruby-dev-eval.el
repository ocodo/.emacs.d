
;;; ruby-dev-eval.el — Functions to evaluate Ruby code.

(require 'ruby-dev-core)
(require 'ruby-dev-error)

;;;###autoload
(defun ruby-dev-eval-string (code &optional filename line)
  "Evaluates an arbitrary string of ruby code and writes to messages.

Optionally, you can specify a FILENAME (__eval__ by default) and a LINE number
 (0 by default)."
  (interactive "sEval Ruby: ")
  (ruby-dev-ensure)
  (ruby-dev-send-request "eval" :code code
                         :filename (or filename "__eval__")
                         :line (or line 0))
  (let ((response (ruby-dev-read-response)))
    (with-ruby-dev-data (success result) response
      (if (eql success :json-false) (ruby-dev-show-error response)
        (message "%s" result)))))

;;;###autoload
(defun ruby-dev-eval-string-and-kill (code &optional filename line)
  "Evaluates an arbitrary string of ruby code and adds it to the kill ring.

Optionally, you can specify a FILENAME (__eval__ by default) and a LINE number
 (0 by default)."
  (interactive "sEval Ruby: ")
  (kill-new (ruby-dev-eval-string code filename line)))

(defun ruby-dev-find-filename ()
  "Attempts to find the filename to use for code evaluated from the current buffer.

If the `buffer-file-name' is set, it is used; otherwise, it defaluts to __eval__."
  (or (buffer-file-name) "__eval__"))

(defun ruby-dev-eval-region-common (start end fct &optional filename line)
  (ruby-dev-ensure)
  (unless filename (setq filename (ruby-dev-find-filename)))
  (unless line (setq line (line-number-at-pos start)))
  (funcall fct (buffer-substring start end) filename line))

;;;###autoload
(defun ruby-dev-eval-region (start end &optional filename line)
  "Tries to evaluate a region of code.

FILENAME and LINE are normally guessed from the buffer and the location of START,
but they can be specified explicitly."
  (interactive "r")
  (ruby-dev-eval-region-common start end 'ruby-dev-eval-string filename line))

;;;###autoload
(defun ruby-dev-eval-region-and-kill (start end &optional filename line)
  "Tries to evaluate a region of code and adds the result to the kill ring.

FILENAME and LINE are normally guessed from the buffer and the location of START,
but they can be specified explicitly."
  (interactive "r")
  (ruby-dev-eval-region-common start end 'ruby-dev-eval-string-and-kill filename line))

(defun ruby-dev-eval-last-sexp-common (fct &optional filename line)
  (ruby-dev-ensure)
  (let (start end)
    (save-excursion
      (ruby-backward-sexp)
      (setq start (point))
      (ruby-forward-sexp)
      (setq end (point)))
    (funcall fct start end filename line)))

;;;###autoload
(defun ruby-dev-eval-last-sexp (&optional filename line)
  "Evaluates the last 'sexp' in code.

Sexps are found using movement functions from `ruby-mode'."
  (interactive)
  (ruby-dev-eval-last-sexp-common 'ruby-dev-eval-region filename line))

;;;###autoload
(defun ruby-dev-eval-last-sexp-and-kill (&optional filename line)
  "Evaluates the last 'sexp' in code and adds it to the kill ring.

Sexps are found using movement functions from `ruby-mode'."
  (interactive)
  (ruby-dev-eval-last-sexp-common 'ruby-dev-eval-region-and-kill filename line))

(put 'ruby-dev-defun 'beginning-op 'ruby-beginning-of-defun)
(put 'ruby-dev-defun 'end-op       'ruby-end-of-defun)
(put 'ruby-dev-defun 'forward-op   'ruby-end-of-defun)

(defun ruby-dev-eval-defun-common (fct &optional filename line)
  (ruby-dev-ensure)
  (let ((bounds (bounds-of-thing-at-point 'ruby-dev-defun)))
    (when bounds
      (funcall fct (car bounds) (cdr bounds) filename line))))

;;;###autoload
(defun ruby-dev-eval-defun (&optional filename line)
  "Evaluates the current top-level expression at point.

This is done using `ruby-beginnning-of-defun' and `ruby-end-of-defun'."
  (interactive)
  (ruby-dev-eval-defun-common 'ruby-dev-eval-region filename line))

;;;###autoload
(defun ruby-dev-eval-defun-and-kill (&optional filename line)
  "Evaluates the current top-level expression at point and adds it to the kill ring.

This is done using `ruby-beginnning-of-defun' and `ruby-end-of-defun'."
  (interactive)
  (ruby-dev-eval-defun-common 'ruby-dev-eval-region-and-kill filename line))

(defun ruby-dev-eval-buffer-common (fct &optional filename)
  (ruby-dev-ensure)
  (unless filename (setq filename (ruby-dev-find-filename)))
  (funcall fct (buffer-string) filename 1))

;;;###autoload
(defun ruby-dev-eval-buffer (&optional filename)
  "Evaluates the whole buffer.

An explicit FILENAME can be specified, otherwise __eval__ is used."
  (interactive)
  (ruby-dev-eval-buffer-common 'ruby-dev-eval-string filename))

;;;###autoload
(defun ruby-dev-eval-buffer-and-kill (&optional filename)
  "Evaluates the whole buffer.

An explicit FILENAME can be specified, otherwise __eval__ is used."
  (interactive)
  (ruby-dev-eval-buffer-common 'ruby-dev-eval-string-and-kill filename))

(provide 'ruby-dev-eval)
