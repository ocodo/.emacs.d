;;; ruby-dev-utils.el — Common utilities functions for ruby-dev.el

(defun ruby-dev-open-source (file line)
  "Attempts to open a file and to jump at a certain line within it.

If the file does not exist, instead of trying create it, a message gets printed."
  (if (not (file-exists-p file))
      (message "File does not exist: %s" file)
    (let ((buffer (find-file file)))
      (with-current-buffer buffer
        (goto-line line)))))

(defun ruby-dev-highlight-code (code &optional mode)
"Returns a syntax highlighhted version of CODE in a given major mode.

If MODE is nil, `ruby-mode' is used."
  (unless mode (setq mode 'ruby-mode))
  (with-temp-buffer
    (funcall mode)
    (insert code)
    (font-lock-fontify-buffer)
    (buffer-string)))

(defun ruby-dev-highlight-code-blocks-in (string &optional mode)
  "Syntax highlight all the code blocks that are found in string.

A code block is a series of line that either start with 3 spaces or are empty.
MODE is the major mode to use for highlighting them. It defaults to
`ruby-mode'."
  (let ((index 0))
    (loop while (string-match "^\\(   +.*\\|\n\\)+" string index) do
      (let ((start (nth 0 (match-data)))
            (end   (nth 1 (match-data))))
        (set-text-properties start end nil string)
        (setq string (concat (substring string 0 start)
                             (ruby-dev-highlight-code
                              (substring string start end)
                              mode)
                             (substring string end)))
        (setq index end))))
  string)

(defun ruby-dev-join-list (list &optional separator)
  "Joins all the elements of a list of strings with a given separator.

If SEPARATOR is nill, a comma followed by a space is used."
  (unless separator (setq separator ", "))
  (if (null list) ""
    (let ((s ""))
      (maplist (lambda (cons)
                 (setq s (concat s (if (null (cdr cons)) (car cons)
                                     (concat (car cons) separator)))))
               list)
      s)))

(defun ruby-dev-group-by (criterion array)
  "Returns a hash containing list of elements grouped according
to the return value of the CRITERION function.

The lists keep the order of appearance in the original array."
  (let ((hash (make-hash-table :test 'equal)))
    (loop for e across array do
      (let ((v (funcall criterion e)))
        (puthash v (append (gethash v hash) (list e)) hash)))
    hash))

(provide 'ruby-dev-utils)
