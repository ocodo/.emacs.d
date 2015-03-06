(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (insert (format "%s" (eval (read (current-kill 0)))))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or `beginning-of-line'."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key (kbd "C-a") 'smart-beginning-of-line)

;; Originally swiped from rejeep's emacs.d rejeep-defuns.el.
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and (region-active-p) (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if (region-active-p)
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(provide 'quick-funcs)
