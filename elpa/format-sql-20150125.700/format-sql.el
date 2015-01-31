;;; format-sql.el --- Use format-sql to make your SQL readable in directly Emacs.

;; Copyright (C) 2015, Friedrich Paetzke <paetzke@fastmail.fm>

;; Author: Friedrich Paetzke <paetzke@fastmail.fm>
;; URL: https://github.com/paetzke/format-sql.el
;; Version: 20150125.700
;; X-Original-Version: 0.3

;;; Commentary:

;; Provides the `format-sql-buffer' command, which uses the external
;; "format-sql" tool to format SQL in the current buffer.

;; To format SQL in a buffer, use the following code:

;;   M-x format-sql-buffer RET

;; To format SQL in a region, use the following code:

;;   M-x format-sql-region RET

;;; Code:


(defgroup format-sql nil
  "Use format-sql to sort the imports in a Python buffer."
  :group 'convenience
  :prefix "format-sql-")


(defcustom format-sql-options nil
  "Options used for format-sql."
  :group 'format-sql
  :type '(repeat (string :tag "option")))


(defun format-sql-apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in format-sql-apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (- from line-offset 1))
                (setq line-offset (+ line-offset len))
                (kill-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in format-sql-apply-rcs-patch")))))))))


(defun format-sql-replace-region (filename)
  (delete-region (region-beginning) (region-end))
  (insert-file-contents filename))


(defun get-file-type ()
  (let ((my-file-type (file-name-extension buffer-file-name)))
    (if (string= my-file-type "py")
        "py"
      "sql"
      )
    )
  )


;;;###autoload
(defun format-sql (&optional only-on-region)
  "Uses the \"format-sql\" tool to reformat the current buffer."
  (interactive "r")
  (when (not (executable-find "format-sql"))
    (error "\"format-sql\" command not found. Install format-sql with \"pip install format-sql\""))

  (let* (
         (my-file-type (get-file-type))
         (tmpfile (make-temp-file "format-sql" nil (concat "." my-file-type)))
         (patchbuf (get-buffer-create "*format-sql patch*"))
         (errbuf (get-buffer-create "*format-sql Errors*"))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         )

    (with-current-buffer errbuf
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-current-buffer patchbuf
      (erase-buffer))

    (if (and only-on-region (use-region-p))
        (write-region (region-beginning) (region-end) tmpfile)
      (write-region nil nil tmpfile))

    (if (zerop (apply 'call-process "format-sql" nil errbuf nil
                      (append `(" " , tmpfile, " ") format-sql-options)))
        (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
            (progn
              (kill-buffer errbuf)
              (message "Buffer is already format-sqled"))

          (if only-on-region
              (format-sql-replace-region tmpfile)
            (format-sql-apply-rcs-patch patchbuf))

          (kill-buffer errbuf)
          (message "Applied format-sql."))
      (error "Could not apply format-sql. Check *format-sql Errors* for details"))
    (kill-buffer patchbuf)
    (delete-file tmpfile)))


;;;###autoload
(defun format-sql-region ()
  "Uses the \"format-sql\" tool to reformat the current region."
  (interactive)
  (format-sql t))


;;;###autoload
(defun format-sql-buffer ()
  "Uses the \"format-sql\" tool to reformat the current buffer."
  (interactive)
  (condition-case err (format-sql)
    (error (message "%s" (error-message-string err)))))


(provide 'format-sql)


;;; format-sql.el ends here
