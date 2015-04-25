;;; format-sql.el --- Use format-sql to make your SQL readable in directly Emacs.

;; Copyright (C) 2015, Friedrich Paetzke <paetzke@fastmail.fm>

;; Author: Friedrich Paetzke <paetzke@fastmail.fm>
;; URL: https://github.com/paetzke/format-sql.el
;; Package-Version: 20150422.633
;; Version: 0.4

;;; Commentary:

;; Provides commands, which use the external `format-sql' tool to
;; format SQL in the current buffer.

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


(defun format-sql--call-executable (errbuf file)
  (zerop (apply 'call-process "format-sql" nil errbuf nil
                (append `(" " , file, " ") format-sql-options))))


(defun get-file-type ()
  (let ((my-file-type (file-name-extension buffer-file-name)))
    (if (string= my-file-type "py")
        "py"
      "sql")))


(defun format-sql--call (only-on-region)
  "Uses the \"format-sql\" tool to reformat the current buffer."
  (interactive "r")
  (format-sql-bf--apply-executable-to-buffer "format-sql"
                                             'format-sql--call-executable
                                             only-on-region
                                             (get-file-type)))


;;;###autoload
(defun format-sql-region ()
  "Uses the \"format-sql\" tool to reformat the current region."
  (interactive)
  (format-sql--call t))


;;;###autoload
(defun format-sql-buffer ()
  "Uses the \"format-sql\" tool to reformat the current buffer."
  (interactive)
  (format-sql--call nil))


;; BEGIN GENERATED -----------------
;; !!! This file is generated !!!
;; buftra.el
;; Copyright (C) 2015, Friedrich Paetzke <paetzke@fastmail.fm>
;; Author: Friedrich Paetzke <paetzke@fastmail.fm>
;; URL: https://github.com/paetzke/buftra.el
;; Version: 0.4


(defun format-sql-bf--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in format-sql-bf--apply-rcs-patch"))
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
              (error "invalid rcs patch or internal error in format-sql-bf-apply--rcs-patch")))))))))


(defun format-sql-bf--replace-region (filename)
  (delete-region (region-beginning) (region-end))
  (insert-file-contents filename))


(defun format-sql-bf--apply-executable-to-buffer (executable-name
                                           executable-call
                                           only-on-region
                                           file-extension)
  "Formats the current buffer according to the executable"
  (when (not (executable-find executable-name))
    (error (format "%s command not found." executable-name)))
  (let ((tmpfile (make-temp-file executable-name nil (concat "." file-extension)))
        (patchbuf (get-buffer-create (format "*%s patch*" executable-name)))
        (errbuf (get-buffer-create (format "*%s Errors*" executable-name)))
        (coding-system-for-read buffer-file-coding-system)
        (coding-system-for-write buffer-file-coding-system))
    (with-current-buffer errbuf
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-current-buffer patchbuf
      (erase-buffer))

    (if (and only-on-region (use-region-p))
        (write-region (region-beginning) (region-end) tmpfile)
      (write-region nil nil tmpfile))

    (if (funcall executable-call errbuf tmpfile)
        (if (zerop (call-process-region (point-min) (point-max) "diff" nil
                                        patchbuf nil "-n" "-" tmpfile))
            (progn
              (kill-buffer errbuf)
              (message (format "Buffer is already %sed" executable-name)))

          (if only-on-region
              (format-sql-bf--replace-region tmpfile)
            (format-sql-bf--apply-rcs-patch patchbuf))

          (kill-buffer errbuf)
          (message (format "Applied %s" executable-name)))
      (error (format "Could not apply %s. Check *%s Errors* for details"
                     executable-name executable-name)))
    (kill-buffer patchbuf)
    (delete-file tmpfile)))


;; format-sql-bf.el ends here
;; END GENERATED -------------------


(provide 'format-sql)


;;; format-sql.el ends here
