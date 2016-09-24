;;; markdown-extras-ocodo --- markdown extras for my config
;;; Commentary:
;;; Code:
(require 'subr-x)

(defun markdown-codefence-region (start end)
  "Enclose the region (START END) in a GFM code-fence, ie. enclosed in three backticks."
  (interactive "r")
  (if (and start end)
      (progn
        (goto-char end)
        (insert "\n```\n\n")
        (delete-blank-lines) ; keep only a single blank line below
        (goto-char start)
        (insert "\n\n```\n")
        (forward-line -2)
        (delete-blank-lines) ; keep only a single blank line above
        (goto-char end)
        (forward-line))
    (message "markdown-codefence-region requires a region")))

(defun markdown--csv-to-table (csv)
  "Turn a CSV into a markdown table."
  (string-join (mapcar
                (lambda (line)
                  (setq line (format "| %s |" line))
                  (s-replace "," " | " line))
                (split-string (s-chomp csv) "\n")) "\n"))

(defun markdown-csv-to-table (begin end)
  "Convert the csv in region (BEGIN END) into a markdown table."
  (interactive "r")
  (unless (region-active-p)
    (error "CSV text region must be selected"))
  (let ((table (markdown--csv-to-table (buffer-substring begin end))))
    (delete-region begin end)
    (goto-char begin)
    (insert table)))

(defun markdown--table-header (table)
  "Make the first row of a markdown TABLE a header."
  (let* ((rows (split-string table "\n"))
         (head (car rows))
         (separator (replace-regexp-in-string "[^\|]" "-" head))
         (tail (cdr rows)))
    (string-join (-flatten (list  head separator tail)) "\n")))

(defun markdown-table-header (begin end)
  "Insert a markdown table header in the current region BEGIN to END."
  (interactive "r")
  (unless (region-active-p)
    (error "Markdown table text region must be selected"))
  (let ((table (markdown--table-header (buffer-substring begin end))))
    (delete-region begin end)
    (goto-char begin)
    (insert table)))

(provide 'markdown-extras-ocodo)

;;; markdown-extras-ocodo ends here
