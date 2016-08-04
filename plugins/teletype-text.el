;;; teletype-text --- some functions to simulate a teletype
;;; Author: Jason Milkins
;;; Url: https://github.com/ocodo/.emacs.d/tree/master/plugins/teletype-text.el
;;
;;; Commentary:
;;; Based on original code by Anrea Rossetti https://github.com/thesoftwarebin
;;
;;  Simulates a teletype to insert text from:
;;  - input into the minibuffer
;;  - last kill (last copy/cut)
;;  - a file
;;  - a register
;;
;;  You can customize the variables `teletype-long-pause-time' and `teletype-short-pause-time'
;;  to vary the output speed.  (Values are specified in Seconds (Decimal - floating point)).
;;
;;; Code:
;;;###autoload
(defgroup teletype nil
  "Simulate teletype text insert."
  :group 'text
  :prefix "teletype-")

;;;###autoload
(defcustom teletype-long-pause-time 0.3
  "Teletype default long pause time."
  :group 'teletype)

;;;###autoload
(defcustom teletype-short-pause-time 0.1
  "Teletype default short pause time."
  :group 'teletype)

;;;###autoload
(defun teletype-char (c small-pause long-pause)
  "Simulate a TeleType to insert character C.
Different pause-times are used for some punctuation chars.

For example teletyping a period triggers a long pause, comma
trigges a medium length pause and newline is executed as
pause-newline-pause.

It's useful for screencasts or a cliche dramatic 'Computer in the
Movies' effect.

SMALL-PAUSE and LONG-PAUSE define the shortest and longest
pause times."
  ;; We'll calcuate a median length pause (used after commas)
  (let ((med-pause (/ (+ long-pause small-pause) 2.0)))
    (pcase c
      ;; Period
      (?.  (insert (char-to-string c))
           (sit-for long-pause))
      ;; Comma
      (?,  (insert (char-to-string c))
           (sit-for med-pause))
      ;; Newline
      (?\n (sit-for med-pause)
           (insert (char-to-string c))
           (sit-for long-pause))
      ;; Anything else
      (_ (insert (char-to-string c))
         (sit-for small-pause)))))

;;;###autoload
(defun teletype-text (text small-pause pause)
  "Insert the TEXT with SMALL-PAUSE and PAUSE to simulate a teletype."
  (mapcar
   (lambda (strchar) (teletype-char strchar small-pause pause))
   (string-to-list text)))

;;;###autoload
(defun teletype-insert-text (text)
  "Simulate a Teletype to insert TEXT."
  (interactive "sTeletype text: ")
  (teletype-text text
                 teletype-short-pause-time
                 teletype-long-pause-time))

;;;###autoload
(defun teletype-insert-last-kill ()
  "Simulate Teletype using text from the last kill."
  (interactive)
  (teletype-text (car kill-ring)
                 teletype-short-pause-time
                 teletype-long-pause-time))

;;;###autoload
(defun teletype-insert-file (file)
  "Simulate Teletype - insert text from a FILE."
  (interactive "fTeleType a file: ")
  (teletype-text (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))
                 teletype-short-pause-time
                 teletype-long-pause-time))

;;;###autoload
(defun teletype-insert-register ()
  "Simulate a Teletype to insert the register using prefix arg C-1 to C-9."
  (interactive)
  (let ((text (if (and
                   current-prefix-arg
                   (> (prefix-numeric-value current-prefix-arg) 0))
                  (get-register (+ 48 (prefix-numeric-value current-prefix-arg)))
                (error "Error: use a prefix arg register (C-1 - C-9)"))))
    (teletype-text text teletype-short-pause-time teletype-long-pause-time)))

;;;###autoload
(defun teletype-from-shell-command (command)
  "Run a shell COMMAND and Teletype the result.
If the region is selected we will use it as the STDIN of the
  shell command, and kill the region before inserting.  Thus
  replacing the region with the teletype output."
  (interactive "sShell Command: ")
  (if (region-active-p)
      (let ((b (region-beginning))
            (e (region-end))
            (teletype-string)
            (cur-buf (current-buffer))
            (temp-buf (make-temp-name "teletype")))
            (shell-command-on-region
             b e
             command
             (get-buffer-create temp-buf))
            (message "---")
            (delete-region b e)
            (deactivate-mark)
            (switch-to-buffer temp-buf)
            (setq teletype-string (buffer-string))
            (switch-to-buffer cur-buf)
            (kill-buffer temp-buf)
            (goto-char b)
            (teletype-insert-text teletype-string))
    (teletype-insert-text (shell-command-to-string command))))

(provide 'teletype-text)
;;; teletype-text.el ends here
