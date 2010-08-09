;; kill current buffer without confirmation
(global-set-key "\C-xk" 'kill-current-buffer)

(defun kill-current-buffer ()
  "Kill the current buffer, without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

(provide 'kill-buffer-without-confirm)