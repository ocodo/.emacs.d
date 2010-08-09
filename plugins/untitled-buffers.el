(defun untitled-close-scratch () 
  (kill-buffer "*scratch*") 
  (if (not (delq nil (mapcar 'buffer-file-name (buffer-list))))
      (untitled-new-buffer)))

(defun untitled-emacs-startup-hook () 
  (untitled-close-scratch)) 

(add-hook 'emacs-startup-hook 'untitled-emacs-startup-hook)

(defun untitled-new-buffer () 
  "Opens a new empty buffer." 
  (interactive) 
  (let ((buf (generate-new-buffer (concat "Untitled-"  (number-to-string (list-length (buffer-list)))))))
    (switch-to-buffer buf) 
    (normal-mode)
    (setq buffer-offer-save t))
  (add-hook 'kill-buffer-query-functions 'ask-to-save-modified nil t) )

(defun untitled-close-empty () (if (get-buffer "Untitled") (kill-buffers-by-name "Untitled")))
(add-hook 'find-file-hook 'untitled-close-empty)
