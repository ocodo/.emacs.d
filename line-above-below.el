(defun add-whitespace-line-below ()
  (interactive)
  (save-excursion
    (end-of-line)
    (newline)))
 
(defun add-whitespace-line-above ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (newline)
    (forward-line -1)))
 
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline))
 
(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1))
 
(global-set-key (kbd "C-S-o") 'open-line-above)
(global-set-key (kbd "C-o") 'open-line-below)

(global-set-key (kbd "C-S-s-o") 'add-whitespace-line-above)
(global-set-key (kbd "M-s-ø") 'add-whitespace-line-below) 

