;; Handy functions, add little helpers in here.

(defun align-number-right (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end ".* \\([0-9]+\\).*" -1 1 nil))

(defun insert-random-in-range (start end)
  (interactive "nRange start:\nnRange end:")
  (insert (format "%i" (random-in-range start end))))

(defun insert-random-radian ()
  "insert a radian value from 0 to 6.28318 (2PI)"
  (interactive)
  (insert (format "%s" (* (/ float-pi 180) (random 361)))))

(defun fraction-radian (denominator)
  (interactive "nDenomiator:")
  (insert (format "%s" (/ (* float-pi 2) denominator))))

(defun random-in-range (start end)
  (random t)
  (+ start (random (+ 1 (- end start)))))

(defun insert-radians-for-degrees (degrees)
  "Insert radians for degrees"
  (interactive "ndegrees:")
  (insert (format "%s" (* (/ float-pi 180) degrees))))

(defun now-is ()
  "Insert current time."
  (interactive)
  (insert (format-time-string "%l:%M%P(%z) %Y-%m-%d")))

(defun utc-seconds ()
  (interactive)
  (insert (format-time-string "%s")))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun open-line-below ()
  (interactive)
  (save-excursion
    (end-of-line)
    (newline)))

(defun open-line-above ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (newline)
    (forward-line -1)))

(global-set-key (kbd "C-S-o") 'open-line-above)
(global-set-key (kbd "C-o") 'open-line-below)

(defun shell-command-on-region-replace (start end command)
  "Run shell-command-on-region interactivly replacing the region in place"
  (interactive (let (string) 
                 (unless (mark)
                   (error "The mark is not set now, so there is no region"))
                 (setq string (read-from-minibuffer "Shell command on region: "
                                                    nil nil nil
                                                    'shell-command-history))
                 (list (region-beginning) (region-end)
                       string)))
  (shell-command-on-region start end command t t))

(defun directory-of-library (library-name)
  "open directory with dired which contains the give library"
  (interactive "M")
  (dired (file-name-as-directory
          (file-name-directory (find-library-name library-name)))))

(defun delete-this-file ()
  (interactive)
  (or (buffer-file-name) (error "no file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun switch-to-scratch ()
  "switch to scratch , take if region is active"
  (interactive)
  (let ((contents
         (and (region-active-p)
              (buffer-substring (region-beginning)
                                (region-end)))))
    (switch-to-buffer "*scratch*")
    (if contents
        (progn
          (goto-char (buffer-end 1))
          (insert contents)))))

(defun ca-with-comment (str)
  (format "%s%s%s" comment-start str comment-end))

(defun ca-all-asscs (asslist query)
  "returns a list of all corresponding values (like rassoc)"
  (cond
   ((null asslist) nil)
   (t
    (if (equal (cdr (car asslist)) query)
        (cons (car (car asslist)) (ca-all-asscs (cdr asslist) query))
      (ca-all-asscs (cdr asslist) query)))))

(defun shell-command-on-buffer-file ()
  "prompts for a command and executes that command on to the associated
 file of current buffer. if no buffer is associated gives an error"
  (interactive)
  (or (buffer-file-name) (error "no file is associated file to this buffer"))
  (let* ((my-cmd (read-shell-command "Command to run: "))
         (cmd-to-run (concat my-cmd " " (buffer-file-name))))
    (shell-command cmd-to-run)))

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

(require 's)

(defun snake-case-at-point-or-region ()
  "snake_case the current word or text selection."
  (interactive)
  (let (pos1 pos2 meat)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'symbol))
            pos2 (cdr (bounds-of-thing-at-point 'symbol))))
    (setq meat (s-snake-case  (buffer-substring-no-properties pos1 pos2)))
    (delete-region pos1 pos2)
    (insert (s-snake-case meat))
    )
  )

(defun dasherise-at-point-or-region ()
  "dasherise-the-current CamelCase or snake_case word or text selection."
  (interactive)
  (let (pos1 pos2 meat)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'symbol))
            pos2 (cdr (bounds-of-thing-at-point 'symbol))))
    (setq meat (s-dashed-words (buffer-substring-no-properties pos1 pos2)))
    (delete-region pos1 pos2)
    (insert  meat)    
    )
  )

(defun upper-camelcase-at-point-or-region ()
  "UpperCamelCaseTheCurrent dashed or snake_case word or any words in text selection."
  (interactive)
  (let (pos1 pos2 meat)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'symbol))
            pos2 (cdr (bounds-of-thing-at-point 'symbol))))
    (setq meat (s-upper-camel-case (buffer-substring-no-properties pos1 pos2)))
    (delete-region pos1 pos2)
    (insert  meat)    
    )
  )


(defun lower-camelcase-at-point-or-region ()
  "lowerCamelCaseTheCurrent dashed or snake_case word or any words in text selection."
  (interactive)
  (let (pos1 pos2 meat)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'symbol))
            pos2 (cdr (bounds-of-thing-at-point 'symbol))))
    (setq meat (s-lower-camel-case (buffer-substring-no-properties pos1 pos2)))
    (delete-region pos1 pos2)
    (insert  meat)    
    )
  )



(defun yank-repeat (&optional arg)
  "Repeat yank with M- number"
  (interactive "*p")
  (dotimes (i arg)
    (yank)))

(provide 'handy-functions)
