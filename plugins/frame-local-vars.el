;;; frame-local-vars.el - frame-local variables that >>override<< buffer-local variables

;; Author: Martin Svenson
;; URL: 
;; License: free for all usages/modifications/distributions/whatever.


;; -- settings

(defvar frame-local-ignore-chk-fn #'frame-local-ignore
  "This should be a function that takes one string as an argument and returns
   t if it should be ignored (no frame-local variables stored)")

;; --- code
(require 'cl)

(defvar *frame-local-vars-enabled* t)
(setq *frame-local-table* (make-hash-table))
(setq *frame-local-buftable* (make-hash-table))
(setq *frame-local-last-buffer* nil)

(defun frame-local-vars-enable()
  (setq *frame-local-vars-enabled* t))

(defun frame-local-vars-disable()
  (setq *frame-local-vars-enabled* nil))

(defun* frame-local-gethash(&optional (frame (selected-frame)))
  (gethash frame *frame-local-table*))

(defun* frame-local-getbufhash(&optional (buffer (current-buffer)))
  (gethash buffer *frame-local-buftable*))

(defun* frame-local-remember-bufvar (var &optional (buffer (current-buffer)))
  (let ((value (cond ((not (local-variable-p var buffer))
		      'undefined-xxy)
		     (t 
		      ; (buffer-local-value var buffer)
		      'donotchange-xxy
			))))
    (puthash buffer (cons (cons var value)
			  (frame-local-getbufhash buffer))
	     *frame-local-buftable*)))

(defun* frame-local-unset-bufvar
    (var &optional (buffer (current-buffer)))
  (puthash buffer (remove* var (frame-local-getbufhash buffer)
			   :key #'car) *frame-local-buftable*))

(defun* unset-frame-default (var &optional (frame (selected-frame)))
  (puthash frame (remove* var (frame-local-gethash frame) :key #'car)
	   *frame-local-table*)) 

(defun* setq-frame-default (var value &optional (frame (selected-frame)))
  (let* ((varlist (frame-local-gethash frame))
	 (target (assoc var varlist)))
    (cond (target
	   (setf (cdr target) value)
	   target)
	  (t
	   (puthash frame (cons (cons var value)
				varlist)
		    *frame-local-table*)))))

(defun frame-local-update-vars (frame buffer)
  (let ((frame-list (frame-local-gethash frame))
	(updated (make-hash-table)))
    (let ((buffer-list (frame-local-getbufhash buffer)))
    (dolist (alist frame-list)
      (puthash (car alist) t updated)
      (let ((prev-value (assoc (car alist) buffer-list)))
	(cond ((not prev-value)
	       ; if this is the first time seeing this variable...
	       (frame-local-remember-bufvar (car alist) buffer)
	       (when (not (local-variable-p (car alist) buffer))
	         ; only update symbols that aren't already buflocal
		 (setf (symbol-value (car alist)) (cdr alist))
		 (when (equal (car alist) 'left-margin-width)
		   ; .. ugly hack to get margins propely updating
		   (frame-local-update-window))))
	      (t
	       (when (not (equal (cdr prev-value)
			  'donotchange-xxy))
		 (setf (symbol-value (car alist)) (cdr alist))))))))
    (let ((buffer-list (frame-local-getbufhash buffer)))
      (when buffer-list
	(dolist (alist buffer-list)
	  (cond ((gethash (car alist) updated) nil)
		((equal (cdr alist) 'undefined-xxy)
		 (kill-local-variable (car alist))
		 (frame-local-unset-bufvar (car alist) buffer))
		((equal (cdr alist) 'donotchange-xxy)
		 ; (setf (symbol-value (car alist)) (cdr alist))
		 (frame-local-unset-bufvar (car alist) buffer))))))))

(defun frame-local-variables-check (&optional force)
  (let ((current-buffer (current-buffer)))
    (when (and
	   *frame-local-vars-enabled*
	   (not (funcall frame-local-ignore-chk-fn
			 (buffer-name (current-buffer))))
	   (or force
	       (not (eql current-buffer *frame-local-last-buffer*))))
      (setq *frame-local-last-buffer* current-buffer)
      (frame-local-update-vars (selected-frame) current-buffer))))

(add-hook 'window-configuration-change-hook #'frame-local-variables-check)

;;; --- this is here since buffer-local margins are a bit nasty to get updated
(defun frame-local-update-window()
  (set-window-margins (selected-window)
		      left-margin-width
		      right-margin-width))

;;; --- default ignore function
(defun frame-local-ignore (str)
  (or
   (string-match "\\*Buffer List\\*" str)
   (string-match "^TAGS" str)
   (string-match "^\\*Messages\\*$" str)
   (string-match "^\\*Completions\\*$" str)
   (string-match "^\\*scratch\\*$" str)
   (string-match "^\\*ESS\\*$" str)
   (string-match "^ " str)
   (string-match "\\*swbuff\\*" str)
   (string-match "^.menuacc" str)
   (string-match "Mew message" str)
   (string-match "output\\*$" str)
   (string-match "compilation" str)
   (string-match "^\\*TeX silent\\*$" str)
   (string-match "inbox" str)))

(provide 'frame-local-vars)