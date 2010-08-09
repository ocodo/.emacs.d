;;; w32-fullscreen.el - (real) Fullscreen support for win32

;; Author: Martin Svenson
;; URL: 
;; License: free for all usages/modifications/distributions/whatever.
;;
;; Requirements:
;;               - Windows XP (untested on Vista), EmacsW32
;;               - w32toggletitle.exe or .py - Utility to toggle windows titlebar on/off

;;; TODO: enable error msg on non-clean exit from w32toggletitle

;; ------ configuration -----
(defvar w32-fullscreen-toggletitle-cmd
   "w32toggletitle.exe"
   "Path to w32toggletitle command")

;; ------ code -----
(defun w32-fullscreen-maximize-frame ()
  "Maximize the current frame (windows only)"
  (interactive)
  (w32-send-sys-command 61488))
 
(defun w32-fullscreen-restore-frame ()
  "Restore a minimized/maximized frame (windows only)"
  (interactive)
  (w32-send-sys-command 61728))

(defun w32-fullscreen-toggle-titlebar ()
  "Toggle display of the titlebar of frame (windows only)"
  (interactive)
  (call-process w32-fullscreen-toggletitle-cmd
		  nil nil nil
		  (frame-parameter (selected-frame) 'window-id))
  (sleep-for 0.2))

(setq *w32-fullscreen-memtable* (make-hash-table))

(defun* w32-fullscreen-recall (var &optional (frame (selected-frame)))
  (cdr (assoc var (gethash frame *w32-fullscreen-memtable*))))

(defun* w32-fullscreen-remember (var val &optional (frame (selected-frame)))
  (let* ((varlist (gethash frame *w32-fullscreen-memtable*))
	 (target (assoc var varlist)))
    (cond (target
	   (setf (cdr target) val))
	  (t
	   (puthash frame (cons (cons var val)
				varlist) *w32-fullscreen-memtable*)))))

(defun w32-fullscreen()
  (interactive)
  "Toggle fullscreen display of current frame (windows only)"
  (cond ((w32-fullscreen-recall 'enabled)
	 (w32-fullscreen-remember 'enabled nil)
	 (w32-fullscreen-off))
	(t
	 (w32-fullscreen-remember 'enabled t)
	 (w32-fullscreen-on))))

(defun w32-fullscreen-on ()
  "Enable fullscreen display of current frame (windows only)"
  (interactive)
  ; - remember interface settings
  (w32-fullscreen-remember 'menu-bar-lines
			   (frame-parameter nil 'menu-bar-lines))
  (w32-fullscreen-remember 'tool-bar-lines
			   (frame-parameter nil 'tool-bar-lines))
  (w32-fullscreen-remember 'vertical-scroll-bars
	(frame-parameter nil 'vertical-scroll-bars))
  ; - set interface settings
  (modify-frame-parameters (selected-frame)
			   '((menu-bar-lines . 0) (tool-bar-lines . 0)
			     (vertical-scroll-bars . nil)))
  (w32-fullscreen-toggle-titlebar)
  (w32-fullscreen-maximize-frame))

(defun w32-fullscreen-off ()
  "Disable fullscreen display of current frame (windows only)"
  (interactive)
  ; - restore interface settings
  (modify-frame-parameters
   (selected-frame)
   `((menu-bar-lines . ,(w32-fullscreen-recall 'menu-bar-lines))
     (tool-bar-lines . ,(w32-fullscreen-recall 'tool-bar-lines))
     (vertical-scroll-bars . ,(w32-fullscreen-recall 'vertical-scroll-bars))))
  (w32-fullscreen-restore-frame)
  (w32-fullscreen-toggle-titlebar))

(provide 'w32-fullscreen)