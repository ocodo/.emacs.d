;;; darkroom-mode.el - Distraction free editing mode

;; Author: Martin Svenson
;; Usage: M-x darkroom-mode
;; License: free for all usages/modifications/distributions/whatever.
;; Requirements: w32-fullscreen.el or something similar under *nix

(require 'cl)

(require 'w32-fullscreen)
(require 'frame-local-vars)

;; ------ configuration -----
(defvar darkroom-mode-face-foreground "white"
  "The foreground color of the default face")

(defvar darkroom-mode-left-margin 0
  "Margin to add to the left side of the screen, depends on your resolution and prefered column width")

(defvar darkroom-mode-right-margin 0
  "Margin to add to the right side of the screen, depends on your resolution and prefered column width")

(defvar darkroom-mode-enable-multi-monitor-support t
  "Whether to enable multi-frame (i.e multiple monitor) support. An option since this feature is experimental")

(defvar darkroom-mode-enable-longline-wrap t
  "If longlines-mode is enabled, should longlines-wrap-follows-window-size also be enabled when going into darkroom mode?")

;; -------- code start -------
(setq *darkroom-mode-memtable* (make-hash-table))

(defun* darkroom-recall (var &optional (frame (selected-frame)))
  (cdr (assoc var (gethash frame *darkroom-mode-memtable*))))

(defun* darkroom-remember (var val &optional (frame (selected-frame)))
  (let* ((varlist (gethash frame *darkroom-mode-memtable*))
	 (target (assoc var varlist)))
    (cond (target
	   (setf (cdr target) val))
	  (t
	   (puthash frame (cons (cons var val)
				varlist) *darkroom-mode-memtable*)))))

(defun darkroom-mode-set-enabled(var)
  (darkroom-remember 'darkroom-mode-enabled var))

(defun darkroom-mode-enabledp()
  (darkroom-recall 'darkroom-mode-enabled))

(defun darkroom-mode-update-window()
  (set-window-margins (selected-window)
		      left-margin-width
		      right-margin-width))

(defun darkroom-mode ()
  (interactive)
  (cond ((darkroom-mode-enabledp)
	 (darkroom-mode-disable))
	(t
	 (darkroom-mode-enable))))

(defun darkroom-mode-enable()
  (interactive)
  ; ----- colors
  ; - remember colors
;;  (darkroom-remember 'background-color (frame-parameter nil 'background-color))
;;  (darkroom-remember 'foreground-color (frame-parameter nil 'foreground-color))
;;  (darkroom-remember 'cursor-color (frame-parameter nil 'cursor-color))
;;  (darkroom-remember 'fc-bg-region (face-background 'region))
;;  (darkroom-remember 'fc-fg-region (face-foreground 'region))
;;  (darkroom-remember 'fc-bg-modeline (face-background 'mode-line))
;;  (darkroom-remember 'fc-fg-modeline (face-foreground 'mode-line))
  ; - set colors
  ;; (modify-frame-parameters
  ;;  (selected-frame)
  ;;  `((background-color . "#110000")
  ;;    (foreground-color . ,darkroom-mode-face-foreground)
  ;;    (cursor-color . "white")))
  ;; (set-face-foreground 'region "white" (selected-frame))
  ;; (set-face-background 'region "#004488" (selected-frame))
  ;; (set-face-foreground 'mode-line "gray80" (selected-frame))
  ;; (set-face-background 'mode-line "gray20" (selected-frame))
  
  ;; ; ----- margins
  ;; ; note: margins are buffer local, so if multi-monitor support is
  ;; ;       enabled, frame-locals are used. Otherwise, it's set
  ;; ;       globally.
  ;; ; - remember margins (only needed if multi-monitor support is disabled)
   (unless darkroom-mode-enable-multi-monitor-support
     (darkroom-remember 'left-margin-width
   		       (default-value 'left-margin-width))
     (darkroom-remember 'right-margin-width
   		       (default-value 'right-margin-width)))
  ;; - set margins
   (cond (darkroom-mode-enable-multi-monitor-support
   	 (setq-frame-default 'left-margin-width darkroom-mode-left-margin)
   	 (setq-frame-default 'right-margin-width darkroom-mode-right-margin)
   	 (frame-local-variables-check t))
   	(t
   	 (setq-default left-margin-width darkroom-mode-left-margin)
   	 (setq-default right-margin-width darkroom-mode-right-margin)))
  (darkroom-mode-update-window)
  
  ; ----- other settings
  ; - remember
  (darkroom-remember 'line-spacing (frame-parameter nil 'line-spacing))
  (when (and  (boundp 'longlines-mode)
	      longlines-mode
	      darkroom-mode-enable-longline-wrap)
    (darkroom-remember 'longlines-wrap-follow
	  longlines-wrap-follows-window-size))
  ; - set
  (modify-frame-parameters (selected-frame)
			   '((line-spacing . 1)))
  (when (and 
	 (boundp 'longlines-mode)
	 longlines-mode
	 darkroom-mode-enable-longline-wrap)
    (longlines-mode 0)
    (setq longlines-wrap-follows-window-size t)
    (longlines-mode 1))
  
  ; ---- frame size
  ; - remember
  (darkroom-remember 'frame-width (frame-parameter nil 'width))
  (darkroom-remember 'frame-height (frame-parameter nil 'height))
  (darkroom-remember 'frame-left (frame-parameter nil 'left))
  (darkroom-remember 'frame-top (frame-parameter nil 'top))
  ; - set
  (cond 
   ((eq system-type 'darwin) (ns-toggle-fullscreen))
   ((eq system-type 'windows-nt) (w32-fullscreen-on))
  )

  (darkroom-mode-set-enabled t)
  (message (format "darkroom mode enabled on %s" (selected-frame))))

(defun darkroom-mode-disable()
  (interactive)
  ; - restore colors
  ;; (modify-frame-parameters
  ;;  (selected-frame)
  ;;  `((background-color . ,(darkroom-recall 'background-color))
  ;;    (foreground-color . ,(darkroom-recall 'foreground-color))
  ;;    (cursor-color . ,(darkroom-recall 'cursor-color))))
  ;; (set-face-foreground 'region
  ;; 		       (darkroom-recall 'fc-fg-region) (selected-frame))
  ;; (set-face-background 'region
  ;; 		       (darkroom-recall 'fc-bg-region) (selected-frame))
  ;; (set-face-foreground 'mode-line
  ;; 		       (darkroom-recall 'fc-fg-modeline) (selected-frame))
  ;; (set-face-background 'mode-line
  ;; 		       (darkroom-recall 'fc-bg-modeline) (selected-frame))
  ; - restore other settings
  (modify-frame-parameters
   (selected-frame)
   `((line-spacing . ,(darkroom-recall 'line-spacing))))
  (when (and
	 (boundp 'longlines-mode)
	 longlines-mode
	 (darkroom-recall 'longlines-wrap-follow)
	 darkroom-mode-enable-longline-wrap)
    (longlines-mode 0)
    (setq longlines-wrap-follows-window-size
	  (darkroom-recall 'longlines-wrap-follow))
    (longlines-mode 1))
  ; - restore margins
  (cond (darkroom-mode-enable-multi-monitor-support
	 (unset-frame-default 'left-margin-width)
	 (unset-frame-default 'right-margin-width)
	 (frame-local-variables-check t))
	(t
	 (setq-default left-margin-width
		       (darkroom-recall 'left-margin-width))
	 (setq-default right-margin-width
		       (darkroom-recall 'right-margin-width))))
  (darkroom-mode-update-window)
  ; - restore frame size	 
  (cond 
   ((eq system-type 'darwin) (ns-toggle-fullscreen))
   ((eq system-type 'windows-nt) (w32-fullscreen-off))
  )
  (darkroom-mode-recall-frame-size)
  (darkroom-mode-set-enabled nil)
  (message (format "darkroom-mode disabled on %s" (selected-frame)))
  ; for some reason frame size needs to be recalled again ...
  (sleep-for 0.05)
  (darkroom-mode-recall-frame-size))

(defun darkroom-mode-recall-frame-size()
  (modify-frame-parameters (selected-frame)
			   `((left . ,(darkroom-recall 'frame-left))
			     (top . ,(darkroom-recall 'frame-top))
			     (width . ,(darkroom-recall 'frame-width))
			     (height . ,(darkroom-recall 'frame-height)))))

;;;;;;;;;;;; small margins darkroom ;;;
(defun darkroom-mode-small()
  (interactive)
  (let ((darkroom-mode-left-margin 1)
	(darkroom-mode-right-margin 1))
    (darkroom-mode)))

(defun darkroom-mode-ecb()
  (interactive)
  (let ((darkroom-mode-left-margin 10)
	(darkroom-mode-right-margin 1))
    (darkroom-mode)))

;;;;;;;;;;;;;;;;; end ;;;;;;;;;;;;;;;;;
(provide 'darkroom-mode)

(global-set-key [(control shift return)] 'darkroom-mode )

