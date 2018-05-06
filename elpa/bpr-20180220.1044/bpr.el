;;; bpr.el --- Background Process Runner

;; Author: Ilya Babanov <ilya-babanov@ya.ru>
;; URL: https://github.com/ilya-babanov/emacs-bpr
;; Version: 1.5
;; Package-Requires: ((emacs "24"))
;; Keywords: background, async, process, management
;; License: Unlicense

;;; Commentary:
;; This package provides functionality for running processes in background.
;; For detailed instructions see https://github.com/ilya-babanov/emacs-bpr.

;;; Code:
(require 'shell)

(defgroup bpr nil
  "Background Process Runner"
  :group 'processes
  :group 'extensions)

(defcustom bpr-close-after-success nil
  "Indicates whether the process output window is closed on success."
  :type 'boolean)

(defcustom bpr-open-after-error t
  "Indicates whether the process output window is shown on error."
  :group 'bpr
  :type 'boolean)

(defcustom bpr-window-creator #'split-window-vertically
  "Function for creating window for process."
  :group 'bpr
  :type 'function)

(defcustom bpr-process-mode #'shell-mode
  "Mode for process's buffer."
  :group 'bpr
  :type 'function)

(defcustom bpr-process-directory nil
  "Directory for process.
If not nil, it will be assigned to default-direcotry.
If nil, standard default-direcotry will be used,
or projectile-project-root, if it's available and bpr-use-projectile isn't nil."
  :group 'bpr
  :type 'string)

(defcustom bpr-use-projectile t
  "Whether to use projectile-project-root (if available) for process's directory."
  :group 'bpr
  :type 'boolean)

(defcustom bpr-erase-process-buffer t
  "Indicates whether the process buffer is erased at the start of the new process."
  :group 'bpr
  :type 'boolean)

(defcustom bpr-scroll-direction 1
  "Scroll text in error window, -1 for scroll up, 1 - scroll down."
  :group 'bpr
  :type 'number)

(defcustom bpr-show-progress t
  "Whether to show progress messages for process."
  :group 'bpr
  :type 'boolean)

(defcustom bpr-poll-timout 0.2
  "Progress update interval."
  :group 'bpr
  :type 'number)

(defcustom bpr-colorize-output nil
  "Whether to colorize process output buffer.
For this operation `ansi-color-apply-on-region' is used."
  :group 'bpr
  :type 'boolean)

(defcustom bpr-on-start '(lambda (process))
  "Function, which is called when the process starts.
If function is interactive, it's called interactively;
if not, it's called in normal way with one argument - process."
  :group 'bpr
  :type 'function)

(defcustom bpr-on-success '(lambda (process))
  "Function, which is called in case of success.
If function is interactive, it's called interactively;
if not, it's called in normal way with one argument - process."
  :group 'bpr
  :type 'function)

(defcustom bpr-on-error '(lambda (process))
  "Function, which is called in case of error
If function is interactive, it's called interactively;
if not, it's called in normal way with one argument - process."
  :group 'bpr
  :type 'function)

(defcustom bpr-on-completion '(lambda (process))
  "Function, which is always called when process is completed
If function is interactive, it's called interactively;
if not, it's called in normal way with one argument - process."
  :group 'bpr
  :type 'function)

(defvar bpr-last-buffer nil
  "Buffer for the last spawned process.")

;;;###autoload
(defun bpr-spawn (cmd)
  "Executes string CMD asynchronously in background."
  (interactive
   (list (read-shell-command "Command: ")))
  (let* ((proc-name (bpr-create-process-name cmd))
         (process (get-process proc-name)))
    (if process
        (progn
          (message "Process already exist: %s" process)
          (bpr-try-refresh-process-window process))
      (bpr-run-process cmd))))

;;;###autoload
(defun bpr-open-last-buffer ()
  "Opens the buffer of the last spawned process."
  (interactive)
  (if (buffer-live-p bpr-last-buffer)
      (set-window-buffer (funcall bpr-window-creator) bpr-last-buffer)
    (message "Can't find last used buffer")))

(defun bpr-run-process (cmd)
  (message "Running process '%s'" cmd)
  (let* ((default-directory (bpr-get-current-directory))
         (proc-name (bpr-create-process-name cmd))
         (buff-name (concat "*" proc-name "*"))
         (buffer (get-buffer-create buff-name))
         (process (start-process-shell-command proc-name buffer cmd)))
    (setq bpr-last-buffer buffer)
    (set-process-plist process (bpr-create-process-plist))
    (set-process-sentinel process 'bpr-handle-result)
    (bpr-handle-progress process)
    (bpr-config-process-buffer buffer)
    (bpr-funcall (process-get process 'on-start) process)))

(defun bpr-get-current-directory ()
  (if bpr-process-directory
      bpr-process-directory
    (bpr-try-get-project-root)))

(defun bpr-try-get-project-root ()
  (if (and bpr-use-projectile (fboundp 'projectile-project-root))
      (let ((projectile-require-project-root nil))
        (projectile-project-root))
    default-directory))

(defun bpr-create-process-name (cmd)
  (concat cmd " (" (abbreviate-file-name (bpr-get-current-directory)) ")"))

(defun bpr-create-process-plist ()
  (list 'poll-timeout bpr-poll-timout
        'close-after-success bpr-close-after-success
        'open-after-error bpr-open-after-error
        'show-progress bpr-show-progress
        'window-creator bpr-window-creator
        'colorize-output bpr-colorize-output
        'scroll-direction bpr-scroll-direction
        'on-start bpr-on-start
        'on-success bpr-on-success
        'on-error bpr-on-error
        'on-completion bpr-on-completion
        'start-time (float-time)))

(defun bpr-config-process-buffer (buffer)
  (when buffer
    (with-current-buffer buffer
      (when (and bpr-erase-process-buffer (not buffer-read-only))
        (let ((inhibit-read-only t))
          (erase-buffer)))
      (funcall bpr-process-mode))))

(defun bpr-handle-progress (process)
  (when (process-live-p process)
    (let* ((show-progress (process-get process 'show-progress)))
      (when show-progress (bpr-show-progress-message process))
      (bpr-delay-progress-handler process))))

(defun bpr-delay-progress-handler (process)
  (let* ((poll-timeout (process-get process 'poll-timeout)))
    (run-at-time poll-timeout nil 'bpr-handle-progress process)))

(defun bpr-handle-result (process &optional event)
  (bpr-colorize-process-buffer process)
  (unless (process-live-p process)
    (bpr-funcall (process-get process 'on-completion) process)
    (let* ((exit-code (process-exit-status process)))
      (if (= exit-code 0)
          (bpr-handle-success process)
        (bpr-handle-error process exit-code)))))

(defun bpr-handle-success (process)
  (bpr-funcall (process-get process 'on-success) process)
  (bpr-show-success-message process)
  (let* ((buffer-window (bpr-get-process-window process))
         (close-after-success (process-get process 'close-after-success)))
    (when (and buffer-window close-after-success)
      (delete-window buffer-window))))

(defun bpr-handle-error (process exit-code)
  (bpr-funcall (process-get process 'on-error) process)
  (bpr-show-error-message process exit-code)
  (let* ((buffer (process-buffer process))
         (buffer-window (get-buffer-window buffer))
         (open-after-error (process-get process 'open-after-error)))
    (when (and open-after-error (not buffer-window))
      (setq buffer-window (funcall (process-get process 'window-creator)))
      (set-window-buffer buffer-window buffer))
    (bpr-try-refresh-process-window process)))

(defun bpr-show-progress-message (process)
  (let* ((status (process-status process))
         (time-diff (bpr-get-process-time-diff process)))
    (message "Status: %s   Time: %.1f   Process: %s" status time-diff process)))

(defun bpr-show-success-message (process)
  (message "Status: %s   Time: %.3f   Process: %s"
           (propertize "Success" 'face '(:foreground "green"))
           (bpr-get-process-time-diff process)
           process))

(defun bpr-show-error-message (process exit-code)
  (message "Status: %s   Code: %s   Time: %.3f   Process: %s"
           (propertize "Error" 'face '(:foreground "red"))
           exit-code
           (bpr-get-process-time-diff process)
           process))

(defun bpr-get-process-time-diff (process)
  (let* ((start-time (process-get process 'start-time)))
    (- (float-time) start-time)))

(defun bpr-get-process-window (process)
  (let* ((buffer (process-buffer process)))
    (get-buffer-window buffer)))

(defun bpr-try-refresh-process-window (process)
  (let* ((window (bpr-get-process-window process))
         (scroll-direciton (process-get process 'scroll-direction)))
    (when window (bpr-refresh-process-window window scroll-direciton))))

(defun bpr-refresh-process-window (window direction)
  (with-selected-window window
    (ignore-errors
      (scroll-down-command (bpr-get-remaining-lines-count direction)))))

(defun bpr-colorize-process-buffer (process)
  (when (and (process-get process 'colorize-output)
             (fboundp 'ansi-color-apply-on-region))
    (with-current-buffer (process-buffer process)
      (unless buffer-read-only
        (ansi-color-apply-on-region (point-min) (point-max))))))

(defun bpr-get-remaining-lines-count (direction)
  (count-lines (point) (buffer-end direction)))

(defun bpr-funcall (fn &rest args)
  (if (commandp fn t)
      (call-interactively fn)
    (apply fn args)))

(provide 'bpr)
;;; bpr.el ends here
