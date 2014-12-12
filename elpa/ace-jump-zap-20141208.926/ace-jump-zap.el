;;; ace-jump-zap.el --- Character zapping, `ace-jump-mode` style

;; Copyright (C) 2014  justin talbott

;; Author: justin talbott <justin@waymondo.com>
;; Keywords: convenience, tools, extensions
;; URL: https://github.com/waymondo/ace-jump-zap
;; Version: 20141208.926
;; X-Original-Version: 0.0.3
;; Package-Requires: ((ace-jump-mode "1.0"))
;;

;;; Commentary:
;;
;; Bind `(ace-jump-zap-up-to-char)' or `(ace-jump-zap-to-char)' to the
;; key-binding of your choice.

;;; Code:

(require 'ace-jump-mode)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

(defvar ajz/zapping nil
  "Internal flag for detecting if currently zapping.")
(defvar ajz/to-char nil
  "Internal flag for determining if zapping to-char or up-to-char.")
(defvar ajz/saved-point nil
  "Internal variable for caching the current point.")

(defcustom ajz/zap-function 'delete-region
  "This is the function used for zapping between point and char.
The default is `delete-region' but it could also be `kill-region'.")

(defcustom ajz/forward-only nil
  "Set to non-nil to choose whether to zap forward only.
Default will zap in both directions in the current window.")

(defun ajz/maybe-zap-start ()
  "Push the mark when zapping with `ace-jump-char-mode'."
  (when ajz/zapping
    (push-mark)))

(defun ajz/maybe-zap-end ()
  "Zap after jumping with `ace-jump-char-mode.'."
  (when ajz/zapping
    (when ajz/to-char (forward-char))
    (cond ((eq ajz/zap-function 'delete-region)
           (call-interactively 'delete-region))
          ((eq ajz/zap-function 'kill-region)
           (kill-region (point) (mark))))
    (deactivate-mark))
  (ajz/reset))

(defun ajz/reset ()
  "Reset the internal zapping variable flags."
  (setq ajz/zapping nil)
  (setq ajz/to-char nil))

(defun ajz/keyboard-reset ()
  "Reset when `ace-jump-mode' is cancelled.
Also called when chosen character isn't found while zapping."
  (interactive)
  (ajz/reset)
  (ace-jump-done))

(defun ajz/forward-query ()
  "Filter for checking if jump candidate is after point."
  (< ajz/saved-point (point)))

(add-hook 'ace-jump-mode-before-jump-hook #'ajz/maybe-zap-start)
(add-hook 'ace-jump-mode-end-hook #'ajz/maybe-zap-end)

;;;###autoload
(defun ace-jump-zap-up-to-char ()
  "Call `ace-jump-char-mode' and zap all characters up to the selected character."
  (interactive)
  (let ((ace-jump-mode-scope 'window)
        (ajz/saved-point (point))
        (ace-jump-search-filter (when ajz/forward-only 'ajz/forward-query)))
    (setq ajz/zapping t)
    (call-interactively 'ace-jump-char-mode)
    (when overriding-local-map
      (define-key overriding-local-map [t] 'ajz/keyboard-reset))))

;;;###autoload
(defun ace-jump-zap-to-char ()
  "Call `ace-jump-char-mode' and zap all characters up to and including the selected character."
  (interactive)
  (setq ajz/to-char t)
  (ace-jump-zap-up-to-char))

;;;###autoload
(defun ace-jump-zap-to-char-dwim (&optional prefix)
  "Without PREFIX, call `zap-to-char'.
With PREFIX, call `ace-jump-zap-to-char'."
  (interactive "P")
  (if prefix
      (ace-jump-zap-to-char)
    (call-interactively 'zap-to-char)))

;;;###autoload
(defun ace-jump-zap-up-to-char-dwim (&optional prefix)
  "Without PREFIX, call `zap-up-to-char'.
With PREFIX, call `ace-jump-zap-up-to-char'."
  (interactive "P")
  (if prefix
      (ace-jump-zap-up-to-char)
    (call-interactively 'zap-up-to-char)))

(provide 'ace-jump-zap)
;;; ace-jump-zap.el ends here
