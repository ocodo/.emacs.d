;;; ace-jump-zap.el --- Character zapping, `ace-jump-mode` style

;; Copyright (C) 2014  justin talbott

;; Author: justin talbott <justin@waymondo.com>
;; Keywords: convenience, tools, extensions
;; URL: https://github.com/waymondo/ace-jump-zap
;; Version: 20140923.1913
;; X-Original-Version: 0.0.2
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

(defvar ajz/zapping nil)
(defvar ajz/to-char nil)

(defun ajz/maybe-zap-start ()
  (when ajz/zapping
    (push-mark)))

(defun ajz/maybe-zap-end ()
  (when ajz/zapping
    (when ajz/to-char (forward-char))
    (call-interactively 'delete-region)
    (deactivate-mark))
  (ajz/reset))

(defun ajz/reset ()
  (setq ajz/zapping nil)
  (setq ajz/to-char nil))

(defun ajz/keyboard-reset ()
  (interactive)
  (ajz/reset)
  (ace-jump-done))

(add-hook 'ace-jump-mode-before-jump-hook #'ajz/maybe-zap-start)
(add-hook 'ace-jump-mode-end-hook #'ajz/maybe-zap-end)

;;;###autoload
(defun ace-jump-zap-up-to-char ()
  "Call `ace-jump-char-mode' and zap all characters
up to the selected character."
  (interactive)
  (let ((ace-jump-mode-scope 'window))
    (setq ajz/zapping t)
    (call-interactively 'ace-jump-char-mode)
    (when overriding-local-map
      (define-key overriding-local-map [t] 'ajz/keyboard-reset))))

;;;###autoload
(defun ace-jump-zap-to-char ()
  "Call `ace-jump-char-mode' and zap all characters
up to and including the selected character."
  (interactive)
  (setq ajz/to-char t)
  (ace-jump-zap-up-to-char))

;;;###autoload
(defun ace-jump-zap-to-char-dwim (&optional prefix)
  "Without PREFIX, call `zap-to-char'. With PREFIX, call
`ace-jump-zap-to-char'."
  (interactive "P")
  (if prefix
      (ace-jump-zap-to-char)
    (call-interactively 'zap-to-char)))

;;;###autoload
(defun ace-jump-zap-up-to-char-dwim (&optional prefix)
  "Without PREFIX, call `zap-up-to-char'. With PREFIX, call
`ace-jump-zap-up-to-char'."
  (interactive "P")
  (if prefix
      (ace-jump-zap-up-to-char)
    (call-interactively 'zap-up-to-char)))

(provide 'ace-jump-zap)
;;; ace-jump-zap.el ends here
