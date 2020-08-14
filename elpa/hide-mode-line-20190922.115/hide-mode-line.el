;;; hide-mode-line.el --- minor mode that hides/masks your modeline -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: March 01, 2018
;; Modified: September 21, 2019
;; Version: 1.0.2
;; Package-Version: 20190922.115
;; Package-Commit: 88888825b5b27b300683e662fa3be88d954b1cea
;; Keywords: frames mode-line
;; URL: https://github.com/hlissner/emacs-hide-mode-line
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Provides `hide-mode-line-mode`.  A minor mode that hides (or masks) the
;; mode-line in your current buffer.  It can be used to toggle an alternative
;; mode-line, toggle its visibility, or simply disable the mode-line in buffers
;; where it isn't very useful otherwise.
;;
;;; Code:

(defvar hide-mode-line-format nil
  "The modeline format to use when `hide-mode-line-mode' is active.")

(defvar hide-mode-line-excluded-modes '(fundamental-mode)
  "List of major modes where `global-hide-mode-line-mode' won't affect.")

(defvar-local hide-mode-line--old-format nil
  "Storage for the old `mode-line-format', so it can be restored when
`hide-mode-line-mode' is disabled.")

;;;###autoload
(define-minor-mode hide-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  (if hide-mode-line-mode
      (progn
        (add-hook 'after-change-major-mode-hook #'hide-mode-line-reset nil t)
        (setq hide-mode-line--old-format mode-line-format
              mode-line-format hide-mode-line-format))
    (remove-hook 'after-change-major-mode-hook #'hide-mode-line-reset t)
    (setq mode-line-format hide-mode-line--old-format
          hide-mode-line--old-format nil))
  (force-mode-line-update))

;; Ensure major-mode or theme changes don't overwrite these variables
(put 'hide-mode-line--old-format 'permanent-local t)
(put 'hide-mode-line-mode 'permanent-local-hook t)
(put 'hide-mode-line-reset 'permanent-local-hook t)

(defun hide-mode-line-reset ()
  "Reset `hide-mode-line-mode' in the current buffer, if necessary.

Sometimes, a major mode is activated after `hide-mode-line-mode' is activated,
thus disabling it (because changing major modes invokes
`kill-all-local-variables' and specifically kills `mode-line-format's local
value, whether or not it's permanent-local.

Attach this to `after-change-major-mode-hook' and `hide-mode-line-mode' will be
cycled to fix this."
  (when hide-mode-line-mode
    (hide-mode-line-mode -1)
    (hide-mode-line-mode +1)))

;;;###autoload
(define-globalized-minor-mode global-hide-mode-line-mode
  hide-mode-line-mode turn-on-hide-mode-line-mode)

;;;###autoload
(defun turn-on-hide-mode-line-mode ()
  "Turn on `hide-mode-line-mode'.
Unless in `fundamental-mode' or `hide-mode-line-excluded-modes'."
  (unless (memq major-mode hide-mode-line-excluded-modes)
    (hide-mode-line-mode +1)))

;;;###autoload
(defun turn-off-hide-mode-line-mode ()
  "Turn off `hide-mode-line-mode'."
  (hide-mode-line-mode -1))

(provide 'hide-mode-line)
;;; hide-mode-line.el ends here
