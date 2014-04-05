;;; command-t.el --- Finds file in project using fuzzy search.

;; Copyright (C) 2013 Mikhail Lapshin

;; Author: Mikhail Lapshin <mikhail.a.lapshin@gmail.com>
;; Git: git@github.com:mlapshin/command-t-emacs.git
;; Keywords: project, convenience
;; Version: 0.0.1
;; Package-Requires: ((find-file-in-project "3.2") (popwin "0.4"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Recommended binding: (global-set-key (kbd "C-x p") 'command-t-find-file)

;;; Code:

(require 'find-file-in-project)
(require 'popwin)

(defgroup command-t nil
  "Visit file with typing only path and name abbreviations."
  :group 'find-file
  :prefix "command-t-")

(defcustom command-t-matches-window-height 11
  "Height of popup window with matched file paths."
  :type 'integer
  :group 'command-t)

(defcustom command-t-ctmatch-path "ctmatch"
  "Path to ctmatch binary file"
  :type 'string
  :group 'command-t)

(defface command-t-selected-match-face
  '((t (:foreground "white" :background "black")))
  "Face for selected matching file in popup window"
  :group 'command-t)

(defvar command-t-matches-window nil
  "Popup window")

(defvar command-t-matches-buffer nil
  "Buffer with matches")

(defvar command-t-minibuffer-map nil
  "Keymap used in minubuffer")

(defvar command-t-matches-buffer-map nil
  "Keymap used in matches buffer")

(defvar command-t-halted nil
  "Set to t when command-t-find-file halted by user")

(defun command-t-trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."

  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun command-t-redraw-matches-buffer ()
  (with-current-buffer command-t-matches-buffer
    (setq buffer-read-only nil)
    (erase-buffer)

    (dotimes (match-index (length command-t-matches))
      (let ((match-line (format "%s\n"
                                (cdr (nth match-index command-t-matches)))))
        (insert match-line)))

    (setq buffer-read-only t)
    (command-t-set-selected-match 0)))

(defun command-t-set-selected-match (match-index)
  (with-current-buffer command-t-matches-buffer
    (save-selected-window
      (select-window command-t-matches-window)
      (set 'command-t-selected-match-index match-index)

      (goto-char (point-min))
      (forward-line match-index)

      (move-overlay command-t-selected-match-overlay
                    (line-beginning-position)
                    (+ 1 (line-end-position))))))

(defun command-t-previous-match (&optional arg)
  "Previous match"
  (interactive "p")

  (with-current-buffer command-t-matches-buffer
    (command-t-set-selected-match (max (- command-t-selected-match-index arg) 0))))

(defun command-t-first-match (&optional arg)
  "First match"
  (interactive "p")
  (command-t-set-selected-match 0))

(defun command-t-last-match (&optional arg)
  "Last match"
  (interactive "p")
  (with-current-buffer command-t-matches-buffer
    (command-t-set-selected-match (- (length command-t-matches) 1))))

(defun command-t-next-match (&optional arg)
  "Next match"
  (interactive "p")
  (with-current-buffer command-t-matches-buffer
    (command-t-set-selected-match (min (+ command-t-selected-match-index arg)
                                       (- (length command-t-matches) 1)))))

(defun command-t-minibuffer-changed-handler (start end old-len)
  (let ((lookup-string (minibuffer-contents)))
    (with-current-buffer command-t-matches-buffer
      (setq command-t-lookup-string lookup-string)))

  (command-t-update-matches))

(defun command-t-ffip-find-command ()
  (format "find %s -type f \\( %s \\) %s | head -n %s"
          (ffip-project-root) (ffip-join-patterns) ffip-find-options ffip-limit))

(defun command-t-update-matches ()
  (with-current-buffer command-t-matches-buffer

    (if command-t-files-cached
        (setq command-t-matches
              (mapcar (lambda (path-with-score)
                        (let ((path (replace-regexp-in-string "^[0-9.]+: " "" path-with-score)))
                          (cons path (substring path (length command-t-project-root)))))

                      (delete "" (split-string (shell-command-to-string
                                                (format "cat %s | %s \"%s\""
                                                        command-t-files-cache-file
                                                        command-t-ctmatch-path
                                                        (shell-quote-argument command-t-lookup-string)))
                                               "\n"))))
      (setq command-t-matches '()))

    (command-t-redraw-matches-buffer)))

(defun command-t-minibuffer-setup-handler ()
  (add-hook 'after-change-functions 'command-t-minibuffer-changed-handler nil t)
  (add-hook 'minibuffer-exit-hook 'command-t-minibuffer-exit-handler nil t))

(defun command-t-halt ()
  (interactive)
  (setq command-t-halted t)
  (exit-minibuffer))

(defun command-t-cleanup ()
  (when command-t-matches-buffer
    (with-current-buffer command-t-matches-buffer
      (delete-file command-t-files-cache-file)))

  (kill-buffer command-t-matches-buffer)
  (setq command-t-matches-buffer nil)
  (setq command-t-matches-window nil))

(defun command-t-minibuffer-exit-handler ())

(defun command-t-create-matches-window ()
  (cadr (popwin:create-popup-window command-t-matches-window-height)))

(defun command-t-create-matches-buffer ()
  (setq command-t-matches-buffer (get-buffer-create " *command-t-matches*"))

  (let ((project-root (ffip-project-root))
        (find-command (command-t-ffip-find-command)))

    (with-current-buffer command-t-matches-buffer
      ;; remember ffip variables from original buffer
      (set (make-local-variable 'command-t-project-root) (expand-file-name project-root))
      (set (make-local-variable 'command-t-find-command) find-command)
      (set (make-local-variable 'command-t-files-cache-file) (make-temp-file "ctem"))
      (set (make-local-variable 'command-t-files-cached) nil)
      (set (make-local-variable 'command-t-lookup-string) "")
      (set (make-local-variable 'command-t-total-matches-count) 0)

      (use-local-map command-t-matches-buffer-map)

      ;; create buffer-local variable which holds selected match index
      (set (make-local-variable 'command-t-selected-match-index) 0)
      (set (make-local-variable 'command-t-matches) '())
      (set (make-local-variable 'command-t-find-process) nil)

      (make-local-variable 'smooth-scroll-margin)
      (make-local-variable 'scroll-margin)

      (setq smooth-scroll-margin 1)
      (setq scroll-margin 1)

      (set (make-local-variable 'command-t-selected-match-overlay) (make-overlay 1 1))
      (overlay-put command-t-selected-match-overlay 'face 'command-t-selected-match-face)

      (setq command-t-find-process (start-process-shell-command
                                    "command-t-find-files"
                                    (buffer-name command-t-matches-buffer)
                                    (format "%s > %s"
                                            command-t-find-command
                                            command-t-files-cache-file)))

      (set-process-sentinel command-t-find-process (lambda (process event)
                                                     (with-current-buffer command-t-matches-buffer
                                                       (setq command-t-files-cached t)
                                                       (setq command-t-total-matches-count
                                                             (command-t-trim-string
                                                              (shell-command-to-string
                                                               (format "cat %s | wc -l" command-t-files-cache-file))))

                                                       (command-t-update-matches)
                                                       (force-mode-line-update t))))

      (set (make-local-variable 'mode-line-format) nil)
      (set (make-local-variable 'header-line-format)
           '("Command-T: "
             (:eval (when (not command-t-files-cached)
                      "fetching project files..."))

             (:eval (when command-t-files-cached
                      (format "%s files matched (%s total)"
                              (length command-t-matches)
                              command-t-total-matches-count)))
             ))

      (force-mode-line-update t)

      (setq show-trailing-whitespace nil))))

(defun command-t-find-file (&optional arg)
  "Finds file using fuzzy matching."
  (interactive "p")

  (if (not (ffip-project-root))
      (message "Command-T: No project root found for this file. Please, specify project root according to find-file-at-project documentation.")
    (progn
      (setq command-t-halted nil)

      (command-t-create-matches-buffer)

      (when t ;;(null command-t-minibuffer-map)
        (setq command-t-minibuffer-map (make-sparse-keymap))
        (define-key command-t-minibuffer-map (kbd "<up>") 'command-t-previous-match)
        (define-key command-t-minibuffer-map (kbd "<down>") 'command-t-next-match)

        (define-key command-t-minibuffer-map (kbd "M-j") 'command-t-next-match)
        (define-key command-t-minibuffer-map (kbd "M-k") 'command-t-previous-match)
        (define-key command-t-minibuffer-map (kbd "C-n") 'command-t-next-match)
        (define-key command-t-minibuffer-map (kbd "C-p") 'command-t-previous-match)

        (define-key command-t-minibuffer-map (kbd "M-<") 'command-t-first-match)
        (define-key command-t-minibuffer-map (kbd "M->") 'command-t-last-match)

        (define-key command-t-minibuffer-map (kbd "C-g") 'command-t-halt)

        (set-keymap-parent command-t-minibuffer-map minibuffer-local-map))

      (when t ;;(null command-t-minibuffer-map)
        (setq command-t-matches-buffer-map (make-sparse-keymap))
        (define-key command-t-matches-buffer-map (kbd "q") 'command-t-halt))

      (let ((filename nil))
        (save-window-excursion
          (setq command-t-matches-window (command-t-create-matches-window))
          (set-window-buffer command-t-matches-window command-t-matches-buffer)
          (command-t-redraw-matches-buffer)

          (minibuffer-with-setup-hook 'command-t-minibuffer-setup-handler
            (read-from-minibuffer "Pattern: " nil command-t-minibuffer-map)

            (unless command-t-halted
              (with-current-buffer command-t-matches-buffer
                (setq filename (car (nth command-t-selected-match-index command-t-matches)))))

            (command-t-cleanup)))

        (when filename
          (find-file filename))))))

(global-set-key (kbd "C-x p") 'command-t-find-file)

(provide 'command-t)

;;; command-t.el ends here
