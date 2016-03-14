;;; evil-jumper.el --- Jump like vimmers do!

;; Copyright (C) 2014-2016 by Bailey Ling
;; Author: Bailey Ling
;; URL: https://github.com/bling/evil-jumper
;; Package-Version: 20160227.1234
;; Filename: evil-jumper.el
;; Description: Jump like vimmers do! (for older versions of evil-mode)
;; Created: 2014-07-01
;; Version: 0.3.1
;; Keywords: evil vim jumplist jump list
;; Package-Requires: ((evil "0") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;
;; evil-jumper is an add-on for older versions of evil-mode (prior
;; to Feb 2016) which replaces the implementation the jump list such
;; that it mimics more closely with Vim's behavior. Specifically, it
;; will jump across buffer boundaries and revive dead buffers if
;; necessary. The jump list can also be persisted to history file
;; using `savehist' and restored
;; between sessions.
;;
;; Install:
;;
;; (require 'evil-jumper)
;;
;; Usage:
;;
;; (evil-jumper-mode t)

;;; Code:

(require 'cl-lib)
(require 'evil)

(defgroup evil-jumper nil
  "evil-jumper configuration options."
  :prefix "evil-jumper"
  :group 'evil)

(defcustom evil-jumper-max-length 100
  "The maximum number of jumps to keep track of."
  :type 'integer
  :group 'evil-jumper)

(defcustom evil-jumper-pre-jump-hook nil
  "Hooks to run just before jumping to a location in the jump list."
  :type 'hook
  :group 'evil-jumper)

(defcustom evil-jumper-post-jump-hook nil
  "Hooks to run just after jumping to a location in the jump list."
  :type 'hook
  :group 'evil-jumper)

(defcustom evil-jumper-ignored-file-patterns '("COMMIT_EDITMSG$" "TAGS$")
  "A list of pattern regexps to match on the file path to exclude from being included in the jump list."
  :type '(repeat string)
  :group 'evil-jumper)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evil-jumper--jumping nil)
(defvar evil-jumper--debug nil)
(defvar evil-jumper--wired nil)

(defvar evil-jumper--buffer-targets "\\*\\(new\\|scratch\\)\\*"
  "Regexp to match against `buffer-name' to determine whether it's a valid jump target.")

(defvar evil-jumper--window-jumps (make-hash-table)
  "Hashtable which stores all jumps on a per window basis.")

(defvar evil-jumper--jump-list nil
  "Printable version of `evil-jumper--window-jumps'.")

(cl-defstruct evil-jumper-struct
  jumps
  (idx -1))

(defun evil-jumper--message (format &rest args)
  (when evil-jumper--debug
    (setq format (concat "evil-jumper: " format))
    (apply 'message format args)))

(defun evil-jumper--get-current (&optional window)
  (unless window
    (setq window (frame-selected-window)))
  (let* ((jump-struct (gethash window evil-jumper--window-jumps)))
    (unless jump-struct
      (setq jump-struct (make-evil-jumper-struct))
      (puthash window jump-struct evil-jumper--window-jumps))
    jump-struct))

(defun evil-jumper--get-window-jump-list ()
  (let ((struct (evil-jumper--get-current)))
    (evil-jumper-struct-jumps struct)))

(defun evil-jumper--set-window-jump-list (list)
  (let ((struct (evil-jumper--get-current)))
    (setf (evil-jumper-struct-jumps struct) list)))

(defun evil-jumper--savehist-sync ()
  "Updates the printable value of window jumps for `savehist'."
  (setq evil-jumper--jump-list
        (cl-remove-if-not #'identity
                          (mapcar #'(lambda (jump)
                                      (let* ((mark (car jump))
                                             (pos (if (markerp mark)
                                                      (marker-position mark)
                                                    mark))
                                             (file-name (cadr jump)))
                                        (if (and (not (file-remote-p file-name))
                                                 (file-exists-p file-name)
                                                 pos)
                                            (list pos file-name)
                                          nil)))
                                  (evil-jumper--get-window-jump-list)))))

(defun evil-jumper--jump-to-index (idx)
  (let ((target-list (evil-jumper--get-window-jump-list)))
    (when (and (< idx (length target-list))
               (>= idx 0))
      (run-hooks 'evil-jumper-pre-jump-hook)
      (setf (evil-jumper-struct-idx (evil-jumper--get-current)) idx)
      (let* ((place (nth idx target-list))
             (pos (car place))
             (file-name (cadr place)))
        (setq evil-jumper--jumping t)
        (if (string-match-p evil-jumper--buffer-targets file-name)
            (switch-to-buffer file-name)
          (find-file file-name))
        (setq evil-jumper--jumping nil)
        (goto-char pos)
        (run-hooks 'evil-jumper-post-jump-hook)))))

(defun evil-jumper--push ()
  "Pushes the current cursor/file position to the jump list."
  (let ((target-list (evil-jumper--get-window-jump-list)))
    (while (> (length target-list) evil-jumper-max-length)
      (nbutlast target-list 1))
    (let ((file-name (buffer-file-name))
          (buffer-name (buffer-name))
          (current-pos (point-marker))
          (first-pos nil)
          (first-file-name nil)
          (excluded nil))
      (when (and (not file-name)
                 (string-match-p evil-jumper--buffer-targets buffer-name))
        (setq file-name buffer-name))
      (when file-name
        (dolist (pattern evil-jumper-ignored-file-patterns)
          (when (string-match-p pattern file-name)
            (setq excluded t)))
        (unless excluded
          (when target-list
            (setq first-pos (caar target-list))
            (setq first-file-name (car (cdar target-list))))
          (unless (and (equal first-pos current-pos)
                       (equal first-file-name file-name))
            (push `(,current-pos ,file-name) target-list)))))
    (evil-jumper--message "%s %s" (selected-window) (car target-list))
    (evil-jumper--set-window-jump-list target-list)))

(defun evil-jumper--set-jump ()
  (unless evil-jumper--jumping
    ;; clear out intermediary jumps when a new one is set
    (let* ((struct (evil-jumper--get-current))
           (target-list (evil-jumper-struct-jumps struct))
           (idx (evil-jumper-struct-idx struct)))
      (nbutlast target-list idx)
      (setf (evil-jumper-struct-jumps struct) target-list)
      (setf (evil-jumper-struct-idx struct) -1))
    (evil-jumper--push)))

(evil-define-motion evil-jumper/backward (count)
  (let ((count (or count 1)))
    (evil-motion-loop (nil count)
      (let* ((struct (evil-jumper--get-current))
             (idx (evil-jumper-struct-idx struct)))
        (when (= idx -1)
          (setq idx (+ idx 1))
          (setf (evil-jumper-struct-idx struct) idx)
          (evil-jumper--push))
        (evil-jumper--jump-to-index (+ idx 1))))))

(evil-define-motion evil-jumper/forward (count)
  (let ((count (or count 1)))
    (evil-motion-loop (nil count)
      (let* ((struct (evil-jumper--get-current))
             (idx (evil-jumper-struct-idx struct)))
        (evil-jumper--jump-to-index (- idx 1))))))

(defun evil-jumper--window-configuration-hook (&rest args)
  (let* ((window-list (window-list-1 nil nil t))
         (existing-window (selected-window))
         (new-window (previous-window)))
    (when (and (not (eq existing-window new-window))
               (> (length window-list) 1))
      (let* ((target-jump-struct (evil-jumper--get-current new-window))
             (target-jump-count (length (evil-jumper-struct-jumps target-jump-struct))))
        (if (evil-jumper-struct-jumps target-jump-struct)
            (evil-jumper--message "target window %s already has %s jumps" new-window target-jump-count)
          (evil-jumper--message "new target window detected; copying %s to %s" existing-window new-window)
          (let* ((source-jump-struct (evil-jumper--get-current existing-window))
                 (source-list (evil-jumper-struct-jumps source-jump-struct)))
            (when (= (length (evil-jumper-struct-jumps target-jump-struct)) 0)
              (setf (evil-jumper-struct-idx target-jump-struct) (evil-jumper-struct-idx source-jump-struct))
              (setf (evil-jumper-struct-jumps target-jump-struct) (copy-sequence source-list)))))))
    ;; delete obsolete windows
    (maphash (lambda (key val)
               (unless (member key window-list)
                 (evil-jumper--message "removing %s" key)
                 (remhash key evil-jumper--window-jumps)))
             evil-jumper--window-jumps)))

(defun evil-jumper--savehist-init ()
  (unless evil-jumper--wired
    (evil-jumper--set-window-jump-list evil-jumper--jump-list)
    (eval-after-load 'savehist
      '(progn
         (push 'evil-jumper--jump-list savehist-additional-variables)
         (add-hook 'savehist-save-hook #'evil-jumper--savehist-sync)))
    (setq evil-jumper--wired t)))

;;;###autoload
(define-minor-mode evil-jumper-mode
  "Global minor mode for vim jumplist emulation."
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (evil-define-key 'normal map [remap evil-jump-backward] #'evil-jumper/backward)
            (evil-define-key 'normal map [remap evil-jump-forward] #'evil-jumper/forward)
            map)
  (when (fboundp 'evil-jumps-struct-p)
    (message "evil-jumper has been integrated into evil-mode and is obsolete.")
    (setq evil-jumper-mode nil))

  (if evil-jumper-mode
      (progn
        (if (boundp 'evil-jumper-file)
            (message "The variable `evil-jumper-file' is obsolete.  Persistence is done with `savehist' now."))
        (if (boundp 'evil-jumper-auto-center)
            (message "The variable `evil-jumper-auto-center' is obsolete. It has been replaced with `evil-jumper-post-jump-hook'."))
        (evil-jumper--savehist-init)
        (add-hook 'next-error-hook #'evil-jumper--set-jump)
        (add-hook 'window-configuration-change-hook #'evil-jumper--window-configuration-hook)
        (defadvice evil-set-jump (after evil-jumper activate)
          (evil-jumper--set-jump))
        (defadvice switch-to-buffer (before evil-jumper activate)
          (evil-jumper--set-jump))
        (defadvice split-window-internal (before evil-jumper activate)
          (evil-jumper--set-jump))
        (defadvice find-tag-noselect (before evil-jumper activate)
          (evil-jumper--set-jump)))
    (when evil-jumper--wired
      (remove-hook 'next-error-hook #'evil-jumper--set-jump)
      (remove-hook 'window-configuration-change-hook #'evil-jumper--window-configuration-hook)
      (ad-remove-advice 'evil-set-jump 'after 'evil-jumper)
      (ad-remove-advice 'switch-to-buffer 'before 'evil-jumper)
      (ad-remove-advice 'split-window-internal 'before 'evil-jumper)
      (ad-remove-advice 'find-tag-noselect 'before 'evil-jumper)))
  (evil-normalize-keymaps))

;;;###autoload
(defun turn-on-evil-jumper-mode ()
  "Turns on vim jumplist emulation."
  (interactive)
  (evil-jumper-mode t))

;;;###autoload
(defun turn-off-evil-jumper-mode ()
  "Turns off vim jumplist emulation."
  (interactive)
  (evil-jumper-mode -1))

;;;###autoload
(defalias 'global-evil-jumper-mode 'evil-jumper-mode)

(provide 'evil-jumper)

;;; evil-jumper.el ends here
