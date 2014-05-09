;;; evil-paredit.el --- Paredit support for evil keybindings
;;
;; Copyright (C) 2012 Roman Gonzalez
;;
;; Author: Roman Gonzalez <romanandreg@gmail.com>
;; Mantainer: Roman Gonzalez <romanandreg@gmail.com>
;; Keywords: paredit, evil
;;
;; This file is NOT part of GNU Emacs.
;;
;; This file is free software (MIT License)

;; Version: 20130408.47
;; X-Original-Version: 0.0.1

;; URL: https://github.com/roman/evil-paredit

;; Package-Requires: ((evil "0.0.0") (paredit "1"))

;;; Code:

(require 'evil)
(require 'paredit)

(define-minor-mode evil-paredit-mode
  "Minor mode for setting up Evil with paredit in a single buffer"
  :keymap '()
  (let ((prev-state evil-state))
    (evil-normal-state)
    (evil-change-state prev-state)))

(defun -evil-paredit-check-region (beginning end)
  (if (and beginning end)
      ;; Check that region begins and ends in a sufficiently similar
      ;; state, so that deleting it will leave the buffer balanced.
      (save-excursion
        (goto-char beginning)
        (let* ((state (paredit-current-parse-state))
               (state* (parse-partial-sexp beginning end nil nil state)))
          (paredit-check-region-state state state*)))))

(evil-define-operator evil-paredit-yank (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (if (fboundp 'paredit-check-region-state)
      (-evil-paredit-check-region beg end)
    (paredit-check-region-for-delete beg end))
  (cond
   ((eq type 'block)
    (evil-yank-rectangle beg end register yank-handler))
   ((eq type 'line)
    (evil-yank-lines beg end register yank-handler))
   (t
    (evil-yank-characters beg end register yank-handler))))

(evil-define-operator evil-paredit-yank-line (beg end type register)
  "Saves whole lines into the kill-ring."
  :motion evil-line
  :move-point nil
  (interactive "<R><x>")
  (evil-paredit-yank beg end type register))

(evil-define-operator evil-paredit-delete
  (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE respecting parenthesis.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (evil-paredit-yank beg end type register yank-handler)
  (if (eq type 'block)
      (evil-apply-on-block #'delete-region beg end nil)
    (kill-region beg end))
  ;; place cursor on beginning of line
  (when (and (evil-called-interactively-p)
             (eq type 'line))
    (evil-first-non-blank)))

(evil-define-operator evil-paredit-delete-line
  (beg end type register yank-handler)
  "Delete to end of line respecting parenthesis."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  ;; act linewise in Visual state
  (let* ((beg (or beg (point)))
         (end (or end beg)))
    (when (evil-visual-state-p)
      (unless (memq type '(line block))
        (let ((range (evil-expand beg end 'line)))
          (setq beg (evil-range-beginning range)
                end (evil-range-end range)
                type (evil-type range))))
      (evil-exit-visual-state))
    (cond
     ((eq type 'block)
      ;; equivalent to $d, i.e., we use the block-to-eol selection and
      ;; call `evil-delete'. In this case we fake the call to
      ;; `evil-end-of-line' by setting `temporary-goal-column' and
      ;; `last-command' appropriately as `evil-end-of-line' would do.
      (let ((temporary-goal-column most-positive-fixnum)
            (last-command 'next-line))
        (evil-paredit-delete beg end 'block register yank-handler)))
     ((eq type 'line)
      (evil-paredit-delete beg end type register yank-handler))
     (t
      (evil-paredit-delete beg (line-end-position)
                               type register yank-handler)))))


(evil-define-operator evil-paredit-change
  (beg end type register yank-handler delete-func)
  "Change text from BEG to END with TYPE respecting parenthesis.
Save in REGISTER or the kill-ring with YANK-HANDLER.
DELETE-FUNC is a function for deleting text, default `evil-delete'.
If TYPE is `line', insertion starts on an empty line.
If TYPE is `block', the inserted text in inserted at each line
of the block."
  (interactive "<R><x><y>")
  (let ((delete-func (or delete-func #'evil-paredit-delete))
        (nlines (1+ (- (line-number-at-pos end)
                       (line-number-at-pos beg)))))
    (funcall delete-func beg end type register yank-handler)
    (cond
     ((eq type 'line)
      (evil-open-above 1))
     ((eq type 'block)
      (evil-insert 1 nlines))
     (t
      (evil-insert 1)))))

(evil-define-operator evil-paredit-change-line
  (beg end type register yank-handler)
  "Change to end of line respecting parenthesis."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (evil-paredit-change beg end type register yank-handler))

(evil-define-key 'normal evil-paredit-mode-map
  (kbd "d") 'evil-paredit-delete
  (kbd "c") 'evil-paredit-change
  (kbd "y") 'evil-paredit-yank
  (kbd "D") 'evil-paredit-delete-line
  (kbd "C") 'evil-paredit-change-line
  (kbd "Y") 'evil-paredit-yank-line
  (kbd "X") 'paredit-backward-delete
  (kbd "x") 'paredit-forward-delete)

(provide 'evil-paredit)

;;; evil-paredit.el ends here
