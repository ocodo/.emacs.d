;;; evil-paredit.el --- Paredit support for evil keybindings
;;
;; Copyright (C) 2012 Roman Gonzalez
;;
;; Author: Roman Gonzalez <romanandreg@gmail.com>
;; Mantainer: Roman Gonzalez <romanandreg@gmail.com>
;; Keywords: paredit, evil
;; Package-Version: 20150413.2048
;;
;; This file is NOT part of GNU Emacs.
;;
;; This file is free software (MIT License)

;; Version: 0.0.2

;; URL: https://github.com/roman/evil-paredit

;; Package-Requires: ((evil "1.0.9") (paredit "25beta"))

;;; Code:

(require 'evil)
(require 'paredit)

;;;###autoload
(define-minor-mode evil-paredit-mode
  "Minor mode for setting up Evil with paredit in a single buffer"
  :keymap '()
  (let ((prev-state evil-state))
    (evil-normal-state)
    (evil-change-state prev-state)))

(defun -evil-paredit-check-region (beg end)
  (if (fboundp 'paredit-check-region-state)
      (if (and beg end)
          ;; Check that region begins and ends in a sufficiently similar
          ;; state, so that deleting it will leave the buffer balanced.
          (save-excursion
            (goto-char beg)
            (let* ((state (paredit-current-parse-state))
                   (state* (parse-partial-sexp beg end nil nil state)))
              (paredit-check-region-state state state*))))
    (paredit-check-region-for-delete beg end)))

(evil-define-operator evil-paredit-yank (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (-evil-paredit-check-region beg end)
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
  (let* ((beg (point))
         (end (evil-paredit-kill-end)))
    (evil-paredit-yank beg end type register)))

(evil-define-operator evil-paredit-delete
  (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE respecting parenthesis.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (evil-paredit-yank beg end type register yank-handler)
  (if (eq type 'block)
      (evil-apply-on-block #'delete-region beg end nil)
    (delete-region beg end))
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
  (let* ((beg (point))
         (end (evil-paredit-kill-end)))
    (evil-paredit-delete beg end
                         type register yank-handler)))

(defun evil-paredit-kill-end ()
  "Returns the position where paredit-kill would kill to"
  (when (paredit-in-char-p)             ; Move past the \ and prefix.
    (backward-char 2))                  ; (# in Scheme/CL, ? in elisp)
  (let* ((eol (point-at-eol))
         (end-of-list-p (save-excursion
                          (paredit-forward-sexps-to-kill (point) eol))))
    (if end-of-list-p (progn (up-list) (backward-char)))
    (cond ((paredit-in-string-p)
           (if (save-excursion (paredit-skip-whitespace t (point-at-eol))
                               (eolp))
               (kill-line)
             (save-excursion
               ;; Be careful not to split an escape sequence.
               (if (paredit-in-string-escape-p)
                   (backward-char))
               (min (point-at-eol)
                    (cdr (paredit-string-start+end-points))))))
          ((paredit-in-comment-p)
           eol)
          (t (if (and (not end-of-list-p)
                      (eq (point-at-eol) eol))
                 eol
               (point))))))

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
  (let* ((beg (point))
         (end (evil-paredit-kill-end)))
    (evil-paredit-change beg end type register yank-handler)))

(defun evil-paredit-change-whole-line ()
  "Change whole line."
  (interactive)
  (beginning-of-line)
  (evil-paredit-change-line nil nil)
  (indent-according-to-mode))

(evil-define-key 'normal evil-paredit-mode-map
  (kbd "d") 'evil-paredit-delete
  (kbd "c") 'evil-paredit-change
  (kbd "y") 'evil-paredit-yank
  (kbd "D") 'evil-paredit-delete-line
  (kbd "C") 'evil-paredit-change-line
  (kbd "S") 'evil-paredit-change-whole-line
  (kbd "Y") 'evil-paredit-yank-line
  (kbd "X") 'paredit-backward-delete
  (kbd "x") 'paredit-forward-delete)

(provide 'evil-paredit)

;;; evil-paredit.el ends here
