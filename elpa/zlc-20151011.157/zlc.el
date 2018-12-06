;;; zlc.el --- Provides zsh like completion system to Emacs

;; Copyright (C) 2010, 2013 mooz

;; Author:  mooz <stillpedant@gmail.com>
;; Version: 0.0.5
;; Package-Version: 20151011.157
;; Keywords: matching, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 'zlc.el' provides zsh like completion system to Emacs. That is, zlc
;; allows you to select candidates one-by-one by pressing `TAB'
;; repeatedly in minibuffer, shell-mode, and so forth. In addition,
;; with arrow keys, you can move around the candidates.
;;
;; To enable zlc, just put the following lines in your Emacs config.
;;
;; (require 'zlc)
;; (zlc-mode t)

;;; Customization:

;; To simulate zsh's `menu select', which allows you to move around
;; candidates, zlc arranges movement commands for 4 directions. If you
;; want to use these commands, bind them to certain keys in your Emacs
;; config.
;;
;; (let ((map minibuffer-local-map))
;;   ;;; like menu select
;;   (define-key map (kbd "<down>")  'zlc-select-next-vertical)
;;   (define-key map (kbd "<up>")    'zlc-select-previous-vertical)
;;   (define-key map (kbd "<right>") 'zlc-select-next)
;;   (define-key map (kbd "<left>")  'zlc-select-previous)
;;
;;   ;;; reset selection
;;   (define-key map (kbd "C-c") 'zlc-reset)
;;   )

;;; Code:

(eval-when-compile (require 'cl))

(defvar zlc--mode nil)
(defvar zlc--global-cache nil)
(defvar zlc--index -1)
(defvar zlc--field-begin 0)
(defvar zlc--previous-overlay nil)

(defface zlc-selected-completion-face
  '((t
     (:foreground "white"
      :background "firebrick"
      :italic nil
      :bold t)))
  "Style of selected item in *Completions* buffer"
  :group 'zlc-face
  )

(defcustom zlc-select-completion-immediately nil
  "Non-nil to select completion immediately when completion list created."
  :type 'boolean
  :group 'zlc)

;; Save completions
(defadvice display-completion-list (after zlc--save-global-cache)
  (setq zlc--global-cache (ad-get-arg 0)))

(defadvice minibuffer-complete (around zlc--around-minibuffer-complete)
  (if zlc--mode
      (zlc-minibuffer-complete)
    ad-do-it))

;; ============================================================ ;;
;; Private
;; ============================================================ ;;

(defsubst zlc--eol-position ()
  (save-excursion
    (end-of-line)
    (point)))

(defsubst zlc--current-columns ()
  (with-current-buffer "*Completions*"
    (save-excursion
      (goto-char (next-single-property-change (point-min) 'mouse-face))
      (let ((edges 0)
            (from (point))
            (limit (zlc--eol-position)))
        (while (/= (setq from (next-single-property-change from 'mouse-face nil limit))
                   limit)
          (incf edges))
        (/ (+ edges 2) 2)))))

(defsubst zlc--normalize-index (idx limit)
  (cond
   ((>= idx limit)
    -1)
   ((< idx 0)
    (if (= idx -1)
        -1                        ; select original string
      (1- limit)))
   (t idx)))

(defsubst zlc--current-candidate ()
  (nth zlc--index zlc--global-cache))

(defsubst zlc--reset ()
  (setq zlc--field-begin (field-end)
        zlc--index -1))

(defsubst zlc--clear-overlay ()
  (when zlc--previous-overlay
    (delete-overlay zlc--previous-overlay)))

(defsubst zlc--ensure-visible (win p)
  (unless (pos-visible-in-window-p p win)
    (set-window-start win p)))

(defsubst zlc--highlight-nth-completion (n)
  (with-current-buffer "*Completions*"
    (let ((begin (point-min))
          (end (point-min)))
      (dotimes (_ (1+ n))
        (setq begin
              (or (next-single-property-change end 'mouse-face) (point-min)))
        (setq end
              (or (next-single-property-change begin 'mouse-face) (point-max))))
      ;; clear previous highlight
      (zlc--clear-overlay)
      ;; create overlay and set face
      (overlay-put
       (setq zlc--previous-overlay
             (make-overlay begin end))
       'face 'zlc-selected-completion-face)
      ;; ensure highlight is in view
      (zlc--ensure-visible (get-buffer-window) begin))))

(defsubst zlc--do-select ()
  ;; clear previous completion
  (delete-region zlc--field-begin (field-end))
  ;; select
  (if (>= zlc--index 0)
      ;; select next completion
      (let* ((cand (zlc--current-candidate))
             (str (if (consp cand) (car cand) cand))
             ;; sometimes (get-text-property 0 'face str) does not work...
             (from (case (cadr (text-properties-at 0 str))
                     ('completions-common-part
                      (or (next-property-change 0 str)
                          (length str)))
                     (t 0))))
        (insert (substring str from))
        (zlc--highlight-nth-completion zlc--index))
    ;; otherwise
    (zlc--clear-overlay)))

;; ============================================================ ;;
;; Public
;; ============================================================ ;;

(defun zlc-reset ()
  (interactive)
  (delete-region zlc--field-begin (field-end))
  (zlc--clear-overlay)
  (zlc--reset))

(defun zlc-select-nth (n)
  (interactive)
  (setq zlc--index (zlc--normalize-index
                    n
                    (length zlc--global-cache)))
  (zlc--do-select))

(defun zlc-select-next (&optional direction)
  (interactive)
  (zlc-select-nth (+ zlc--index (or direction 1))))

(defun zlc-select-previous ()
  (interactive)
  (zlc-select-next -1))

(defun zlc-select-next-vertical (&optional direction)
  (interactive)
  (zlc-select-nth (+ zlc--index (* (zlc--current-columns)
                                   (or direction 1)))))

(defun zlc-select-previous-vertical ()
  (interactive)
  (zlc-select-next-vertical -1))

(defun zlc-enter-directory-by-slash ()
  (interactive)
  (if (and (equal (string (char-before)) "/")
           (eq (point) (field-end)))
      (zlc-minibuffer-complete)
    (progn (insert "/")
           (setq minibuffer-scroll-window nil))))

(defsubst zlc--do-completion ()
  (if (eq (car (help-function-arglist 'completion--do-completion)) '&optional)
      (completion--do-completion)
    (completion--do-completion (minibuffer-prompt-end) (point-max))))

(defun zlc-minibuffer-complete ()
  "Complete the minibuffer contents as far as possible.
Return nil if there is no valid completion, else t.
If no characters can be completed, display a list of possible completions.
If you repeat this command after it displayed such a list,
select completion orderly."
  (interactive)
  ;; reset when ...
  (unless (or (eq last-command this-command)
              (eq last-command 'zlc-reset)
              (eq last-command 'zlc-select-previous)
              (eq last-command 'zlc-select-previous-vertical)
              (eq last-command 'zlc-select-next)
              (eq last-command 'zlc-select-next-vertical)
              (eq last-command 'zlc-enter-directory-by-slash))
    (setq minibuffer-scroll-window nil))
  (let ((window minibuffer-scroll-window)
        completion-status)
    ;; If there's completions, select one of them orderly.
    (if (window-live-p window)
        (or (zlc-select-next 1) t)
      ;; otherwise, reset completions and arrange new one
      (zlc--reset)
      (case (setq completion-status
                  ;; Suppress `minibuffer-message' in the completion
                  ;; process by setting nil to
                  ;; `completion-show-inline-help', since
                  ;; `minibuffer-message' is a blocking fucntion
                  (let (completion-show-inline-help)
                    (zlc--do-completion)))
        (#b000 (goto-char (field-end))
               (minibuffer-message "No candidates found"))
        (#b001 (goto-char (field-end))
               (minibuffer-message "Sole completion")
               t)
        ((#b011 #b110 #b111)
         (goto-char (field-end))
         ;; immediately display completions
         (minibuffer-completion-help)
         (when (or (eq #b110 completion-status)
                   (eq #b111 completion-status))
           (setq zlc--field-begin (field-end)))
         ;; select first completion if needed
         (when zlc-select-completion-immediately
           (zlc-select-next 1))
         t)
        (t t)))))

;;;###autoload
(defun zlc-mode (&optional arg)
  "Toggle zlc (zsh like completion) on or off.
With ARG, turn zlc on if arg is positive, off otherwise."
  (interactive "P")
  (setq zlc--mode
        (cond
         ((null arg) (not zlc--mode))
         ((eq arg t) t)
         ((> (prefix-numeric-value arg) 0) t)
         (t nil)))
  ;; advice
  (let ((manip (if zlc--mode 'ad-enable-advice 'ad-disable-advice)))
    (funcall manip 'display-completion-list 'after 'zlc--save-global-cache)
    (funcall manip 'minibuffer-complete 'around 'zlc--around-minibuffer-complete))
  (ad-activate 'display-completion-list)
  (ad-activate 'minibuffer-complete)
  ;; ok
  (message "zlc mode %s" (if zlc--mode "enabled" "disabled")))

;; ============================================================ ;;
;; Settings
;; ============================================================ ;;

(let ((map minibuffer-local-map))
  (define-key map (kbd "<backtab>") 'zlc-select-previous)
  (define-key map (kbd "S-<tab>") 'zlc-select-previous)
  (define-key map (kbd "/") 'zlc-enter-directory-by-slash))

(provide 'zlc)
;;; zlc.el ends here
