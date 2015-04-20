;;; ace-flyspell.el --- Jump to and correct spelling errors using `ace-jump-mode' and flyspell

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; URL: https://github.com/cute-jumper/ace-flyspell
;; Package-Version: 20150415.1135
;; Version: 0.1
;; Package-Requires: ((ace-jump-mode "2.0"))
;; Keywords: extensions

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

;; Demos: See https://github.com/cute-jumper/ace-flyspell

;; #+TITLE: ace-flyspell

;; Jump to and correct spelling errors using `ace-jump-mode' and flyspell. Inspired
;; by [[https://github.com/abo-abo/][abo-abo(Oleh Krehel)]]'s [[https://github.com/abo-abo/ace-link][ace-link]].

;; * Setup
;;   : (add-to-list 'load-path "/path/to/ace-flyspell.el")
;;   : (require 'ace-flyspell)

;;   Optional:
;;   : M-x ace-flyspell-setup

;;   If you call =M-x ace-flyspell-setup= , then this setup binds the command
;;   =ace-flyspell-dwim= to =C-.=, which is originally bound to
;;   =flyspell-auto-correct-word= if you enable the =flyspell-mode=. Of course, you
;;   can choose to change the key binding.

;;   Usually, you should enable =flyspell-mode= because this package aims to jump
;;   to and correct spelling errors detected by =flyspell=, or at least you need to
;;   run =flyspell-buffer= to detect spelling errors.

;; * Usage
;;   There are three available commands:
;; ** =ace-flyspell-jump-word=
;;    This command jumps to an spelling error using =ace-jump-word-mode=, and move
;;    the point to where the spelling error is.
;; ** =ace-flyspell-correct-word=
;;    This command is different from =ace-flyspell-jump-word= in the sense that it
;;    aims to *correct* rather than *jump to* the spelling error. At first, it
;;    looks like =ace-flyspell-jump-word=, but after you jump to the misspelt word,
;;    it will enter another /mode/, where you hit =.= to invoke
;;    =flyspell-auto-correct-word= to correct the current misspelt word and hit any
;;    other key to accept the correction and return to the original position. You
;;    can also hit "," to save the current word into personal dictionary.

;;    This command is useful when you're writing an article and want to temporarily
;;    go back to some spelling error and return to where you left off after fixing
;;    the error.
;; ** =ace-flyspell-dwim=
;;    If the word under the cursor is misspelt, then this command is identical to
;;    =flyspell-auto-correct-word=, otherwise it will call
;;    =ace-flyspell-correct-word= to jump to and correct some spelling error.

;;    This command is bound to =C-.= after you call =M-x ace-flyspell-setup=.

;; * Acknowledgment
;;   This package is based on [[https://github.com/winterTTr/ace-jump-mode][winterTTr]]'s awesome [[https://github.com/winterTTr/ace-jump-mode][ace-jump-mode]] package and
;;   inspired by [[https://github.com/abo-abo/ace-link][abo-abo(Oleh Krehel)]]'s [[https://github.com/abo-abo/ace-link][ace-link]] package, from which I borrowed two
;;   convenient macros, and this package is kind of like the a variant of the
;;   original [[https://github.com/abo-abo/ace-link][ace-link]] package although I made some further extensions to meet my
;;   own needs.

;;; Code:

(require 'ace-jump-mode)
(require 'flyspell)

(defgroup ace-flyspell nil
  "Jump to and correct spelling errors using `ace-jump-mode' and flyspell"
  :group 'flyspell)

(defface ace-flyspell--background
  '((t (:box t :bold t)))
  "face for ace-flyspell"
  :group 'ace-flyspell)

(defconst ace-flyspell--ov (let ((ov (make-overlay 1 1 nil nil t)))
                             (overlay-put ov 'face 'ace-flyspell--background)
                             (delete-overlay ov)
                             ov))

(defvar ace-flyspell--original-end-hook ace-jump-mode-end-hook
  "Save the original `ace-jump-mode-end-hook' to cooperate with
  other packages which set this hook")

(defun ace-flyspell--restore-end-hook ()
  "Restore the original `ace-jump-mode-end-hook'."
  (setq ace-jump-mode-end-hook ace-flyspell--original-end-hook)
  (remove-hook 'mouse-leave-buffer-hook 'ace-flyspell--restore-end-hook)
  (remove-hook 'kbd-macro-termination-hook 'ace-flyspell--restore-end-hook))

;; Two convenient macros from `ace-link.el', which is a pacakge written by
;; Oleh Krehel <ohwoeowho@gmail.com>.
;; Original code URL: https://github.com/abo-abo/ace-link
;; I modified the macro name to conform the naming convention
;; Update: 04/14/2015 - Fix end hook problems.
(defmacro ace-flyspell--flet (binding &rest body)
  "Temporarily override BINDING and execute BODY."
  (declare (indent 1))
  (let* ((name (car binding))
         (old (cl-gensym (symbol-name name))))
    `(let ((,old (symbol-function ',name)))
       (unwind-protect
           (progn
             (fset ',name (lambda ,@(cdr binding)))
             ,@body)
         (fset ',name ,old)))))

(defmacro ace-flyspell--generic (candidates &rest follower)
  "Ace jump to CANDIDATES using FOLLOWER."
  (declare (indent 1))
  `(progn
     (add-hook 'mouse-leave-buffer-hook 'ace-flyspell--restore-end-hook)
     (add-hook 'kbd-macro-termination-hook 'ace-flyspell--restore-end-hook)
     (ace-flyspell--flet (ace-jump-search-candidate
                          (str va-list)
                          (mapcar (lambda (x)
                                    (make-aj-position
                                     :offset (1- x)
                                     :visual-area (car va-list)))
                                  ,candidates))
       (setq ace-jump-mode-end-hook
             (list
              (lambda ()
                (ace-flyspell--restore-end-hook)
                ,@follower)))
       (condition-case err
           (let ((ace-jump-mode-scope 'window))
             (ace-jump-do ""))
         (error
          (ace-flyspell--restore-end-hook)
          (signal (car err) (cdr err)))))))
;; End macros from `ace-link.el'

(defun ace-flyspell--collect-candidates ()
  (save-excursion
    (save-restriction
      (narrow-to-region (window-start) (window-end (selected-window) t))
      (let ((pos (point-min))
            (pos-max (point-max))
            (pos-list nil)
            (word t))
        (goto-char pos)
        (while (and word (< pos pos-max))
          (setq word (flyspell-get-word t))
          (when word
            (setq pos (nth 1 word))
            (let* ((ovs (overlays-at pos))
                   (r (ace-flyspell--has-flyspell-overlay-p ovs)))
              (when r
                (push pos pos-list)))
            (setq pos (1+ (nth 2 word)))
            (goto-char pos)))
        (nreverse pos-list)))))

(defun ace-flyspell--has-flyspell-overlay-p (ovs)
  (let ((r nil))
    (while (and (not r) (consp ovs))
      (if (flyspell-overlay-p (car ovs))
          (setq r t)
        (setq ovs (cdr ovs))))
    r))

(defun ace-flyspell-help ()
  (message "[.]: correct word; [,]: save to personal dictionary"))

(defun ace-flyspell--auto-correct-word ()
  (interactive)
  (flyspell-auto-correct-word)
  (ace-flyspell-help))

(defun ace-flyspell--insert-word ()
  (interactive)
  (let* ((word-tuple (save-excursion (flyspell-get-word)))
         (word (car word-tuple))
         (start (cadr word-tuple)))
    (ispell-send-string (concat "*" word "\n"))
    (setq ispell-pdict-modified-p '(t)) ; dictionary modified!
    (when (fboundp 'flyspell-unhighlight-at)
      (flyspell-unhighlight-at start))
    (ispell-pdict-save)
    (ace-flyspell--reset)))

(defun ace-flyspell--reset ()
  (interactive)
  (setq overriding-local-map nil)
  (remove-hook 'mouse-leave-buffer-hook 'ace-flyspell--reset)
  (remove-hook 'kbd-macro-termination-hook 'ace-flyspell--reset)
  (remove-hook 'minibuffer-setup-hook 'ace-flyspell--reset)
  (goto-char (mark))
  (delete-overlay ace-flyspell--ov))

;;;###autoload
(defun ace-flyspell-correct-word ()
  (interactive)
  (ace-flyspell--generic
      (ace-flyspell--collect-candidates)
    (forward-char)
    (let* ((word-length (length (save-excursion (car (flyspell-get-word))))))
      (move-overlay ace-flyspell--ov (point) (+ (point) word-length)))
    (setq overriding-local-map
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd ".") 'ace-flyspell--auto-correct-word)
            (define-key map (kbd ",") 'ace-flyspell--insert-word)
            (define-key map [t] 'ace-flyspell--reset)
            map))
    (add-hook 'mouse-leave-buffer-hook 'ace-flyspell--reset)
    (add-hook 'kbd-macro-termination-hook 'ace-flyspell--reset)
    (add-hook 'minibuffer-setup-hook 'ace-flyspell--reset)
    (ace-flyspell-help)))

;;;###autoload
(defun ace-flyspell-jump-word ()
  (interactive)
  (ace-flyspell--generic
      (ace-flyspell--collect-candidates)
    (forward-char)))

;;;###autoload
(defun ace-flyspell-dwim ()
  (interactive)
  (if (or (and (eq flyspell-auto-correct-pos (point))
               (consp flyspell-auto-correct-region))
          (not (flyspell-word)))
      (flyspell-auto-correct-word)
    (ace-flyspell-correct-word)))

;;;###autoload
(defun ace-flyspell-setup ()
  (interactive)
  (global-set-key (kbd "C-.") 'ace-flyspell-dwim)
  (eval-after-load "flyspell"
    '(define-key flyspell-mode-map (kbd "C-.") 'ace-flyspell-dwim)))

(provide 'ace-flyspell)
;;; ace-flyspell.el ends here
