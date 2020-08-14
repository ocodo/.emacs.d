;;; edit-at-point.el --- edit(copy,cut..) current things(word,symbol..) under cursor

;; Author: <e.enoson@gmail.com>
;; License: MIT
;; URL: http://github.com/enoson/edit-at-point.el
;; Package-Version: 20191013.1218
;; Package-Commit: 28c85a65c9c61f2aff50bc5e93f61cde26a5d9c0
;; Version: 1.1

;;; Commentary:

;; copy/cut/delete/paste : word,symbol,string,line,parenthesis((),[], {}),function definition
;; duplicate : line,paren,defun
;; move up/down : line

;; sample keybinding config:
;; (require 'bind-key)
;; (bind-keys
;;   ("C-S-a". edit-at-point-word-copy)
;;   ("C-S-b". edit-at-point-word-cut)
;;   ("C-S-c". edit-at-point-word-delete)
;;   ("C-S-d". edit-at-point-word-paste)
;;   ("C-S-e". edit-at-point-symbol-copy)
;;   ("C-S-f". edit-at-point-symbol-cut)
;;   ("C-S-g". edit-at-point-symbol-delete)
;;   ("C-S-h". edit-at-point-symbol-paste)
;;   ("C-S-i". edit-at-point-str-copy)
;;   ("C-S-j". edit-at-point-str-cut)
;;   ("C-S-k". edit-at-point-str-delete)
;;   ("C-S-l". edit-at-point-str-paste)
;;   ("C-S-m". edit-at-point-line-copy)
;;   ("C-S-n". edit-at-point-line-cut)
;;   ("C-S-o". edit-at-point-line-delete)
;;   ("C-S-p". edit-at-point-line-paste)
;;   ("C-S-q". edit-at-point-line-dup)
;;   ("C-S-r". edit-at-point-line-up)
;;   ("C-S-s". edit-at-point-line-down)
;;   ("C-S-t". edit-at-point-paren-copy)
;;   ("C-S-u". edit-at-point-paren-cut)
;;   ("C-S-v". edit-at-point-paren-delete)
;;   ("C-S-w". edit-at-point-paren-paste)
;;   ("C-S-x". edit-at-point-paren-dup)
;;   ("C-S-y". edit-at-point-defun-copy)
;;   ("C-S-z". edit-at-point-defun-cut)
;;   ("C-{"  . edit-at-point-defun-delete)
;;   ("C-:"  . edit-at-point-defun-paste)
;;   ("C-\"" . edit-at-point-defun-dup))

;;; Code:
(require 'thingatpt) ;bounds-of-thing-at-point

(defun edit-at-point (thing action)
  (while (not (setq b (bounds-of-thing-at-point thing)))
    (backward-char))
  (funcall action (car b) (cdr b)))

;;;###autoload
(defun edit-at-point-word-copy ()
  (interactive)
  (edit-at-point 'word 'kill-ring-save))

;;;###autoload
(defun edit-at-point-word-cut ()
  (interactive)
  (edit-at-point 'word 'kill-region))

;;;###autoload
(defun edit-at-point-word-delete ()
  (interactive)
  (edit-at-point 'word 'delete-region))

;;;###autoload
(defun edit-at-point-word-paste ()
  (interactive)
  (edit-at-point-word-delete)
  (yank))

;;;###autoload
(defun edit-at-point-symbol-copy ()
  (interactive)
  (edit-at-point 'symbol 'kill-ring-save))

;;;###autoload
(defun edit-at-point-symbol-cut ()
  (interactive)
  (edit-at-point 'symbol 'kill-region))

;;;###autoload
(defun edit-at-point-symbol-delete ()
  (interactive)
  (edit-at-point 'symbol 'delete-region))

;;;###autoload
(defun edit-at-point-symbol-paste ()
  (interactive)
  (edit-at-point-symbol-delete)
  (yank))

(defun edit-str-at-point (action)
  (setq p (point))
  (forward-char)
  (ignore-errors
    (while (not (nth 3 (syntax-ppss)))
      (backward-char)))
  (if (= 1 (point))
      (goto-char p)
    (goto-char (nth 8 (syntax-ppss)))
    (edit-at-point 'sexp action)))

;;;###autoload
(defun edit-at-point-str-copy ()
  (interactive)
  (edit-str-at-point 'kill-ring-save))

;;;###autoload
(defun edit-at-point-str-cut ()
  (interactive)
  (edit-str-at-point 'kill-region))

;;;###autoload
(defun edit-at-point-str-delete ()
  (interactive)
  (edit-str-at-point 'delete-region))

;;;###autoload
(defun edit-at-point-str-paste ()
  (interactive)
  (edit-at-point-str-delete)
  (yank))

;;;###autoload
(defun edit-at-point-line-copy ()
  (interactive)
  (edit-at-point 'line 'kill-ring-save))

;;;###autoload
(defun edit-at-point-line-cut ()
  (interactive)
  (setq c (current-column))
  (edit-at-point 'line 'kill-region)
  (move-to-column c))

;;;###autoload
(defun edit-at-point-line-delete ()
  (interactive)
  (setq c (current-column))
  (edit-at-point 'line 'delete-region)
  (move-to-column c))

;;;###autoload
(defun edit-at-point-line-paste ()
  (interactive)
  (setq c (current-column))
  (edit-at-point 'line 'delete-region)
  (yank)
  (move-to-column c))

;;;###autoload
(defun edit-at-point-line-dup ()
  (interactive)
  (save-excursion
    (edit-at-point-line-copy)
    (beginning-of-line)
    (yank)))

;;;###autoload
(defun edit-at-point-line-down (arg)
  (interactive "p")
  (setq c (current-column))
  (edit-at-point-line-cut)
  (forward-line arg)
  (yank)
  (if arg (forward-line -1))
  (move-to-column c))

;;;###autoload
(defun edit-at-point-line-up ()
  (interactive)
  (edit-at-point-line-down -1))

(defun --is-newline-at-point ()
  (or (not (char-after)) (= 10 (char-after))))

(defun --goto-str-beg ()
  (let ((s (syntax-ppss)))
    (if (nth 3 s)
        (goto-char (nth 8 s)))))

(defun edit-paren-at-point (action)
  (setq char (char-after))
  (if (member char (string-to-list "([{"))
      (forward-char))
  (--goto-str-beg)
  (edit-at-point 'list action))

;;;###autoload
(defun edit-at-point-paren-copy ()
  (interactive)
  (save-excursion (edit-paren-at-point 'kill-ring-save)
                  (princ (list-at-point))))

;;;###autoload
(defun edit-at-point-paren-cut ()
  (interactive)
  (edit-paren-at-point 'kill-region))

;;;###autoload
(defun edit-at-point-paren-delete ()
  (interactive)
  (edit-paren-at-point 'delete-region))

;;;###autoload
(defun edit-at-point-paren-comment ()
  (interactive)
  (edit-paren-at-point 'comment-or-uncomment-region))

;;;###autoload
(defun edit-at-point-paren-paste ()
  (interactive)
  (edit-at-point-paren-delete)
  (yank))

;;;###autoload
(defun edit-at-point-paren-dup ()
  (interactive)
  (setq nl (--is-newline-at-point))
  (edit-at-point-paren-copy)
  (--goto-str-beg)
  (ignore-errors (up-list))
  (newline-and-indent)
  (yank)
  (if nl (newline-and-indent)))

;;;###autoload
(defun edit-at-point-defun-copy ()
  (interactive)
  (edit-at-point 'defun 'kill-ring-save))

;;;###autoload
(defun edit-at-point-defun-cut ()
  (interactive)
  (edit-at-point 'defun 'kill-region))

;;;###autoload
(defun edit-at-point-defun-delete ()
  (interactive)
  (edit-at-point 'defun 'delete-region))

;;;###autoload
(defun edit-at-point-defun-paste ()
  (interactive)
  (edit-at-point-defun-delete)
  (yank))

;;;###autoload
(defun edit-at-point-defun-dup ()
  (interactive)
  (edit-at-point-defun-copy)
  (end-of-thing 'defun)
  (newline-and-indent)
  (yank))

(provide 'edit-at-point)

;;; edit-at-point.el ends here
