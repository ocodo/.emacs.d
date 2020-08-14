;;; electric-spacing.el --- Insert operators with surrounding spaces smartly

;; Copyright (C) 2004, 2005, 2007-2015 Free Software Foundation, Inc.

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 6.0b

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Electric spacing mode is a minor mode which automatically inserts
;; surrounding spaces around operator symbols.  For example, `='
;; becomes ` = ', `+=' becomes ` += '.  This is most handy for writing
;; C-style source code.
;;
;; Type `M-x electric-spacing-mode' to toggle this minor mode.

;;; Acknowledgements

;; Nikolaj Schumacher <n_schumacher@web.de>, for suggesting
;; reimplementing as a minor mode and providing an initial patch for
;; that.

;;; Code:

(require 'cc-mode)
(require 'thingatpt)

;;; electric-spacing minor mode

(defcustom electric-spacing-double-space-docs nil
  "Enable double spacing of . in document lines - e,g, type '.' => get '.  '."
  :type 'boolean
  :group 'electricity)

(defcustom electric-spacing-docs t
  "Enable electric-spacing in strings and comments."
  :type 'boolean
  :group 'electricity)

(defcustom electric-spacing-control-statement-parens t
  "Enable electric-spacing for '(' in control statements like if, for, while, etc.

See `electric-spacing-parens' to enable everywhere."
  :type 'boolean
  :group 'electricity)

(defcustom electric-spacing-parens nil
  "Enable electric-spacing for '(' everywhere.

See `electric-spacing-control-statement-parens'
to enable only in control statements."
  :type 'boolean
  :group 'electricity)

(defvar electric-spacing-rules
  '((?= . electric-spacing-self-insert-command)
    (?< . electric-spacing-<)
    (?> . electric-spacing->)
    (?% . electric-spacing-%)
    (?+ . electric-spacing-+)
    (?- . electric-spacing--)
    (?* . electric-spacing-*)
    (?/ . electric-spacing-/)
    (?& . electric-spacing-&)
    (?| . electric-spacing-self-insert-command)
    (?: . electric-spacing-:)
    (?? . electric-spacing-?)
    (?, . electric-spacing-\,)
    (?~ . electric-spacing-~)
    (?. . electric-spacing-.)
    (?! . electric-spacing-!)
    (?\( . electric-spacing-left-paren)
    (?^ . electric-spacing-self-insert-command)))

(defvar electric-spacing-operators '(?= ?< ?> ?% ?+ ?- ?* ?/ ?& ?| ?: ?? ?, ?~ ?. ?\( ?^ ?\; ?!))

(defun electric-spacing-post-self-insert-function ()
  (when (and (electric-spacing-should-run?)
             (memq last-command-event electric-spacing-operators))
    (goto-char (electric--after-char-pos))
    (delete-char -1)
    (let ((fn (electric-spacing-find-mode-specific-tuning last-command-event)))
      (if fn
          (funcall fn)
        (electric-spacing-insert last-command-event)))))

;;;###autoload
(define-minor-mode electric-spacing-mode
  "Toggle automatic surrounding space insertion (Electric Spacing mode).
With a prefix argument ARG, enable Electric Spacing mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

This is a local minor mode.  When enabled, typing an operator automatically
inserts surrounding spaces.  e.g., `=' becomes ` = ',`+=' becomes ` += '.  This
is very handy for many programming languages."
  :global nil
  :group 'electricity
  :lighter " _+_"

  ;; body
  (require 'electric-spacing-init)
  (if electric-spacing-mode
      (add-hook 'post-self-insert-hook
                #'electric-spacing-post-self-insert-function nil t)
    (remove-hook 'post-self-insert-hook
                 #'electric-spacing-post-self-insert-function t)))

(defun electric-spacing-self-insert-command ()
  "Insert character with surrounding spaces."
  (electric-spacing-insert (string last-command-event)))

(defun electric-spacing-insert (op &optional only-where)
  "See `electric-spacing-insert-1'."
  (delete-horizontal-space)
  (cond ((and (electric-spacing-lispy-mode?)
              (not (electric-spacing-document?)))
         (electric-spacing-lispy op))
        (t
         (electric-spacing-insert-1 op only-where))))

(defun electric-spacing-insert-1 (op &optional only-where)
  "Insert operator OP with surrounding spaces.
e.g., `=' becomes ` = ', `+=' becomes ` += '.

When `only-where' is 'after, we will insert space at back only;
when `only-where' is 'before, we will insert space at front only;
when `only-where' is 'middle, we will not insert space."
  (pcase only-where
    (`before (insert " " op))
    (`middle (insert op))
    (`after (insert op " "))
    (_
     (let ((begin? (bolp)))
       (unless (or (looking-back (regexp-opt
                                  (mapcar 'char-to-string
                                          (mapcar 'car electric-spacing-rules)))
                                 (line-beginning-position))
                   begin?)
         (insert " "))
       (insert op " ")
       (when begin?
         (indent-according-to-mode))))))

(defun electric-spacing-c-types ()
  (concat c-primitive-type-key "?"))

(defun electric-spacing-document? ()
  (nth 8 (syntax-ppss)))

(defun electric-spacing-should-run? ()
  (or (not electric-spacing-docs)
      (not (electric-spacing-document?))))

(defun electric-spacing-lispy-mode? ()
  (derived-mode-p 'emacs-lisp-mode
                  'lisp-mode
                  'lisp-interaction-mode
                  'scheme-mode))

(defun electric-spacing-lispy (op)
  "We're in a Lisp-ish mode, so let's look for parenthesis.
Meanwhile, if not found after ( operators are more likely to be function names,
so let's not get too insert-happy."
  (cond
   ((save-excursion
      (backward-char 1)
      (looking-at "("))
    (if (equal op ",")
        (electric-spacing-insert-1 op 'middle)
      (electric-spacing-insert-1 op 'after)))
   ((equal op ",")
    (electric-spacing-insert-1 op 'before))
   (t
    (electric-spacing-insert-1 op 'middle))))

(defconst electric-spacing-operators-regexp
  (regexp-opt
   (mapcar (lambda (el) (char-to-string (car el)))
           electric-spacing-rules)))

(defun electric-spacing-get-fun-throw (char mode tag)
  (setq fn (intern (format "electric-spacing%s-%c"
                           (if mode (format "-%s" mode) "")
                           char)))
  (when (fboundp fn)
    (throw tag fn)))

(defun electric-spacing-find-mode-specific-tuning (char)
  (catch 'ret
    ;; mode specific
    (let ((mode major-mode))
      (while mode
        (electric-spacing-get-fun-throw char mode 'ret)
        (setq mode (get mode 'derived-mode-parent))))
    ;; cc-mode
    (when c-buffer-is-cc-mode
      (electric-spacing-get-fun-throw char 'cc-mode 'ret))
    ;; common
    (electric-spacing-get-fun-throw char nil 'ret)))


;;; Fine Tunings

(defun electric-spacing-\, ()
  "See `electric-spacing-insert'."
  (electric-spacing-insert "," 'after))

(defun electric-spacing-\; ()
  "See `electric-spacing-insert'."
  (electric-spacing-insert ";" 'after)
  (indent-according-to-mode))

(defun electric-spacing-left-paren ()
  "See `electric-spacing-insert'."
  (cond ((derived-mode-p 'emacs-lisp-mode)
	 ;; Do nothing in Emacs lisp mode
	 (insert "("))
	((looking-back "[,;] *" nil)
	 (electric-spacing-insert "(" 'before))
	((looking-back "[({!~] *" nil)
	 (electric-spacing-insert "(" 'middle))
	((or electric-spacing-parens
	     (and electric-spacing-control-statement-parens
		  (looking-back
		   (concat "\\("
			   (regexp-opt
			    '("if" "elif" "switch" "for" "while"))
			   "\\)\\ *")
		   (line-beginning-position))))
	 (electric-spacing-insert "(" 'before))
	(t
	 (insert "("))))


(defun electric-spacing-. ()
  "See `electric-spacing-insert'."
  (cond ((and electric-spacing-double-space-docs
              (electric-spacing-document?))
         (electric-spacing-insert "." 'after)
         (insert " "))
        ((or (looking-back "[0-9]" nil)
             (or (and
                  (derived-mode-p 'python-mode 'ruby-mode)
                  (looking-back "[a-z\)]" nil))
                 (and
                  (derived-mode-p 'js-mode 'js2-mode)
                  (looking-back "[a-z\)$]" nil))))
         (insert "."))
        ((derived-mode-p 'cperl-mode 'perl-mode 'ruby-mode)
         ;; Check for the .. range operator
         (if (looking-back "." nil)
             (insert ".")
           (insert " . ")))
        (t
         (electric-spacing-insert "." 'after))))

(defun electric-spacing-+ ()
  "See `electric-spacing-insert'."
  (cond ;; func(++i);
        ((looking-back "( *" nil)
         (electric-spacing-insert "+" 'middle))

        ;; j = ++i;
        ((looking-back "= *" nil)
         (electric-spacing-insert "+" 'before))

        (t
         (electric-spacing-insert "+"))))

(defun electric-spacing-- ()
  "See `electric-spacing-insert'."
  (cond ;; exponent notation, e.g. 1e-10: don't space
        ((looking-back "[0-9.]+[eE]" nil)
         (insert "-"))

        ;; a = -9
        ((and (looking-back (concat electric-spacing-operators-regexp " *") nil)
              (not (looking-back "- *" nil)))
          (electric-spacing-insert "-" 'before))

        ;; func(--i)
        ((looking-back "( *" nil)
         (electric-spacing-insert "-" 'middle))

        (t
         (electric-spacing-insert "-"))))

(defun electric-spacing-? ()
  "See `electric-spacing-insert'."
  (electric-spacing-insert "?" 'after))

(defun electric-spacing-! ()
  "See `electric-spacing-insert'."
  (electric-spacing-insert "!" 'middle))

(defun electric-spacing-~ ()
  "See `electric-spacing-insert'."
  ;; First class regex operator =~ langs
  (cond ((derived-mode-p 'ruby-mode 'perl-mode 'cperl-mode)
         (if (looking-back "= " nil)
             (progn
               (delete-char -2)
               (insert "=~ "))
           (insert "~")))
        (t
         (insert "~"))))

(defun electric-spacing-/ ()
  "See `electric-spacing-insert'."
  ;; *nix shebangs #!
  (cond ((and (eq 1 (line-number-at-pos))
              (save-excursion
                (move-beginning-of-line nil)
                (looking-at "#!")))
         (insert "/"))
        (t
         (electric-spacing-insert "/"))))

(defun electric-spacing-enclosing-paren ()
  "Return the opening parenthesis of the enclosing parens, or nil if not inside any parens."
  (interactive)
  (let ((ppss (syntax-ppss)))
    (when (nth 1 ppss)
      (char-after (nth 1 ppss)))))

(provide 'electric-spacing)

;;; electric-spacing.el ends here
