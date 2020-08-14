;;; electric-spacing-cc-mode.el --- c-buffer-is tunings

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: William Xu <william.xwl@gmail.com>

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

;;; Code:

(require 'electric-spacing)

(defun electric-spacing-cc-mode-include-line ()
  "Return t if we are on the line of including header files."
  (save-excursion
    (re-search-backward "#include < ?" (line-beginning-position) t 1)))

(defun electric-spacing-cc-mode-hook ()
  "Unset c-electric-* keymaps."
  (dolist (op electric-spacing-operators)
    (local-unset-key (char-to-string op))))

(add-hook 'c-mode-common-hook 'electric-spacing-cc-mode-hook)

(defun electric-spacing-cc-mode-: ()
  (cond ((looking-back ": *" nil)
         (or (re-search-backward " +: *" (line-beginning-position) t 1)
             (re-search-backward ": *" (line-beginning-position) t 1))
         (replace-match "::"))
        ((save-excursion
           (re-search-backward "struct\\|class" (line-beginning-position) t 1))
         (electric-spacing-insert ":"))
        (t
         (electric-spacing-insert ":" 'after)))
  (indent-according-to-mode))

(defun electric-spacing-cc-mode-* ()
  "See `electric-spacing-insert'."
  ;; ,----
  ;; | a * b;
  ;; | char *a;
  ;; | char **b;
  ;; | (*a)->func();
  ;; | *p++;
  ;; | *a = *b;
  ;; | printf("%d", *ip);
  ;; | func(*p);
  ;; `----
  (cond ((looking-back (concat (electric-spacing-c-types) " *") nil)
         (electric-spacing-insert "*" 'before))
        ((looking-back "\\* *" nil)
         (electric-spacing-insert "*" 'middle))
        ((looking-back "^[ (]*" nil)
         (electric-spacing-insert "*" 'middle)
         (indent-according-to-mode))
        ((looking-back "( *" nil)
         (electric-spacing-insert "*" 'middle))
        ((looking-back ", *" nil)
         (electric-spacing-insert "*" 'before))
        ((looking-back "= *" nil)
         (electric-spacing-insert "*" 'before))
        (t
         (electric-spacing-insert "*"))))

(defun electric-spacing-cc-mode-& ()
  "See `electric-spacing-insert'."
  ;; ,----[ cases ]
  ;; | char &a = b; // FIXME
  ;; | void foo(const int& a);
  ;; | char *a = &b;
  ;; | int c = a & b;
  ;; | a && b;
  ;; | scanf ("%d", &i);
  ;; | func(&i)
  ;; `----
  (cond ((looking-back (concat (electric-spacing-c-types) " *") nil)
         (electric-spacing-insert "&" 'after))
        ((looking-back "= *" nil)
         (electric-spacing-insert "&" 'before))
        ((looking-back "( *" nil)
         (electric-spacing-insert "&" 'middle))
        ((looking-back ", *" nil)
         (electric-spacing-insert "&" 'before))
        (t
         (electric-spacing-insert "&" 'middle))))

(defun electric-spacing-cc-mode-< ()
  "See `electric-spacing-insert'."
  (cond ((looking-back "operator<?" nil)
         (insert "<"))
        (t
         (electric-spacing-insert "<"))))

(defun electric-spacing-cc-mode-> ()
  "See `electric-spacing-insert'."
  (cond ((looking-back " - " nil)
         (delete-char -3)
         (insert "->"))
        ((electric-spacing-cc-mode-include-line)
         (save-excursion (replace-match "#include <"))
         (insert ">"))
        ((save-excursion (re-search-backward " < " (line-beginning-position) t 1))
         (save-excursion (replace-match "<"))
         (electric-spacing-insert ">" 'after))
        (t
         (electric-spacing-insert ">"))))

(defun electric-spacing-cc-mode-+ ()
  "See `electric-spacing-insert'."
  (cond ((looking-back "\\+ *" nil)
         (when (looking-back "[a-zA-Z0-9_] +\\+ *" nil)
           (save-excursion
             (backward-char 2)
             (delete-horizontal-space)))
         (electric-spacing-insert "+" 'middle)
         (indent-according-to-mode))
        (t
         (electric-spacing-insert "+"))))

(defun electric-spacing-cc-mode-- ()
  "See `electric-spacing-insert'."
  (cond ((looking-back "\\- *" nil)
         (electric-spacing-insert "-" 'middle))
        ((looking-back (concat electric-spacing-operators-regexp " *") nil)
         (insert "-"))
        (t
         (electric-spacing-insert "-"))))

(defun electric-spacing-cc-mode-? ()
  "See `electric-spacing-insert'."
  (electric-spacing-insert "?"))

(defun electric-spacing-cc-mode-% ()
  "See `electric-spacing-insert'."
  ;; ,----
  ;; | a % b;
  ;; | printf("%d %d\n", a % b);
  ;; `----
  (if (and (looking-back "\".*" nil)
           (not (looking-back "\",.*" nil)))
      (insert "%")
    (electric-spacing-insert "%")))

(defun electric-spacing-cc-mode-\( ()
  (electric-spacing-insert "(" 'middle))

(defun electric-spacing-cc-mode-. ()
  (electric-spacing-insert "." 'middle))

(defun electric-spacing-cc-mode-/ ()
  (if (electric-spacing-cc-mode-include-line)
      (insert "/")
    (electric-spacing-insert "/")))

(defun electric-spacing-cc-mode-! ()
  "See `electric-spacing-insert'."
  (if (looking-back "(" nil)
      (electric-spacing-insert "!" 'middle)
    (electric-spacing-insert "!" 'before)))

(defun electric-spacing-cc-mode-\; ()
  "See `electric-spacing-insert'."
  (electric-spacing-insert ";" 'after)
  (indent-according-to-mode)
  (unless (save-excursion
            (re-search-backward "for ?(" (line-beginning-position) t 1))
    (newline)
    (indent-according-to-mode)))

(provide 'electric-spacing-cc-mode)
;;; electric-spacing-cc-mode.el ends here
