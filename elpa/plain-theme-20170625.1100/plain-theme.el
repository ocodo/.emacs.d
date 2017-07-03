;;; plain-theme.el --- Plain theme without syntax highlighting

;; Copyright (C) 2016 Yegor Timoshenko

;; Author: Yegor Timoshenko <yegortimoshenko@gmail.com>
;; URL: https://github.com/yegortimoshenko/plain-theme
;; Package-Version: 20170625.1100
;; Version: 7

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED AS IS AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
;; REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
;; FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
;; INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
;; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
;; OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.

;;; Code:

(deftheme plain "Plain theme without syntax highlighting.")

(defgroup plain-theme nil
  "Plain theme colors and faces."
  :group 'faces
  :prefix "plain-")

(defcustom plain-background "white"
  "Color to use for background."
  :type 'color)

(defcustom plain-foreground "black"
  "Color to use for text."
  :type 'color)

(defcustom plain-faces '(cursor default eshell-prompt fringe minibuffer-prompt)
  "List of faces to decolorize."
  :type '(repeat symbol))

(defun plain--spec (face)
  "Return spec for a FACE."
  `(,face ((t (:background ,plain-background :foreground ,plain-foreground)))))

(defun plain--add (faces)
  "Add FACES to the theme definition."
  (apply 'custom-theme-set-faces 'plain (mapcar 'plain--spec faces)))

(plain--add plain-faces)

(defcustom plain-prefix-alist
  '((font-lock . "font-lock-")
    (sh-script . "sh-")
    (web-mode . "web-mode-"))
  "Mapping from files to face prefixes: when file is first loaded,
decolorizes every face that starts with the prefix."
  :type '(alist :key-type symbol :value-type string))

(require 'cl-lib)

(defun plain--prefix (prefix)
  "Return all faces that start with PREFIX."
  (cl-remove-if-not (lambda (s) (string-prefix-p prefix (symbol-name s)))
		    (face-list)))

(dolist (a plain-prefix-alist)
  (eval-after-load (car a) `(plain--add (plain--prefix ,(cdr a)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'plain)

;;; plain-theme.el ends here
