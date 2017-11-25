;;; robots-txt-mode.el --- Major mode for editing robots.txt

;; Copyright (C) 2016 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 9 Mar 2016
;; Version: 0.0.2
;; Package-Version: 20170908.642
;; Keywords: languages, comm, web
;; URL: https://github.com/emacs-php/robots-txt-mode

;; This file is NOT part of GNU Emacs.

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

;; This is a major-mode for editing `robots.txt'.

;;; Code:

(defvar robots-txt-mode-directives
  '("Allow"    ; Google
    "Disallow" ; Standard
    "Sitemap")
  "Directive keywords for robots.")

(defconst robots-txt-mode-syntax-keywords
  (list
   (cons ; Line Comment
    "^ *\\(#.+\\)"
    '((1 font-lock-comment-face)))
   (cons
    (concat
     "^\\(User-agent:\\)"
     " "
     "\\(.+\\)")
    '((1 font-lock-keyword-face)
      (2 font-lock-function-name-face)))
   (cons ; Directive
    (concat
     "^\\(" (regexp-opt robots-txt-mode-directives) ":\\)"
     " "
     "\\(.+\\)")
    '((1 font-lock-type-face)
      (2 font-lock-string-face)))
   (cons ; Non standard directive
    (concat
     "^\\(\\(?:[-a-zA-Z]\\)+:\\)"
     " +"
     "\\(.+\\)")
    '((1 font-lock-builtin-face)
      (2 font-lock-string-face)))))

;;;###autoload
(define-derived-mode robots-txt-mode prog-mode "[o_-]"
  "Major mode for editing `robots.txt'"
  (setq font-lock-defaults '(robots-txt-mode-syntax-keywords nil t)))

;;;###autoload
(add-to-list 'auto-mode-alist '("/robots\\.txt\\'" . robots-txt-mode))

(provide 'robots-txt-mode)
;;; robots-txt-mode.el ends here
