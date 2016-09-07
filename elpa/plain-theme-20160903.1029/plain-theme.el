;;; plain-theme.el --- Black and white theme without syntax highlighting

;; Copyright (C) 2016 Yegor Timoshenko

;; Author: Yegor Timoshenko <yegortimoshenko@gmail.com>
;; URL: https://github.com/yegortimoshenko/plain-theme
;; Package-Version: 20160903.1029
;; Version: 1

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

;;; Commentary:

;; To install, enable MELPA (http://melpa.org/#/getting-started) and execute:
;; M-x package-install plain-theme

;;; Code:

(deftheme plain)

(defvar plain-faces
  `(default
    font-lock-builtin-face
    font-lock-comment-delimiter-face
    font-lock-comment-face
    font-lock-constant-face
    font-lock-doc-face
    font-lock-function-name-face
    font-lock-keyword-face
    font-lock-negation-char-face
    font-lock-function-name-face
    font-lock-keyword-face
    font-lock-negation-char-face
    font-lock-preprocessor-face
    font-lock-regexp-grouping-backslash
    font-lock-regexp-grouping-construct
    font-lock-string-face
    font-lock-type-face
    font-lock-variable-name-face
    font-lock-warning-face
    fringe
    mode-line))

(let ((black "#000")
      (white "#fff"))
  (apply 'custom-theme-set-faces 'plain
   `(mode-line ((t (:box (:line-width 1)))))
   `(cursor ((t (:background ,black :foreground ,white))))
    (mapcar (lambda (n) `(,n ((t (:background ,white :foreground ,black))))) plain-faces)))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'plain)

;;; plain-theme.el ends here
