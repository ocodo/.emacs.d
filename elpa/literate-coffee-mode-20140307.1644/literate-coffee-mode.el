;;; literate-coffee-mode.el --- major-mode for Literate CoffeeScript -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-literate-coffee-mode
;; Version: 20140307.1644
;; X-Original-Version: 0.02
;; Package-Requires: ((coffee-mode "0.5.0"))

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

;;; Code:

(require 'coffee-mode)

(defun litcoffee-syntax-propertize-function (start end)
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ("^\\(.\\{4\\}\\).*$"
     (0 (ignore
         (let ((head-of-line (match-string 1)))
           (unless (string-match-p "^\\(\t\\| \\{4,\\}\\)" head-of-line)
             (put-text-property (match-beginning 0) (match-end 0)
                                'syntax-table (string-to-syntax "w")))))))
    (coffee-block-strings-delimiter
     (0 (ignore (coffee-syntax-block-strings-stringify))))
    ("\\(?:[^\\]\\)\\(/\\)"
     (1 (ignore
         (let ((ppss (progn
                       (goto-char (match-beginning 1))
                       (syntax-ppss))))
           (when (nth 8 ppss)
             (put-text-property (match-beginning 1) (match-end 1)
                                'syntax-table (string-to-syntax "_")))))))
    (coffee-regexp-regexp (1 (string-to-syntax "_")))
    ("^[[:space:]]\\{4,\\}*\\(###\\)\\([[:space:]]+.*\\)?$"
     (1 (ignore
         (let ((after-triple-hash (match-string-no-properties 2)))
           (when (or (not after-triple-hash)
                     (not (string-match-p "###\\'" after-triple-hash)))
             (put-text-property (match-beginning 1) (match-end 1)
                                'syntax-table (string-to-syntax "!"))))))))
   (point) end))

;;;###autoload
(define-derived-mode litcoffee-mode coffee-mode "LitCoffee"
  "Major mode for editing Literate CoffeeScript."
  (set (make-local-variable 'syntax-propertize-function)
       'litcoffee-syntax-propertize-function))

(provide 'literate-coffee-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.litcoffee\\'" . litcoffee-mode))

;;; literate-coffee-mode.el ends here
