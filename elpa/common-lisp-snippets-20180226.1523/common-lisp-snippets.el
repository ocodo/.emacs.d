;;; common-lisp-snippets.el --- Yasnippets for Common Lisp -*- lexical-binding: t; -*-
;;
;; Copyright © 2015–2017 Mark Karpov <markkarpov92@gmail.com>
;;
;; Author: Mark Karpov <markkarpov92@gmail.com>
;; URL: https://github.com/mrkkrp/common-lisp-snippets
;; Version: 0.1.2
;; Package-Requires: ((yasnippet "0.8.0"))
;; Keywords: snippets
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a collection of Yasnippets for Common Lisp.  It includes snippets
;; for top-level forms and (as a bonus) headers for popular free-software
;; licenses: GNU GPL and MIT License.

;;; Code:

(require 'yasnippet)

(defvar common-lisp-snippets-root
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Root directory of Common Lisp snippets.")

;;;###autoload
(defun common-lisp-snippets-initialize ()
  "Initialize Common Lisp snippets, so Yasnippet can see them."
  (let ((dir (expand-file-name "snippets" common-lisp-snippets-root)))
    (when (boundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs dir t))
    (yas-load-directory dir)))

;;;###autoload
(eval-after-load 'yasnippet
  '(common-lisp-snippets-initialize))

(provide 'common-lisp-snippets)

;;; common-lisp-snippets.el ends here
