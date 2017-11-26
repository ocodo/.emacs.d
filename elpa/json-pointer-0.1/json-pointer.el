;;; json-pointer.el --- JSON pointer implementation in Emacs Lisp

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-json-pointer
;; Version: 0.01

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

;; JSON pointer implementation in Emacs Lisp

;;; Code:

(require 'cl-lib)

(defun json-pointer--parse-path (path)
  (let ((paths (split-string path "/" t)))
    (cl-loop for path in paths
             for p1 = (replace-regexp-in-string "~1" "/" path)
             for p2 = (replace-regexp-in-string "~0" "~" path)
             collect
             (if (string-match-p "[[:digit:]]" p2)
                 (string-to-number p2)
               (intern p2)))))

;;;###autoload
(defun json-pointer-get (json path)
  (let ((data (cl-copy-list json))
        (paths (json-pointer--parse-path path)))
    (cl-loop for p in paths
             if (and (consp data) (assoc p data))
             do
             (setq data (assoc-default p data))
             else

             if (and (vectorp data) (integerp p) (> (length data) p))
             do
             (setq data (aref data p))
             else
             return nil

             finally return data)))

(provide 'json-pointer)

;;; json-pointer.el ends here
