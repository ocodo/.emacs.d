;;; helm-rb.el --- Search Ruby's method by ag and display helm

;; Copyright (C) 2013 by Yuta Yamada
;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/helm-rb
;; Version: 0.0.1
;; Package-Requires: ((helm "1.0") (helm-ag-r "20131123"))
;; Keywords: Searching Ruby

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:
;; See readme.md
;;; Code:

(require 'helm)
(require 'helm-ag-r)

(defvar helm-rb-get-methods-program
  (let ((current (or load-file-name (buffer-file-name))))
    (expand-file-name "get_methods.rb" (file-name-directory current)))
  "The path to the program `get_methods.rb'.")

(defvar helm-rb-source
  '((name . "helm-rb")
    (candidate-in-buffer)
    (action . helm-rb-action)))

(defun helm-rb-action (line)
  (pop-to-buffer "*ri(helm-rb)*")
  (erase-buffer)
  (insert
   (shell-command-to-string
    (concat "ri -f markdown "
            (shell-quote-argument line))))
  (goto-char (point-min)))

;;;###autoload
(defun helm-rb ()
  "Search Ruby's method by using helm and ag.
If you want to change methods searching program, you can specify
the program's path to `helm-rb-get-methods-program' variable."
  (interactive)
  (let ((helm-ag-r-user-option "--nocolor"))
    (helm-ag-r-pype
     (concat "ruby " (shell-quote-argument helm-rb-get-methods-program))
     helm-rb-source)))

(provide 'helm-rb)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; helm-rb.el ends here
