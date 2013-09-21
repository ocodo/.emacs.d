;;; helm-rb.el --- Search Ruby's method by ag and display helm

;; Copyright (C) 2013 by Yuta Yamada
;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/helm-rb
;; Version: 0.0.1
;; Package-Requires: ((helm "20130916") (helm-ag-r "20130917"))
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

(require 'helm)
(require 'helm-ag-r)

(defvar helm-rb-get-methods-path
  (let* ((name "get_methods.rb")
         (path
          (file-name-as-directory
           (file-name-directory (concat "./" name))))
         (file-path
          (expand-file-name (format "%s%s" path name))))
    (if (file-exists-p file-path)
        file-path
      "Failed to set get_methods.rb's path"))
  "A path for get_methods.rb.")

(defvar helm-rb-methods-list nil)

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

(defun helm-rb-setup ()
  (let* ((command
          (concat
           "ruby "
           (shell-quote-argument helm-rb-get-methods-path)))
         (methods-list (split-string
                        (shell-command-to-string command) "\n")))
    (setq helm-rb-methods-list methods-list)))

(defun helm-rb-init ()
  (if helm-rb-methods-list
      helm-rb-methods-list
    (helm-rb-setup)))

;;;###autoload
(defun helm-rb ()
  (interactive)
  (let ((helm-ag-r-user-option "--nocolor"))
    (helm-ag-r-pype
     (concat "ruby " (shell-quote-argument helm-rb-get-methods-path))
     helm-rb-source)))

(provide 'helm-rb)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; helm-rb.el ends here
