;;; flymake-tip.el --- show flymake's error by popup-tip -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/flycheck-tip
;; Version: 0.5.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: flymake

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

;;; Code:

(require 'error-tip)
(require 'flymake)
(require 'cl-lib)

(defvar flymake-tip--err-info-function
  (cond
   ((version<= "26" (number-to-string emacs-major-version))
    '(lambda ()
       ;; Return list of string of error/warning info on the current cursor
       (cl-loop for diag in (flymake-diagnostics (point-at-bol) (point-at-eol))
                collect (flymake-diagnostic-text diag))))

   ((fboundp 'flymake-find-err-info)
    '(lambda ()
       ;; Old implementation for emacs-major-version < 26
       (cl-loop
        with errors = (flymake-find-err-info flyamke-err-info (line-number-at-pos))
        for err in (car errors)
        if (vectorp err)
        collect (elt err 4))))))

;;;###autoload
(defun flymake-tip-cycle (reverse)
  (interactive)
  (let ((jump (lambda ()
                (if reverse
                    (flymake-goto-prev-error)
                  (flymake-goto-next-error)))))
    (let ((line (line-number-at-pos)))
      (funcall jump)
      (when (eq line (line-number-at-pos))
        (goto-char (if reverse (point-at-bol) (point-at-eol)))
        (funcall jump))))
  (error-tip-popup-error-message
   (funcall flymake-tip--err-info-function)))

;;;###autoload
(defun flymake-tip-cycle-reverse ()
  (interactive)
  (flymake-tip-cycle t))

(provide 'flymake-tip)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; flymake-tip.el ends here
