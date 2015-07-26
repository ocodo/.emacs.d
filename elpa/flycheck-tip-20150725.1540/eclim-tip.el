;;; eclim-tip.el --- show emacs-eclim's error by popup-tip

;; Copyright (C) 2014 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/flycheck-tip
;; Package-Requires: ((popup "0.5.0"))

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
;; This program show ecrim's errors by popup-tip.
;; see also emacs-eclim : https://github.com/senny/emacs-eclim

;;; Code:
(eval-when-compile (require 'cl))
(require 'flycheck-tip)
;; to avoid warning `eclim-problems-highlight'
(with-no-warnings (require 'eclim))

;; memo
;; eclim(eclim--problems-filtered's) error elements
;; warning
;; endColumn
;; endLine
;; column
;; line
;; filename
;; message

;;;###autoload
(defun eclim-tip-cycle (&optional reverse)
  (interactive)
  (error-tip-cycle
   (error-tip-collect-current-file-errors
    (append (eclim--problems-filtered) nil)) reverse))

;;;###autoload
(defun eclim-tip-cycle-reverse ()
  (interactive)
  (error-tip-cycle t))

(provide 'eclim-tip)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; eclim-tip.el ends here
