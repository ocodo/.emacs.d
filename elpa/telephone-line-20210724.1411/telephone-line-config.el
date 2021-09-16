;;; telephone-line-config.el --- Easy config for telephone-line

;; Copyright (C) 2015-2017 Daniel Bordak

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
;;
;; Easy, premade config(s) for telephone-line.

;;; Code:

(require 'telephone-line)

;;;###autoload
(defun telephone-line-evil-config ()
  "Deprecated, just call (telephone-line-mode t) instead."
  (telephone-line-mode t))

(provide 'telephone-line-config)
;;; telephone-line-config.el ends here
