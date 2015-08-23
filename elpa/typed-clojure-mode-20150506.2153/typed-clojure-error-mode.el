;;; typed-clojure-error-mode.el --- Typed Clojure Error navigator

;; Copyright © 2014 John Walker
;;
;; Author: John Walker <john.lou.walker@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Adds syntax coloring for type reports from typed-clojure-mode

;;; Code:

(require 'cider-interaction)

(defvar typed-clojure-error-mode-keywords
      '(("^Type Error\\|Internal Error" . font-lock-constant-face)
	("^Hint:\\|^in: \\|^with expected type:$\\|^Arguments:$\\|^Domains:$\\|^Ranges:$" . font-lock-variable-name-face)))

(define-derived-mode typed-clojure-error-mode cider-popup-buffer-mode
  "CTYP"
  "Major mode for typed-clojure errors"
  (setq font-lock-defaults '(typed-clojure-error-mode-keywords)))

(provide 'typed-clojure-error-mode)

;;; typed-clojure-error-mode.el ends here
