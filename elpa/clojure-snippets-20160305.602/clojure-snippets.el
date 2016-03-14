;;; clojure-snippets.el --- Yasnippets for clojure

;; Copyright (C) 2013 Max Penet

;; Author: Max Penet <m@qbits.cc>
;; Keywords: snippets
;; Version: 1.0.0
;; Package-Requires: ((yasnippet "0.8.0"))

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

;;; Code:

(setq clojure-snippets-dir (file-name-directory load-file-name))

;;;###autoload
(defun clojure-snippets-initialize ()
  (let ((snip-dir (expand-file-name "snippets" clojure-snippets-dir)))
    (when (boundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs snip-dir t))
    (yas-load-directory snip-dir)))

;;;###autoload
(eval-after-load 'yasnippet
   '(clojure-snippets-initialize))

(require 'yasnippet)

(provide 'clojure-snippets)

;;; clojure-snippets.el ends here
