;;; processing-snippets.el --- Snippets for the Processing major mode

;; Copyright (C) 2013  Peter Vasil

;; Author: Peter Vasil <mail@petervasil.net>
;; Version 1.0.0
;; Keywords: snippets
;; URL: https://github.com/ptrv/processing2-emacs
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

(require 'yasnippet)

(defvar processing-snippets-root (file-name-directory (or load-file-name
                                                          (buffer-file-name))))

;;;###autoload
(defun processing-snippets-initialize ()
  (let ((snip-dir (expand-file-name "snippets" processing-snippets-root)))
    (if (file-exists-p snip-dir)
        (progn
          (when (fboundp 'yas-snippet-dirs)
            (add-to-list 'yas-snippet-dirs snip-dir t))
          (yas-load-directory snip-dir t))
      (user-error "Error: Porcessing snippets dir %s is invalid!" snip-dir))))

;;;###autoload
(eval-after-load 'yasnippet
  '(processing-snippets-initialize))

(provide 'processing-snippets)
;;; processing-snippets.el ends here
