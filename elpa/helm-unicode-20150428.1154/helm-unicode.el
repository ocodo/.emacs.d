;;; helm-unicode.el --- Helm command for unicode characters. -*- lexical-binding: t -*-

;; Copyright © 2015 Emanuel Evans

;; Version: 0.0.1
;; Package-Version: 20150428.1154
;; Package-Requires: ((helm "1.6") (emacs "24.4"))

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
;; A helm command for looking up unicode characters by name 😉.

;;; Code:

(require 'helm)
(require 'helm-utils)

(defvar helm-unicode-names nil
  "Internal variable for unicode characters.")

(defvar helm-source-unicode
  '((name . "Unicode Characters")
    (init . (lambda ()
              (unless helm-unicode-names
                (setq helm-unicode-names
                      (sort (mapcar (lambda (char-pair)
                                      (format "%s %c"
                                              (car char-pair)
                                              (cdr char-pair)))
                                    (ucs-names))
                            #'string-lessp)))
              (helm-init-candidates-in-buffer 'global
                helm-unicode-names)))
    (candidates-in-buffer)
    (persistent-action . ignore)
    (filtered-candidate-transformer . (lambda (candidates _source)
                                        (sort candidates
                                              #'helm-generic-sort-fn)))
    (action . (("Insert Character" . helm-unicode-insert-char))))
  "Helm source for all unicode characters.")

(defun helm-unicode-insert-char (candidate)
  "Insert CANDIDATE into the main buffer."
  (insert (substring candidate -1)))

;;;###autoload
(defun helm-unicode (arg)
  "Precofigured `helm' for looking up unicode characters by name.

With prefix ARG, reinitialize the cache."
  (interactive "P")
  (when arg (setq helm-unicode-names nil))
  (helm-other-buffer 'helm-source-unicode "*Helm unicode*"))

(provide 'helm-unicode)

;;; helm-unicode.el ends here
