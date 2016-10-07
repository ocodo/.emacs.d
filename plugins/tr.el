;;; tr.el --- perform some functions like tr

;; Author: Jason Milkins <jasonm23@gmail.com>

;; URL: https://github.com/ocodo/.emacs.d/tree/master/plugins/tr.el

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
;;  Squeeze, delete, substitute like the unix command: tr
;;

;;; Code:

;;;###autoload
(defun tr-squeeze-in-region (start end c)
  "Squeeze in the current region (START to END) the same as tr -s C."
  (interactive "r\ncSqueeze Char:")
  (save-mark-and-excursion
   (goto-char start)
   (while (re-search-forward (format "%c+" c) end t)
     (replace-match (char-to-string c) nil nil))))

;;;###autoload
(defun tr-delete-in-region (start end c)
  "Delete in the current region (START to END) the same as tr -d C."
  (interactive "r\ncDelete Char:")
  (save-mark-and-excursion
   (goto-char start)
   (while (re-search-forward (format "%c" c) end t)
     (replace-match "" nil nil))))

;;;###autoload
(defun tr-translate-in-region (start end a b)
  "Translate chars in the current region (START to END) the same as tr A B."
  (interactive "r\ncChar: \ncSubstitute: ")
  (save-mark-and-excursion
   (goto-char start)
   (while (re-search-forward (format "%c" a) end t)
     (replace-match (char-to-string b) nil nil))))

(require 'hydra)

(defhydra tr-hydra (:color
                    blue
                    :body-pre
                    (unless
                        (region-active-p)
                      (error "Region isn't set")))
  "Translate/tr: "
  ("t" tr-translate-in-region "Translate char in region (tr -t char to-char)")
  ("s" tr-squeeze-in-region "Squeeze char in region (tr -s char)")
  ("d" tr-delete-in-region "Delete char in region (tr -d char)"))

(defun tr-bind-hydra ()
  "Bind tr-hydra/body to default keymaping.

C-x t"
  (interactive)
  (global-set-key (kbd "C-x t") 'tr-hydra/body))

(provide 'tr)
;;; tr.el ends here
