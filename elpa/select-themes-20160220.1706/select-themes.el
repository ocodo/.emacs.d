;;; select-themes.el --- Color theme selection with completing-read -*- lexical-binding: t; -*-

;;  Copyright (c) 2016 by Jason Milkins

;; Author: Jason Milkins <jasonm23@gmail.com>
;; URL: https://github.com/jasonm23/emacs-select-themes
;; Package-Version: 20160220.1706
;; Version: 0.1.3

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

;; select-themes.el provides interactive theme selection using the default
;; completing-read interface, differs from M-x `load-theme' by
;; disabling other loaded themes first.

;;; Code:

;;;###autoload
(defun select-themes (theme)
  "Interactively select a THEME, from the available custom themes.

You can also select '*Emacs default*' to return to Emacs default theme.

Note: multiple enabled themes cause Emacs to slow down, so we
disable them before selecting the new theme."
  (interactive (list (completing-read "Select theme: "
                                (sort (custom-available-themes) 'string<)
                                nil nil nil nil
                                "*Emacs default*")))
  (mapc 'disable-theme custom-enabled-themes)
  (unless (string= "*Emacs default*" theme)
    (load-theme (intern-soft theme))))

(provide 'select-themes)
;;; select-themes.el ends here
