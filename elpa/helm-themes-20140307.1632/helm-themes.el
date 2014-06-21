;;; helm-themes.el --- Color theme selection with helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-themes
;; Version: 20140307.1632
;; X-Original-Version: 0.03
;; Package-Requires: ((helm "1.0"))

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

;; helm-themes.el provide theme selection with helm interface.
;; Its persistent action can set theme temporary.
;;

;;; Code:

(require 'helm-config)

(defun helm-c-themes-delete-theme ()
  (mapc 'disable-theme custom-enabled-themes))

(defun helm-c-themes-load-theme (theme-str)
  (helm-c-themes-delete-theme)
  (load-theme (intern theme-str) t))

(defvar helm-themes-source
  '((name . "Selection Theme")
    (candidates . custom-available-themes)
    (action . helm-c-themes-load-theme)
    (persistent-action . helm-c-themes-load-theme)))

;;;###autoload
(defun helm-themes ()
  "Theme selection with helm interface"
  (interactive)
  (let ((changed nil)
        (orig-theme (when custom-enabled-themes
                      (car custom-enabled-themes))))
    (unwind-protect
        (progn
          (when (helm :sources helm-themes-source :buffer "*helm-themes*")
            (setq changed t)))
      (when (not changed)
        (helm-c-themes-delete-theme)
        (when orig-theme
          (load-theme orig-theme t))))))

(provide 'helm-themes)

;;; helm-themes.el ends here
