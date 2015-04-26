;;; svg-mode-line-themes.el --- SVG-based themes for mode-line
;;; Version: 0.1.3
;;; Package-Requires: ((xmlgen "0.4"))
;;; Author: sabof
;;; URL: https://github.com/sabof/svg-mode-line-themes

;;; Commentary:

;; The project is hosted at https://github.com/sabof/svg-mode-line-themes
;; The latest version, and all the relevant information can be found there.

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'nadvice nil t)
(require 'svg-mode-line-themes-core)
(require 'svg-mode-line-themes-widgets)
(require 'svg-mode-line-themes-diesel)
(require 'svg-mode-line-themes-bio-diesel)
(require 'svg-mode-line-themes-nasa)
(require 'svg-mode-line-themes-black-crystal)

(when (and (default-value 'mode-line-format)
           (not (assoc 'default smt/themes)))
  (setq smt/themes
        (acons 'default (default-value 'mode-line-format)
               smt/themes)))

(setq smt/current-theme 'diesel)

;;;###autoload
(defun* smt/next-theme ()
  (interactive)
  (assert (> (length smt/themes) 1))
  (let* (( position (position smt/current-theme smt/themes :key 'car))
         ( next-theme
           (or (car (nth (1+ position) smt/themes))
               (car (nth 0 smt/themes)))))
    (setq smt/current-theme next-theme)
    (when (eq 'archetype smt/current-theme)
      (smt/next-theme)
      (return-from smt/next-theme))
    (force-mode-line-update)
    (message "Current mode-line theme: %s" next-theme)))

;;;###autoload
(defun smt/set-theme (theme)
  (interactive
   (list (intern (completing-read
                  "Set mode-line theme to: "
                  (mapcar 'symbol-name (remove 'archetype (mapcar 'car smt/themes))) nil t))))
  (setq smt/current-theme theme)
  (force-mode-line-update))

;;;###autoload
(defun* smt/enable (&optional use-header-line)
  (unless (image-type-available-p 'svg)
    (display-warning 'svg-mode-line-themes "SVG support is not available")
    (return-from smt/enable))
  (set-default (if use-header-line
                   'header-line-format
                 'mode-line-format)
               '(:eval (smt/modeline-format)))
  (if (and (boundp 'pre-redisplay-function)
           (fboundp 'add-function))
      (add-function :before
                    pre-redisplay-function
                    'smt/register-user-location)
    (add-hook 'post-command-hook 'smt/register-user-location))
  (force-mode-line-update))

(provide 'svg-mode-line-themes)
;;; svg-mode-line-themes.el ends here
