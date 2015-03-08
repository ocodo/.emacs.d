;;; remember-theme.el --- Remembers the last theme in use and re-loads for the next session.

;; Author: Jason Milkins <jasonm23@gmail.com>
;; Url: https://github.com/jasonm23/emacs-remember-theme
;; Version: 20150308.1931

;;; Commentary:

;; I keep my `.emacs` in source control, and use the same defaults on all
;; the machines I use. However, I like to have different themes on
;; different machines.
;;
;; To help me do this automatically, I've created this little feature that
;; remembers the current theme when Emacs closes, and loads it again when
;; you start up (clearing any other loaded themes first.)
;;
;; If you have things you'd like to run after the theme has loaded,
;; use the hook provided, 'remember-theme-after-load-hook (see
;; add-hook if you haven't used hooks before.)

;;; Installation:

;; Install from the melpa via elpa/pacakge.el.
;; Then in .emacs / .emacs.d/init.el add:
;;
;; (remember-theme-load)
;; (add-hook 'kill-emacs-hook 'remember-theme-save)
;;
;; Install with: `M-x package-install remember-theme`

;;; Changelog:
;; 20150308.1931
;; * Stop forcing load at after-init, update instructions.
;; 20140122.1500
;; * Add custom hook to be run after loading the remembered theme
;; 20131231.0025
;; * Custom Variable to control location of remember-theme (defaults to ~/.emacs-theme)
;; 20131215.0441
;; * Conditional execution (no ~/.emacs-theme, no disable current theme(s) / no theme load attempt)
;; 20130807.1251
;; * Conditional execution (no theme, no save)
;; 20130718.230
;; * Updated documentation/header
;; 20130716.1310
;;  * Fixed typos
;; 20130716.311
;;  * Unload all loaded themes before loading the remembered theme.
;; 0.1.2
;;  * Fix bug for non existent .emacs-theme
;; 0.1.1
;;  * Fix autoloads
;; 0.1.0
;;  * Init
;;

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3 of the License.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.
;;
;; This file is not a part of Emacs
;;; Package-Requires: ((emacs "24.1"))

;;; Code:
(require 'cl)

(defvar remember-theme-emacs-dot-file "~/.emacs-theme"
  "location to store remembered theme.")

(defvar remember-theme-after-load-hook nil
  "Hook called after loading the remembered theme.")

;;;###autoload
(defun remember-theme-read ()
"Return first line from `remember-theme-emacs-dot-file'."
  (with-temp-buffer
    (insert-file-contents remember-theme-emacs-dot-file)
    (car (split-string (buffer-string) "\n" t))))

;;;###autoload
(defun remember-theme-save ()
  "Creates (or replaces) ~/.emacs-theme.
Stores the name of the current Emacs theme,
for retrieval by remember-theme-load"
  (when (> (length custom-enabled-themes) 0)
    (when (file-exists-p remember-theme-emacs-dot-file)
      (delete-file remember-theme-emacs-dot-file))
    (append-to-file (format "%s\n" (symbol-name (car custom-enabled-themes))) "" remember-theme-emacs-dot-file)))

;;;###autoload
(defun remember-theme-load ()
  "Load the theme used last.
This is stored in the file `remember-theme-emacs-dot-file'.
The last line of `remember-theme-emacs-dot-file' is read as the theme name.

`remember-theme-emacs-dot-file' is created by remember-theme-save
manually creating or editing this file is not recommended.

Also if the theme is no longer available on this site, an error will be thrown.

Any currently loaded themes will be disabled and the theme named in
`remember-theme-emacs-dot-file' will be loaded.

If no `remember-theme-emacs-dot-file' file exists the operation is skipped."
    (when (file-exists-p remember-theme-emacs-dot-file)
      (loop for theme
            in custom-enabled-themes
            do (disable-theme theme))
      (let* ((theme-name (remember-theme-read))
             (theme-symbol (intern theme-name)))
        (unless (member theme-symbol custom-enabled-themes)
          (require (intern (format "%s-theme" theme-name))))       
        (load-theme theme-symbol))
    (run-hooks 'remember-theme-after-load-hook)))

(provide 'remember-theme)

;;; remember-theme.el ends here
