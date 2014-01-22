;;; remember-theme.el --- Remembers the last theme in use and re-loads for the next session.

;; Author: Jason Milkins <jasonm23@gmail.com>
;; Url: https://github.com/jasonm23/emacs-remember-theme
;; Version: 20140122.1500

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
;; add-hook if you haven't used hooks before.

;;; Installation:

;; Install from the marmalade repo via elpa/pacakge.el, and everything
;; is set up for you automatically.
;;
;; Install with: `M-x package-install remember-theme`

;;; Changelog:
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

;;; Code:
(defgroup remember-theme nil
  "Options controlling remember-theme")

(defcustom remember-theme-file "~/.emacs-theme"
  "Name/Location of the file which stores the current theme (file
is updated on Emacs exit)"
  :type '(file)
  :group 'remember-theme)

(defvar remember-theme-after-load-hook nil
  "Hook called after loading the remembered theme")

;;;###autoload
(defun remember-theme-save ()
  "Creates (or replaces) remember-theme-file (default ~/.emacs-theme), and stores the name of
  the current Emacs theme, for retrieval by remember-theme-load"
  (when (> (length custom-enabled-themes) 0)
    (when (file-exists-p remember-theme-file)
      (delete-file remember-theme-file))
    (append-to-file (format "%s\n" (symbol-name (car custom-enabled-themes))) "" remember-theme-file)))

;;;###autoload
(defun remember-theme-load ()
  "Load the theme used last - This is stored in the
  remember-theme-file. The last line of .emacs-theme is read as the
  theme

  remember-theme-file (default ~/.emacs-theme) is created by
  remember-theme-save. Don't manually create or edit this file.

  Currently enabled themes will be disabled and the theme in
  remember-theme-file will be loaded.

  If no remember-theme-file exists the operation is skipped, and
  any currently loaded theme(s) will be left enabled."
  (when (file-exists-p remember-theme-file)
    (loop for theme
          in custom-enabled-themes
          do (disable-theme theme))
    (load-theme (intern (car (nreverse (with-temp-buffer
                                         (insert-file-contents remember-theme-file)
                                         (split-string
                                          (buffer-string)))))))
    (run-hooks 'remember-theme-after-load-hook)))

;;;###autoload
(when load-file-name
  (add-hook 'after-init-hook 'remember-theme-load)
  (add-hook 'kill-emacs-hook 'remember-theme-save))

(provide 'remember-theme)

;;; remember-theme.el ends here
