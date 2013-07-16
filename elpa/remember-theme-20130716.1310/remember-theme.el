;;; remember-theme.el --- Remembers the last theme in use and re-loads for the next session.
;;;
;;; Author: Jason Milkins <jasonm23@gmail.com>
;;; Url: https://github.com/jasonm23/emacs-remember-theme
;;; Version: 20130716.1310
;;;
;;; Changelog :
;;;
;;; 20130716.1310 L Fixed typo
;;;
;;; 20130716.311 : Unload all loaded themes before loading the
;;; remembered theme.
;;;
;;;        0.1.2 : Fix bug for non existent .emacs-theme
;;;
;;;        0.1.1 : Fix autoloads
;;;
;;;        0.1.0 : Init
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, version 3 of the License.
;;;
;;; This file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs.
;;;
;;; This file is not a part of Emacs
;;;
;;; Commentary:
;;;
;;; Emacs Remember Theme
;;;  
;;; I keep my `.emacs` in source control, and use the same defaults on all
;;; the machines I use. However, I like to have different themes on
;;; different machines.
;;; 
;;; To help me do this automatically, I've created this little feature that
;;; remembers the current theme when Emacs closes, and loads it again when
;;; you start up (clearing any other loaded themes first.)
;;; 
;;; If you install via elpa (marmalade repo) everything is set up for you
;;; automatically.
;;; 
;;; Just install with: `M-x package-install remember-theme`
;;;


;;;###autoload
(defun remember-theme-save ()
  "Creates (or replaces) ~/.emacs-theme, and stores the name of
the current Emacs theme, for retrieval by remember-theme-load"
  (when (file-exists-p "~/.emacs-theme")
    (delete-file "~/.emacs-theme"))
  (append-to-file (format "%s\n" (symbol-name (car custom-enabled-themes)))
                  nil
                  "~/.emacs-theme"))

;;;###autoload
(defun remember-theme-load ()
  "Load the theme used last - This is stored in the file
~/.emacs-theme. The last line of .emacs-theme is read as the
theme name.

~/.emacs-theme is created by remember-theme-save manually
creating or editing this file is not supported"
  (loop for theme
        in custom-enabled-themes
        do (disable-theme theme))
  (when (file-exists-p "~/.emacs-theme")
      (load-theme (intern (car (nreverse (with-temp-buffer
                                           (insert-file-contents "~/.emacs-theme")
                                           (split-string
                                            (buffer-string)))))))))

;;;###autoload
(when load-file-name
  (add-hook 'after-init-hook 'remember-theme-load)
  (add-hook 'kill-emacs-hook 'remember-theme-save))
  
(provide 'remember-theme)

;;; remember-theme.el ends here
