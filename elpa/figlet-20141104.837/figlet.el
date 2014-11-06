;;; figlet.el --- Annoy people with big, ascii art text

;; Copyright (C) 2014 Aurelien Aptel <aurelien.aptel@gmail.com>
;; Copyright (C) 2008 Philip Jackson

;; Author: Philip Jackson <phil@shellarchive.co.uk>
;; Version: 20141104.837
;; X-Original-Version: 0.5

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To use this feature simple eval (require 'figlet) type M-x figlet
;; and you will be asked for a string. If you use a prefix (C-u M-x
;; figlet) then you will be asked for a font.

;; Have a look at `figlet-comment', `figlet-figletify-region' and
;; `figlet-figletify-region-comment'.

;; Warning, leaving large ascii art text in your teams codebase might
;; cause an outbreak of physical violence.

(defvar figlet-fonts '())
(defvar figlet-default-font "small"
  "Default font to use when none is supplied.")
(defvar figlet-options '()
  "List of options for the figlet call.")
(defvar figlet-font-directory nil
  "Figlet default font directory")

(defun figlet-get-font-dir ()
  "Return default font directory."
  (or figlet-font-directory
      (setq figlet-font-directory
            (let ((s (shell-command-to-string "figlet -I2")))
              (substring s 0 (1- (length s)))))))

(defun figlet-get-font-list ()
  "Get a list of figlet fonts."
  (or figlet-fonts
      (setq figlet-fonts
            (mapcar (lambda (f)
                      (replace-regexp-in-string "\\.flf$" "" f))
                    (directory-files (figlet-get-font-dir) nil "^[^.].+\\.flf$")))))

;;;###autoload
(defun figlet (string)
  "Pass a string through figlet and insert the output at
point. Use a prefix arg to be promted for a font."
  (interactive "sTo be fug: ")
  (let* ((fonts (figlet-get-font-list))
         (font (if current-prefix-arg
                   (if fonts
                       (completing-read "Font: " fonts nil t)
                       (read-from-minibuffer "Font: " figlet-default-font))
                   figlet-default-font)))
    (insert
     (with-temp-buffer
       (call-process "figlet" nil t t
                     (mapconcat 'identity figlet-options " ")
                     "-f" font string)
       (goto-char (point-min))
       (re-search-forward "^." nil t)
       (delete-region (point-min) (point-at-bol))
       (re-search-forward "^[[:blank:]]*$" nil t)
       (delete-region (point) (point-max))
       (delete-trailing-whitespace)
       (buffer-substring (point-min) (point-max))))))

;;;###autoload
(defun figlet-comment (string)
  "Insert a figlet string just as `figlet' would but comment the
result (using `comment-region')"
  (interactive "sTo be fug: ")
  (let ((start (point)))
    (save-excursion
      (figlet string)
      (comment-region start (point)))))

;;;###autoload
(defun figlet-figletify-region (start end)
  "Convert the region into a figlet string."
  (interactive "r")
  (let ((str (buffer-substring start end)))
    (delete-region start end)
    (figlet str)))

;;;###autoload
(defun figlet-figletify-region-comment (start end)
  "Convert the region into a figlet string as with
`figlet-figletify-region' but comment it out too."
  (interactive "r")
  (let ((str (buffer-substring start end)))
    (delete-region start end)
    (figlet-comment str)))

;;;###autoload
(defun figlet-preview-fonts (&optional text)
  "View an example of each font in a new buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Figlet Font Examples*"))
  (delete-region (point-min) (point-max))
  (mapconcat (lambda (x)
               (let ((figlet-default-font x))
                 (insert (concat x ":\n"))
                 (figlet (or text x))))
             (figlet-get-font-list)
             "\n"))

(provide 'figlet)

;;; figlet.el ends here
