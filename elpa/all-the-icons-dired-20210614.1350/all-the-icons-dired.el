;;; all-the-icons-dired.el --- Shows icons for each file in dired mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2020  jtbm37
;; Copyright (C) 2021 Jimmy Yuen Ho Wong

;; Author: jtbm37
;; Maintainer: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Version: 2.0
;; Package-Version: 20210614.1350
;; Package-Commit: a758766878b6e8b9eaaf41d68599a2df99e37f48
;; Keywords: files icons dired
;; Package-Requires: ((emacs "24.4") (all-the-icons "2.2.0"))
;; URL: https://github.com/wyuenho/all-the-icons-dired

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
;; To use this package, simply add this to your init.el:
;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; To manually install, add this to your init.el before the hook mentioned above.
;; (add-to-load-path (expand-file-name "~/path/to/all-the-icons-dired"))
;; (load "all-the-icons-dired.el")


;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'all-the-icons)
(require 'subr-x)

(defface all-the-icons-dired-dir-face
  '((((background dark)) :foreground "white")
    (((background light)) :foreground "black"))
  "Face for the directory icon"
  :group 'all-the-icons-faces)

(defcustom all-the-icons-dired-v-adjust 0.01
  "The default vertical adjustment of the icon in the dired buffer."
  :group 'all-the-icons
  :type 'number)

(defcustom all-the-icons-dired-monochrome t
  "Whether to show the icons as the same color as the text on the same line."
  :group 'all-the-icons
  :type 'boolean)

(defvar all-the-icons-dired-mode)

(defun all-the-icons-dired--add-overlay (pos string)
  "Add overlay to display STRING at POS."
  (let ((ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'all-the-icons-dired-overlay t)
    (overlay-put ov 'after-string string)))

(defun all-the-icons-dired--overlays-in (beg end)
  "Get all all-the-icons-dired overlays between BEG to END."
  (cl-remove-if-not
   (lambda (ov)
     (overlay-get ov 'all-the-icons-dired-overlay))
   (overlays-in beg end)))

(defun all-the-icons-dired--overlays-at (pos)
  "Get all-the-icons-dired overlays at POS."
  (apply #'all-the-icons-dired--overlays-in `(,pos ,pos)))

(defun all-the-icons-dired--remove-all-overlays ()
  "Remove all `all-the-icons-dired' overlays."
  (save-restriction
    (widen)
    (mapc #'delete-overlay
          (all-the-icons-dired--overlays-in (point-min) (point-max)))))

(defun all-the-icons-dired--refresh ()
  "Display the icons of files in a dired buffer."
  (all-the-icons-dired--remove-all-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (dired-move-to-filename nil)
        (let ((case-fold-search t))
          (when-let* ((file (dired-get-filename 'relative 'noerror))
                      (icon (if (file-directory-p file)
                                (all-the-icons-icon-for-dir file
                                                            :face 'all-the-icons-dired-dir-face
                                                            :v-adjust all-the-icons-dired-v-adjust)
                              (apply 'all-the-icons-icon-for-file file
                                     (append
                                      `(:v-adjust ,all-the-icons-dired-v-adjust)
                                      (when all-the-icons-dired-monochrome
                                        `(:face ,(face-at-point))))))))
            (if (member file '("." ".."))
                (all-the-icons-dired--add-overlay (point) "  \t")
              (all-the-icons-dired--add-overlay (point) (concat icon "\t"))))))
      (forward-line 1))))

(defun all-the-icons-dired--refresh-advice (fn &rest args)
  "Advice function for FN with ARGS."
  (apply fn args)
  (when all-the-icons-dired-mode
    (all-the-icons-dired--refresh)))

(defvar all-the-icons-dired-advice-alist
  '((dired-aux     dired-create-directory       all-the-icons-dired--refresh-advice)
    (dired-aux     dired-do-create-files        all-the-icons-dired--refresh-advice)
    (dired-aux     dired-do-kill-lines          all-the-icons-dired--refresh-advice)
    (dired-aux     dired-do-rename              all-the-icons-dired--refresh-advice)
    (dired-aux     dired-insert-subdir          all-the-icons-dired--refresh-advice)
    (dired         wdired-abort-changes         all-the-icons-dired--refresh-advice)
    (dired         dired-internal-do-deletions  all-the-icons-dired--refresh-advice)
    (dired-narrow  dired-narrow--internal       all-the-icons-dired--refresh-advice)
    (dired         dired-readin                 all-the-icons-dired--refresh-advice)
    (dired         dired-revert                 all-the-icons-dired--refresh-advice)
    (find-dired    find-dired-sentinel          all-the-icons-dired--refresh-advice))
  "A list of file, adviced function, and advice function.")

(defun all-the-icons-dired--setup ()
  "Setup `all-the-icons-dired'."
  (setq-local tab-width 1)
  (pcase-dolist (`(,file ,sym ,fn) all-the-icons-dired-advice-alist)
    (with-eval-after-load file
      (advice-add sym :around fn)))
  (all-the-icons-dired--refresh))

(defun all-the-icons-dired--teardown ()
  "Functions used as advice when redisplaying buffer."
  (kill-local-variable 'tab-width)
  (pcase-dolist (`(,file ,sym ,fn) all-the-icons-dired-advice-alist)
    (with-eval-after-load file
      (advice-remove sym fn)))
  (all-the-icons-dired--remove-all-overlays))

;;;###autoload
(define-minor-mode all-the-icons-dired-mode
  "Display all-the-icons icon for each file in a dired buffer."
  :lighter " all-the-icons-dired-mode"
  (when (derived-mode-p 'dired-mode)
    (if all-the-icons-dired-mode
        (all-the-icons-dired--setup)
      (all-the-icons-dired--teardown))))

(provide 'all-the-icons-dired)
;;; all-the-icons-dired.el ends here
