;;; helm-projectile-all.el --- Find files in all projectile projects using helm

;; Copyright (C) 2014 Dror Levin

;; Author: Dror Levin
;; Version: 20140429.952
;; X-Original-Version: 1.0
;; Package-Requires: ((helm "1.6.0") (projectile "0.10.0") (dash "1.5.0") (s "1.9.0"))

;; This file is not part of GNU Emacs.

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

;; Find files in all projects known to projectile.

;;; Code:

(require 'projectile)
(require 'helm-locate)
(require 'helm-files)
(require 'helm-help)
(require 'dash)
(require 's)

(defvar helm-source-projectile-all-files-list
  `((name . "Projectile Files")
    (init . (lambda ()
              (with-current-buffer (helm-candidate-buffer 'global)
                (-mapcat
                 (lambda (dir)
                   (let* ((truedir (file-truename dir))
                          (filelist (flet ((projectile-project-root () truedir))
                                      (projectile-dir-files truedir))))
                       (dolist (file filelist)
                         (insert (concat truedir file "\n")))))
                 projectile-known-projects))))
    (candidates-in-buffer)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-find-file-help-message)
    (mode-line . helm-ff-mode-line-string)
    (type . file)
    (action . (("Find file" . (lambda (file) (find-file file)))
               ("Open dired in file's directory" . helm-open-dired))))
  "Helm source definition.")

;;;###autoload
(defun helm-projectile-all ()
  "Use projectile with Helm for finding files in all projects."
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm :sources helm-source-projectile-all-files-list
          :buffer "*helm projectile all*")))

(provide 'helm-projectile-all)
;;; helm-projectile-all.el ends here
