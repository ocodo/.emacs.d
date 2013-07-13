;;; helm-git-grep.el --- helm for git grep

;; Copyright (C) 2013 mechairoi

;; Author: mechairoi
;; Maintainer: Yasuyuki Oka <yasuyk@gmail.com>
;; Version: 0.1
;; URL: https://github.com/yasuyk/helm-git-grep
;; Package-Requires: ((helm "1.0"))
;; Keywords: helm, git

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

;; Add the following to your emacs init file:
;;
;; (require 'helm-git-grep) ;; Not necessary if using ELPA package
;; (global-set-key (kbd "C-c g") 'helm-git-grep)

;; Original version is anything-git-grep, and port to helm.
;; https://github.com/mechairoi/anything-git-grep

;;; Code:

(eval-when-compile (require 'cl))
(require 'vc-git)
(require 'helm)
(require 'helm-fix-multiline-process)

(defun helm-git-grep-find-git-root ()
  (vc-git-root (or (buffer-file-name) default-directory)))

(defun helm-git-grep-find-git-submodule-root ()
  (vc-git-root (or (buffer-file-name) default-directory)))

(defun helm-git-grep-process ()
  (helm-aif (helm-attr 'default-directory)
      (let ((default-directory it))
        (apply 'start-process "git-grep-process" nil
               "git" "--no-pager" "grep" "--full-name" "-n" "--no-color"
               (nbutlast
                (apply 'append
                       (mapcar
                        (lambda (x) (list "-e" x "--and"))
                        (split-string helm-pattern " +" t))))))
    '()))

(defun helm-git-submodule-grep-process ()
  (helm-aif (helm-attr 'default-directory)
      (let ((default-directory it))
        (apply 'start-process
               "git-submodule-grep-process" nil
               (list
                "git" "--no-pager" "submodule" "--quiet" "foreach"
                (format "git grep --full-name -n --no-color %s | sed s!^!$path/!"
                        (mapconcat (lambda (x)
                                     (format "-e %s " (shell-quote-argument x)))
                                   (split-string helm-pattern " +" t)
                                   "--and ")))))
    '()))

(defvar helm-source-git-grep
  '((name . "Git Grep")
    (init . (lambda () (helm-attrset
                        'default-directory
                        (helm-git-grep-find-git-root))))
    (default-directory . nil)
    (candidates-process . helm-git-grep-process)
    (type . file-line)
    (candidate-number-limit . 300)
    (requires-pattern . 3)
    (volatile)
    (delayed)))

(defvar helm-source-git-submodule-grep
  '((name . "Git Submodule Grep")
    (init . (lambda () (helm-attrset
                        'default-directory
                        (helm-git-grep-find-git-submodule-root))))
    (candidates-process . helm-git-submodule-grep-process)
    (default-directory . nil)
    (type . file-line)
    (candidate-number-limit . 300)
    (requires-pattern . 3)
    (volatile)
    (delayed)))

;;;###autoload
(defun helm-git-grep ()
  "Helm git grep"
  (interactive)
  (helm-other-buffer '(helm-source-git-grep
                           helm-source-git-submodule-grep)
   "*helm git grep"))

;;;###autoload
(defun helm-git-grep-from-here ()
  "Helm git grep with current symbol using `helm'."
  (interactive)
  (helm :sources '(helm-source-git-grep
                       helm-source-git-submodule-grep)
            :input (thing-at-point 'symbol)))

(provide 'helm-git-grep)

;;; helm-git-grep.el ends here
