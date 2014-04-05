;;; anything-git-grep.el --- anything for git grep

;; Copyright (C) 2013 mechairoi

;; Author: mechairoi
;; Version: 0.1
;; URL: https://github.com/mechairoi/anything-git-grep
;; Package-Requires: ((anything "1.3.9"))
;; Keywords: anything, git

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

;;; Code:

(eval-when-compile (require 'cl))
(require 'vc-git)
(require 'anything-config)
(require 'anything-fix-multiline-process)
(provide 'anything-git-grep)

(defun anything-git-grep-find-git-root ()
  (vc-git-root (or (buffer-file-name) default-directory)))

(defun anything-git-grep-find-git-submodule-root ()
  (vc-git-root (or (buffer-file-name) default-directory)))

(defun anything-git-grep-process ()
  (anything-aif (anything-attr 'default-directory)
      (let ((default-directory it))
        (apply 'start-process "git-grep-process" nil
               "git" "--no-pager" "grep" "--full-name" "-n" "--no-color"
               (nbutlast
                (apply 'append
                       (mapcar
                        (lambda (x) (list "-e" x "--and"))
                        (split-string anything-pattern " +" t))))))
    '()))

(defun anything-git-submodule-grep-process ()
  (anything-aif (anything-attr 'default-directory)
      (let ((default-directory it))
        (apply 'start-process
               "git-submodule-grep-process" nil
               (list
                "git" "--no-pager" "submodule" "--quiet" "foreach"
                (format "git grep --full-name -n --no-color %s | sed s!^!$path/!"
                        (mapconcat (lambda (x)
                                     (format "-e %s " (shell-quote-argument x)))
                                   (split-string anything-pattern " +" t)
                                   "--and ")))))
    '()))

(defvar anything-c-source-git-grep
  '((name . "Git Grep")
    (init . (lambda () (anything-attrset
                        'default-directory
                        (anything-git-grep-find-git-root))))
    (default-directory . nil)
    (candidates . anything-git-grep-process)
    (type . file-line)
    (candidate-number-limit . 300)
    (requires-pattern . 3)
    (volatile)
    (delayed)))

(defvar anything-c-source-git-submodule-grep
  '((name . "Git Submodule Grep")
    (init . (lambda () (anything-attrset
                        'default-directory
                        (anything-git-grep-find-git-submodule-root))))
    (candidates . anything-git-submodule-grep-process)
    (default-directory . nil)
    (type . file-line)
    (candidate-number-limit . 300)
    (requires-pattern . 3)
    (volatile)
    (delayed)))

(defun anything-git-grep ()
  "Anything git grep"
  (interactive)
  (anything-other-buffer '(anything-c-source-git-grep
                           anything-c-source-git-submodule-grep)
   "*anything git grep"))

(defun anything-git-grep-from-here ()
  "Anything git grep with current symbol using `anything'."
  (interactive)
  (anything :sources '(anything-c-source-git-grep
                       anything-c-source-git-submodule-grep)
            :input (thing-at-point 'symbol)))

;;; anything-git-grep.el ends here
