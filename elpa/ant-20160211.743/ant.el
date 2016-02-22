;;; ant.el --- helpers for compiling with ant

;; Copyright (C) 2010 Andrew Gwozdziewycz <git@apgwoz.com>

;; Version: 0.1
;; Package-Version: 20160211.743
;; Keywords: compilation, ant, java

;; This file is NOT part of GNU Emacs

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defvar ant-last-task "compile")
(defvar ant-build-file-name "build.xml")
(defvar ant-command "ant -emacs")

(defvar *ant-tasks-cache* '())

(defvar *ant-tasks-command* "grep -e '<target.*name=\"[^\-][^\"]*.*$'")
(defvar ant-tasks-default '("compile" "test" "clean"))

(defun ant-find-tasks (directory)
  (let ((output (shell-command-to-string (concat *ant-tasks-command* " "
                                                 directory "/"
                                                 ant-build-file-name))))
    (message output)
    (if (> (length output) 0)
        (mapcar (lambda (x) (replace-regexp-in-string ".*<target.*name=\"\\([^\-][^\"]*\\).*" "\\1" x)) 
                (split-string output "[\n]"))
      nil)))

;; should cache tasks from the build file at some point
(defun ant-tasks (directory)
  (let ((tasks (assoc-string directory *ant-tasks-cache*)))
    (cdr 
     (or tasks
         (progn 
           (let ((newtasks (or (ant-find-tasks directory) ant-tasks-default)))
             (setq *ant-tasks-cache*
                   (cons (cons directory newtasks) *ant-tasks-cache*))
             newtasks))))))

(defun ant-get-task (directory)
  (let ((task (completing-read-multiple (concat "Task (default): ") 
                                        (ant-tasks directory))))
    (if (> (length task) 0)
        (mapconcat 'identity task " ")
      "")))

(defun ant-find-root (indicator)
  (let ((cwd default-directory))
    (while (and (not (file-exists-p (concat cwd indicator)))
                (not (string-equal (expand-file-name cwd) "/")))
      (setq cwd (concat cwd "../")))
    (if (file-exists-p (concat cwd indicator))
        (expand-file-name cwd)
      nil)))

;;;###autoload
(defun ant-kill-cache ()
  (interactive)
  (setq *ant-tasks-cache* '()))

;;;###autoload
(defun ant (&optional task)
  "Run ant `task` in project root directory."
  (interactive)
  (let ((default-directory (ant-find-root ant-build-file-name)))
    (if default-directory
        (let ((task (or task (ant-get-task default-directory))))
          (setq ant-last-task task)
          (compile (concat ant-command " " task)))
      (message "Couldn't find an ant project."))))

;;;###autoload
(defun ant-last ()
  "Run the last ant task in project"
  (interactive)
  (ant (or ant-last-task "")))

;;;###autoload
(defun ant-compile ()
  (interactive)
  (ant "compile"))

;;;###autoload
(defun ant-clean ()
  (interactive)
  (ant "clean"))

;;;###autoload
(defun ant-test ()
  (interactive)
  (ant "test"))

(provide 'ant)

;;; ant.el ends here
