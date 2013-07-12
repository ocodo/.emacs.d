;;; find-things-fast.el --- Find things fast, leveraging the power of git

;; Copyright (C) 2010 Elliot Glaysher
;; Copyright (C) 2006, 2007, 2008 Phil Hagelberg and Doug Alcorn

;; Author: Elliot Glaysher and Phil Hagelberg and Doug Alcorn
;; URL: https://github.com/mamciek/find-things-fast 
;; Version: 1.0
;; Created: 2010-02-19
;; Keywords: project, convenience
;; EmacsWiki: FindThingsFast

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides methods that quickly get you to your destination inside
;; your current project, leveraging the power of git if you are using it to
;; store your code.

;; A project is defined as a directory which meets one of these conditions in
;; the following order:
;;
;; - The presence of a `.dir-locals.el' file (emacs23 style) or a
;;   `.emacs-project' file (`project-local-variables' style).
;; - The git repository that the current buffer is in.
;; - The project root defined in `project-root.el', a library not included with
;;   GNU Emacs.
;; - The current default-directory if none of the above is found.

;; When we're in a git repository, we use git-ls-files and git-grep to speed up
;; our searching. Otherwise, we fallback on find statements. As a special
;; optimization, we prune ".svn" directories whenever we find.

;; ftf provides three main user functions:
;;
;; - `ftf-find-file' which does a filename search. It has been tested on large
;;   projects; it takes slightly less than a second from command invocation to
;;   prompt on the Chromium source tree 9,183 files as of the time of
;;   this writing).
;;
;;   It uses git-ls-files for speed when available (such as the Chromium tree),
;;   and falls back on a find statement at the project root/default-directory
;;   when git is unavailable.
;;
;; - `ftf-grepsource' which greps all the files for the escaped pattern passed
;;   in at the prompt.
;;
;;   It uses git-grep for speed when available (such as the Chromium tree),
;;   and falls back on a find|xargs grep statement when not.
;;
;; - The `with-ftf-project-root' macro, which locally changes
;;   `default-directory' to what find-things-fast thinks is the project
;;   root. Two trivial example functions, `ftf-gdb' and `ftf-compile' are
;;   provided which show it off.

;; By default, it looks only for files whose names match `ftf-filetypes'. The
;; defaults were chosen specifically for C++/Chromium development, but people
;; will probably want to override this so we provide a helper function you can
;; use in your mode hook, `ftf-add-filetypes'. Example:
;;
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda (ftf-add-filetypes '("*.el" "*.elisp"))))

;; If `ido-mode' is enabled, the menu will use `ido-completing-read'
;; instead of `completing-read'.

;; This file was based on the `find-file-in-project.el' by Phil Hagelberg and
;; Doug Alcorn which can be found on the EmacsWiki. Obviously, the scope has
;; greatly expanded.

;; Recommended binding:
;; (global-set-key (kbd [f1]) 'ftf-find-file)
;; (global-set-key (kbd [f2]) 'ftf-grepsource)
;; (global-set-key (kbd [f4]) 'ftf-gdb)
;; (global-set-key (kbd [f5]) 'ftf-compile)

;;; Code:

(defvar ftf-filetypes
  '("*.*")
  "A list of filetype patterns that grepsource will use.")

(defun ftf-add-filetypes (types)
  "Makes `ftf-filetypes' local to this buffer and adds the
elements of list types to the list"
  (make-local-variable 'ftf-filetypes)
  (dolist (type types)
    (add-to-list 'ftf-filetypes type)))

(defun ftf-find-locals-directory ()
  "Returns the directory that contains either `.dir-locals.el' or
  `.emacs-project' or nil if no path components contain such a directory."
  (defun parent-dir (path)
    (file-name-directory (directory-file-name path)))
  (defun dir-has-project-file (path)
    (or (file-exists-p (concat path "/.dir-locals.el"))
        (file-exists-p (concat path "/.emacs-project"))))
  (let ((path default-directory)
        (return-path nil))
    (while path
      (cond ((string-equal path "/")
             (setq return-path nil
                   path nil))
            ((dir-has-project-file path)
             (setq return-path path
                   path nil))
            (t (set 'path (parent-dir path)))))
    return-path))

(defun ftf-project-directory ()
  "Returns what we should use as `default-directory'."
  (or (ftf-find-locals-directory)
      (ftf-get-top-git-dir default-directory)
      ;; `project-details' is defined in the `project-root.el' package. This
      ;; will be nil if it doesn't exist.
      (if (boundp 'project-details) (cdr project-details) nil)
      default-directory))

(defun ftf-get-find-command ()
  "Creates the raw, shared find command from `ftf-filetypes'."
  (concat "find . -path '*/.svn' -prune -o -name \""
          (mapconcat 'identity ftf-filetypes "\" -or -name \"")
          "\""))

;; Adapted from git.el 's git-get-top-dir
(defun ftf-get-top-git-dir (dir)
  "Retrieve the top-level directory of a git tree. Returns nil on error or if
not a git repository.."
  ;; temp buffer for errors in toplevel git rev-parse
  (with-temp-buffer
    (if (eq 0 (call-process "git" nil t nil "rev-parse"))
        (let ((cdup (with-output-to-string
                      (with-current-buffer standard-output
                        (cd dir)
                        (call-process "git" nil t nil
                                      "rev-parse" "--show-cdup")))))
          (expand-file-name (concat (file-name-as-directory dir)
                                    (car (split-string cdup "\n")))))
      nil)))

(defun ftf-interactive-default-read (string)
  "Gets interactive arguments for a function. This reuses your
current major mode's find-tag-default-function if possible,
otherwise defaulting to `find-tag-default'."
  (list
   (let* ((default (funcall (or (get major-mode 'find-tag-default-function)
                                'find-tag-default)))
          (spec (read-from-minibuffer
                 (if default
                     (format "%s (default %s): "
                             (substring string 0
                                        (string-match "[ :]+\\'" string))
                             default)
                   string)
                 nil nil nil default nil)))
    (if (equal spec "")
        (or default (error "There is no default symbol to grep for."))
      spec))))

;;;###autoload
(defun ftf-grepsource (cmd-args)
  "Greps the current project, leveraging local repository data
for speed and falling back on a big \"find | xargs grep\"
command if we aren't.

The project's scope is defined first as a directory containing
either a `.dir-locals.el' file or an `.emacs-project' file OR the
root of the current git repository OR a project root defined by
the optional `project-root.el' package OR the default directory
if none of the above is found."
  (interactive (ftf-interactive-default-read "Grep project for string: "))
  ;; When we're in a git repository, use git grep so we don't have to
  ;; find-files.
  (let ((quoted (replace-regexp-in-string "\"" "\\\\\"" cmd-args))
        (git-toplevel (ftf-get-top-git-dir default-directory))
        (default-directory (ftf-project-directory))
        (grep-use-null-device nil))
    (cond (git-toplevel ;; We can accelerate our grep using the git data.
           (grep (concat "git --no-pager grep --no-color -n -e \""
                         quoted
                         "\" -- \""
                         (mapconcat 'identity ftf-filetypes "\" \"")
                         "\"")))
          (t            ;; Fallback on find|xargs
             (grep (concat (ftf-get-find-command)
                           " | xargs grep -nH -e \"" quoted "\""))))))

(defun ftf-project-files-string ()
  "Returns a string with the raw output of ."
  (let ((git-toplevel (ftf-get-top-git-dir default-directory)))
    (cond (git-toplevel
           (shell-command-to-string
            (concat "git ls-files -- \""
                    (mapconcat 'identity ftf-filetypes "\" \"")
                    "\"")))
           (t
            (let ((default-directory (ftf-project-directory)))
              (shell-command-to-string (ftf-get-find-command)))))))

(defun ftf-project-files-hash ()
  "Returns a hashtable filled with file names as the key and "
  (let ((default-directory (ftf-project-directory))
        (table (make-hash-table :test 'equal)))
    (mapcar (lambda (file)
              (let* ((file-name (file-name-nondirectory file))
                     (full-path (expand-file-name file))
                     (pathlist (cons full-path (gethash file-name table nil))))
                (puthash file-name pathlist table)))
            (split-string (ftf-project-files-string)))
    table))

(defun ftf-project-files-alist ()
  "Return an alist of all filenames in the project and their path.

Files with duplicate filenames are suffixed with the name of the
directory they are found in so that they are unique."
  (let ((table (ftf-project-files-hash))
        file-alist)
    (maphash (lambda (file-name full-path)
               (cond ((> (length full-path) 1)
                      (dolist (path full-path)
                        (let ((entry (cons file-name path)))
                          (ftf-uniqueify entry)
                          (set 'file-alist (cons entry file-alist)))))
                     (t
                      (set 'file-alist
                           (cons (cons file-name (car full-path))
                                 file-alist)))))
             table)
    file-alist))

(defun ftf-uniqueify (file-cons)
  "Set the car of the argument to include the directory name plus
the file name."
  (setcar file-cons
	  (concat (car file-cons) ": "
		  (cadr (reverse (split-string (cdr file-cons) "/"))))))

;;;###autoload
(defun ftf-find-file ()
  "Prompt with a completing list of all files in the project to find one.

The project's scope is defined first as a directory containing
either a `.dir-locals.el' file or an `.emacs-project' file OR the
root of the current git repository OR a project root defined by
the optional `project-root.el' package OR the default directory
if none of the above is found."
  (interactive)
  (let* ((project-files (ftf-project-files-alist))
	 (filename (if (functionp 'ido-completing-read)
                   (ido-completing-read "Find file in project: "
                                        (mapcar 'car project-files))
                 (completing-read "Find file in project: "
                                  (mapcar 'car project-files))))
     (file (cdr (assoc filename project-files))))
    (if file
        (find-file file)
      (error "No such file."))))

(defmacro with-ftf-project-root (&rest body)
  "Run BODY with `default-directory' set to what the
find-things-fast project root. A utility macro for any of your
custom functions which might want to run "
  `(let ((default-directory (ftf-project-directory)))
          ,@body))

;;;###autoload
(defun ftf-compile ()
  "Run the `compile' function from the project root."
  (interactive)
  (with-ftf-project-root (call-interactively 'compile)))

;;;###autoload
(defun ftf-gdb ()
  "Run the `gdb' function from the project root."
  (interactive)
  (with-ftf-project-root (call-interactively 'gdb)))

(provide 'find-things-fast)
;;; find-things-fast.el ends here
