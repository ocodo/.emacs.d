;;; rake.el --- Run rake commands

;; Copyright (C) 2014 Adam Sokolnicki

;; Author:            Adam Sokolnicki <adam.sokolnicki@gmail.com>
;; URL:               https://github.com/asok/rake.el
;; Version: 20141229.245
;; X-Original-Version:           0.2.0
;; Keywords:          rake, ruby
;; Package-Requires:  ((f "0.13.0") (dash "1.5.0") (cl-lib "0.5"))

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
;;
;; Package to interact with rake command - make for Ruby.
;; It uses completion to choose a rake task to run.
;; It can use one of the ruby preloaders and caching to speed up the execution of rake.
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)

(defmacro rake--with-root (root body-form)
  `(let* ((default-directory root))
     ,body-form))

(defmacro rake--choose-command-prefix (root &rest cases)
  `(cond ((rake--spring-p root)
          ,(plist-get cases :spring))
         ((rake--zeus-p root)
          ,(plist-get cases :zeus))
         ((rake--bundler-p root)
          ,(plist-get cases :bundler))
         (t
          ,(plist-get cases :vanilla))))

(defcustom rake-enable-caching t
  "When t enables tasks caching."
  :group 'rake
  :type 'boolean)

(defcustom rake-cache-file
  (expand-file-name "rake.cache" user-emacs-directory)
  "The name of rake's cache file."
  :group 'rake
  :type 'string)

(defcustom rake-completion-system 'ido
  "The completion system to be used by rake."
  :group 'rake
  :type 'symbol
  :options '(ido grizzl helm default))

(defconst rake--edit-command 4)
(defconst rake--omit-cache   16)

(defun rake--spring-p (root)
  (file-exists-p (f-canonical
                  (concat
                   temporary-file-directory
                   "spring/"
                   (md5 root 0 -1)
                   ".pid"))))

(defun rake--zeus-p (root)
  (file-exists-p (expand-file-name ".zeus.sock" root)))

(defun rake--bundler-p (root)
  (file-exists-p (expand-file-name "Gemfile" root)))

(defun rake--vertical-ido-on-p ()
  (and
   (boundp 'ido-vertical-decorations)
   (eq ido-decorations ido-vertical-decorations)))

(defun rake--vertical-completion-system-p ()
  (cl-case rake-completion-system
    ('grizzl t)
    ('helm t)
    ('ido (rake--vertical-ido-on-p))
    (t nil)))

(defun rake--root ()
  (file-truename (locate-dominating-file default-directory "Rakefile")))

(defun rake--unserialize-cache ()
  "Read data serialized by `rake--serialize-cache' from `rake-cache-file'."
  (when (file-exists-p rake-cache-file)
    (with-temp-buffer
      (insert-file-contents rake-cache-file)
      (read (buffer-string)))))

(defvar rake--cache
  (or (rake--unserialize-cache)
      (make-hash-table :test 'equal)))

(defun rake--serialize-cache ()
  "Serialize `rake--cache' to `rake-cache-file'.
The saved data can be restored with `rake--unserialize-cache'."
  (when (file-writable-p rake-cache-file)
    (with-temp-file rake-cache-file
      (insert (let (print-length) (prin1-to-string rake--cache))))))

(defun rake--tasks-output (root)
  (shell-command-to-string
   (rake--choose-command-prefix root
                                :zeus "zeus rake -T -A"
                                :spring "spring rake -T -A"
                                :bundler "bundle exec rake -T -A"
                                :vanilla "rake -T -A")))

(defun rake--parse-tasks (output)
  "Parses the OUTPUT of rake command with list of tasks. Returns a list of tasks."
  (--keep it
          (--map (if (string-match "rake \\(.+\\)$" it)
                     (match-string 1 it))
                 (split-string output "[\n]"))))

(defun rake--fresh-tasks (root)
  "Returns list of the rake tasks for the current project."
  (rake--parse-tasks (rake--tasks-output root)))

(defun rake--cached-tasks (arg root)
  "Returns cached list of the tasks for project in ROOT.
If ARG is 16 then regenerate the cache first.
If ARG is not 16 and the tasks are not found for the project it will regenerate the cache."
  (when (= arg rake--omit-cache)
    (rake--regenerate-cache root))
  (or (gethash root rake--cache) (rake--regenerate-cache root)))

(defun rake--regenerate-cache (root)
  "Regenerates cache for the tasks for the project in ROOT dir and saves it
to `rake-cache-file'. Returns a list of the tasks for the project."
  (let ((tasks (rake--fresh-tasks root)))
    (puthash root tasks rake--cache)
    (rake--serialize-cache)
    tasks))

(defun rake--cached-or-fresh-tasks (arg root)
  "Returns a list of all the rake tasks defined in the current project.
If `rake-enable-caching' is t look in the cache, if not fallback to calling rake."
  (if rake-enable-caching
      (rake--cached-tasks arg root)
    (rake--fresh-tasks root)))

(defun rake--tasks-without-doscstrings (tasks)
  (--map (rake--trim-docstring it) tasks))

(defun rake--trim-docstring (task)
  (replace-regexp-in-string "[ ]*#.*$" "" task))

(defun rake--completing-read (prompt choices)
  (cl-case rake-completion-system
    ('ido     (ido-completing-read prompt choices))
    ('default (completing-read     prompt choices))
    ('helm (if (fboundp 'helm-comp-read)
               (helm-comp-read prompt choices
                               :candidates-in-buffer t
                               :must-match 'confirm)
             (user-error "Please install helm first")))
    ('grizzl (if (and (fboundp 'grizzl-completing-read) (fboundp 'grizzl-make-index))
                 (grizzl-completing-read prompt (grizzl-make-index choices))
               (user-error "Please install grizzl first")))
    (t (funcall rake-completion-system prompt choices))))

(define-derived-mode rake-compilation-mode compilation-mode "Rake Compilation"
  "Compilation mode used by `rake' command.")

;;;###autoload
(defun rake-regenerate-cache ()
  "Regenerates the rake's cache for the current project."
  (interactive)
  (rake--regenerate-cache (rake--root)))

;;;###autoload
(defun rake (arg &optional compilation-mode)
  "Runs rake command."
  (interactive "P")
  (let* ((root (or (rake--root) (user-error "Rakefile not found")))
         (arg (or (car arg) 0))
         (prefix (rake--choose-command-prefix root
                                              :spring  "spring rake "
                                              :zeus    "zeus rake "
                                              :bundler "bundle exec rake "
                                              :vanilla "rake "))
         (prompt "Rake: ")
         (tasks (rake--cached-or-fresh-tasks arg root))
         (task (rake--trim-docstring
                (rake--completing-read prompt
                                       (if (rake--vertical-completion-system-p)
                                           tasks
                                         (rake--tasks-without-doscstrings tasks)))))
         (command (if (= arg rake--edit-command)
                      (read-string prompt (concat prefix task " "))
                    (concat prefix task))))
    (rake--with-root
     root
     (compile command (or compilation-mode 'rake-compilation-mode)))))

(provide 'rake)

;;; rake.el ends here
