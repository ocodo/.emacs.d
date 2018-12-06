;;; chef-mode.el --- minor mode for editing an opscode chef repository

;; Copyright (C) 2011 Maciej Pasternacki

;; Author: Maciej Pasternacki <maciej@pasternacki.net>
;; Created: 28 Aug 2011
;; Version: 0.1
;; Package-Version: 20180628.1453
;; Keywords: chef knife

;; This file is NOT part of GNU Emacs.

;;; License:

;; Copyright (c) 2011, Maciej Pasternacki <maciej@pasternacki.net>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * Neither the name of the copyright holder nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL MACIEJ PASTERNACKI BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; This library defines a minor mode to work with Opscode Chef
;; (http://www.opscode.com/chef/) repository. It creates two
;; keybindings:

;; - C-c C-c (M-x chef-knife-dwim) - when editing part of chef
;;   repository (cookbook, data bag item, node/role/environment
;;   definition), uploads that part to the Chef Server by calling
;;   appropriate knife command
;; - C-c C-k (M-x knife) - runs a user-specified knife command

;; The library detects bundler and, if Gemfile is present on top-level
;; of the Chef repository, runs 'bundle exec knife' instead of plain
;; 'knife'.

;; If chef-use-rvm is non-nil, it talks with rvm.el
;; (https://github.com/senny/rvm.el) to use proper Ruby and gemset.

;;; Code:


(defvar chef-knife-command "knife"
  "Knife command to run")

(defvar chef-use-bundler t
  "Use `bundle exec knife' if Gemfile exists")

(defvar chef-use-rvm t
  "If non-nil, require rvm.el and call rvm-activate-corresponding-ruby on chef repo root before calling knife")

(defvar chef-mode-map (make-sparse-keymap)
  "Key map for chef-mode")

(define-key chef-mode-map (kbd "\C-c \C-k") 'knife)
(define-key chef-mode-map (kbd "\C-c \C-c") 'chef-knife-dwim)

(define-minor-mode chef-mode
  "Mode for interacting with Opscode Chef"
  nil chef-mode-map)

(defun turn-on-chef-mode ()
  "Enable chef-mode."
  (chef-mode 1))

(define-globalized-minor-mode global-chef-mode
  chef-mode turn-on-chef-mode)

(defun find-chef-root (&optional path)
  (when (null path)
    (setq path (or buffer-file-name
                   default-directory)))
  (cond
   ((not (file-directory-p path))
    (find-chef-root (concat (file-name-as-directory path) "..")))
   ((equal (expand-file-name path) (expand-file-name "~")) nil)
   ((equal (expand-file-name path) "/") nil)
   ((let ((ff (directory-files path)))
      (or (member ".chef" ff)
          (and (member "cookbooks" ff)
               (member "roles" ff)
               (member "config" ff))))
    (file-name-as-directory (expand-file-name path)))
   (t (find-chef-root (concat (file-name-as-directory path) "..")))))

(defun chef/fallback (trigger)
  (let* ((chef-mode nil)
         (command (key-binding trigger)))
    (when (commandp command)
      (call-interactively command))))

(defmacro chef/with-root-or-fallback (trigger &rest body)
  `(let ((chef-root (find-chef-root)))
     (if (null chef-root)
         (chef/fallback ,trigger)
       ,@body)))

(defun chef-run-knife (command &rest args)
  (when chef-use-rvm
    (rvm-activate-corresponding-ruby))

  (let ((knife-buffer (get-buffer-create "*knife*"))
        (use-bundler (and chef-use-bundler (file-exists-p "Gemfile"))))
    (with-current-buffer knife-buffer
      (setq default-directory chef-root)
      (setq list-buffers-directory default-directory)
      (special-mode)
      (toggle-read-only 0)
      (erase-buffer)
      (insert (concat "# " default-directory "\n"
                      (when use-bundler
                        "bundle exec ")
                      chef-knife-command " " command " "
                      (mapconcat 'identity args " ")
                      "\n\n")))
    (if use-bundler
        (apply 'call-process
               "bundle" nil knife-buffer
               "bundle" "exec" chef-knife-command (cons command args))
      (apply 'call-process
             chef-knife-command nil knife-buffer
             chef-knife-command (cons command args)))
    (with-current-buffer knife-buffer
      (toggle-read-only 1))
    (switch-to-buffer-other-window knife-buffer t)
    (fit-window-to-buffer)))

(defun knife (command)
  "Run knife"
  (interactive "Command: knife ")
  (chef/with-root-or-fallback
   (kbd "\C-c \C-k")
   (apply 'chef-run-knife (split-string-and-unquote command))))

(defun chef-knife-dwim ()
  "Upload currently edited thing to the Chef server.

Guesses whether you have "
  (interactive)
  (chef/with-root-or-fallback
   (kbd "\C-c \C-c")
   (let ((b (current-buffer)))
     (save-some-buffers nil (lambda ()
                              (eq b (current-buffer)))))
   (if buffer-file-name
       (let ((default-directory chef-root)
             (rpath (file-relative-name buffer-file-name chef-root)))
         (cond
          ((string-match "^\\(?:site-\\)?cookbooks/\\([^/]+\\)/" rpath)
           (print (match-string 1 rpath))
           (chef-run-knife "cookbook" "upload" (match-string 1 rpath)))
          ((string-match "^\\(role\\|node\\|environment\\)s/\\(.*\\)" rpath)
           (chef-run-knife (match-string 1 rpath) "from" "file" (match-string 2 rpath)))
          ((string-match "^data.bags/\\([^/]+\\)/\\(.*\\.yaml\\)" rpath)
           (chef-run-knife "data" "bag" "from" "yaml" (match-string 1 rpath) (match-string 2 rpath)))
          ((string-match "^data.bags/\\([^/]+\\)/\\(.*\\)" rpath)
           (chef-run-knife "data" "bag" "from" "file" (match-string 1 rpath) (match-string 2 rpath)))
          (t (chef/fallback (kbd "\C-c \C-c")))))
     (chef/fallback (kbd "\C-c \C-c")))))



(defun chef-resource-lookup ()
  "Open the documentation in a browser for the chef resource at point"
  (interactive)
  (let* ((base "http://wiki.opscode.com/display/chef/Resources")
        (anchor "#Resources-")
        (tbl '(
               ("cookbook_file" . "CookbookFile")
               ("cron" . "Cron")
               ("deploy" . "Deploy")
               ("directory" . "Directory")
               ("env" . "Env")
               ("erl_call" . "ErlangCall")
               ("execute" . "Execute")
               ("file" . "File")
               ("git" . "Git")
               ("group" . "Group")
               ("http_request" . "HTTPRequest")
               ("ifconfig" . "Ifconfig")
               ("link" . "Link")
               ("log" . "Log")
               ("mdadm" . "Mdadm")
               ("mount" . "Mount")
               ("ohai" . "Ohai")
               ("package" . "Package")
               ("apt_package" . "Package")
               ("dpkg_package" . "Package")
               ("easy_install_package" . "Package")
               ("freebsd_package" . "Package")
               ("macports_package" . "Package")
               ("portage_package" . "Package")
               ("rpm_package" . "Package")
               ("gem_package" . "Package")
               ("yum_package" . "Package")
               ("zypper_package" . "Package")
               ("powershell" . "PowerShellScript")
               ("remote_directory" . "RemoteDirectory")
               ("remote_file" . "RemoteFile")
               ("route" . "Route")
               ("ruby_block" . "RubyBlock")
               ("scm" . "SCM")
               ("script" . "Script")
               ("bash" . "Script")
               ("csh" . "Script")
               ("perl" . "Script")
               ("python" . "Script")
               ("ruby" . "Script")
               ("service" . "Service")
               ("subversion" . "Subversion")
               ("template" . "Template")
               ("user" . "User")
               ))
        (target (assoc-string (symbol-at-point) tbl)))

  (if target
      (browse-url (concat base anchor (cdr target)))
    (browse-url base))))


(provide 'chef-mode)
;;; chef-mode.el ends here
