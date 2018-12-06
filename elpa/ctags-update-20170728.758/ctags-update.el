;;; ctags-update.el --- (auto) update TAGS in parent directory using exuberant-ctags

;; Created: 2011-10-16 13:17
;; Version: 1.0
;; Package-Version: 20170728.758
;; Author: Joseph(纪秀峰)  jixiuf@gmail.com
;; Keywords: exuberant-ctags etags
;; URL: https://github.com/jixiuf/ctags-update

;; Copyright (C) 2011,2017 Joseph(纪秀峰) all rights reserved.

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

;; And the following to your ~/.emacs startup file.

;; (ctags-global-auto-update-mode)
;; (setq ctags-update-prompt-create-tags nil);you need manually create TAGS in your project

;; or only turn it on for some special mode
;;
;;(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)
;;(add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
;; ...
;;(add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode)
;;

;; when you save a file ,`ctags-auto-update-mode' will update TAGS using `exuberant-ctags'.

;; custom the interval  of updating TAGS  by  `ctags-update-delay-seconds'.

;; if you want to update (create) TAGS manually
;; you can
;;     (autoload 'ctags-update "ctags-update" "update TAGS using ctags" t)
;;     (global-set-key "\C-cE" 'ctags-update)
;; with prefix `C-u' ,then you can generate a new TAGS file in your selected directory,
;; with prefix `C-uC-u' same to prefix `C-u',but save the command to kill-ring instead of execute it."

;;
;; on windows ,you can custom `ctags-update-command' like this:
;; (when (equal system-type 'windows-nt)
;;   (setq ctags-update-command (expand-file-name  "~/.emacs.d/bin/ctags.exe")))


;;; Code:

(require 'etags)

(defgroup ctags-update nil
  "auto update TAGS in parent directory using `exuberant-ctags'"
  :prefix "ctags-update"
  :group 'etags)

(defcustom ctags-update-command "ctags"
  "it only support `exuberant-ctags'
take care it is not the ctags in `emacs-VERSION/bin/'
you should download `exuberant-ctags' and make sure
the ctags is under $PATH before `emacs-VERSION/bin/'"
  :type 'string
  :group 'ctags-update)

(defcustom ctags-update-delay-seconds  (* 5 60) ; 5 mins
  "in `after-save-hook' current-time - last-time must bigger than this value,
then `ctags-update' will be called"
  :type 'integer
  :group 'ctags-update)

(defcustom ctags-update-other-options
  (list
   "--fields=+iaSt"
   "--extra=+q"
   ;; "−−c++−kinds=+p"
   "--exclude='*.elc'"
   "--exclude='*.class'"
   "--exclude='.git'"
   "--exclude='.svn'"
   "--exclude='SCCS'"
   "--exclude='RCS'"
   "--exclude='CVS'"
   "--exclude='EIFGEN'")
  "other options for ctags"
  :group 'ctags-update
  :type '(repeat string))

(defcustom ctags-update-lighter " ctagsU"
  "Lighter displayed in mode line when `ctags-auto-update-mode'
is enabled."
  :group 'ctags-update
  :type 'string)

(defcustom ctags-update-prompt-create-tags t
  "Promtp create `TAGS' when tag file not exists."
  :group 'ctags-update
  :type 'string)

(defvar ctags-update-last-update-time
  (- (float-time (current-time)) ctags-update-delay-seconds 1)
  "make sure when user first call `ctags-update' it can run immediately")

(defvar ctags-auto-update-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(defvar  ctags-auto-update-mode-hook nil)

(defvar ctags-update-use-xemacs-etags-p
  (fboundp 'get-tag-table-buffer)
  "Use XEmacs etags?")

(defun ctags-update-file-truename (filename &optional counter prev-dirs)
  "empty function")

(if ctags-update-use-xemacs-etags-p
    (unless (fboundp 'symlink-expand-file-name)
      (fset 'symlink-expand-file-name 'file-truename)))
(if (fboundp 'symlink-expand-file-name)
    (fset 'ctags-update-file-truename 'symlink-expand-file-name)
  (fset 'ctags-update-file-truename 'file-truename))


(defsubst ctags-update-native-w32-p()
  (and (equal system-type 'windows-nt)
       (not (string-match-p "MINGW" (or (getenv "MSYSTEM") "")))))

(defun ctags-update-command-args (tagfile-full-path &optional save-tagfile-to-as)
  "`tagfile-full-path' is the full path of TAGS file . when files in or under the same directory
with `tagfile-full-path' changed ,then TAGS file need to be updated. this function will generate
the command to update TAGS"
  (append
   (list "-R" "-e" )
   (list "-f" (ctags-update-get-system-path (or save-tagfile-to-as tagfile-full-path)))
   ctags-update-other-options
   (if (ctags-update-native-w32-p)
       ;; on windows "ctags -R d:/.emacs.d"  works , but "ctags -R d:/.emacs.d/" doesn't
       ;; On Windows, "gtags d:/tmp" work, but "gtags d:/tmp/" doesn't
       (list (directory-file-name
              (file-name-directory (or save-tagfile-to-as tagfile-full-path ))))
     (list "."))))

(defun ctags-update-get-command(command command-args)
  "get the full command as string."
  (concat command " "(mapconcat 'identity  command-args " ")))

(defun ctags-update-get-system-path(file-path)
  "when on windows `expand-file-name' will translate from \\ to /
some times it is not needed . then this function is used to translate /
to \\ when on windows"
  (if (ctags-update-native-w32-p)
      (convert-standard-filename  file-path)
    file-path))

(defun ctags-update-find-tags-file ()
  "recursively searches each parent directory for a file named 'TAGS' and returns the
path to that file or nil if a tags file is not found. Returns nil if the buffer is
not visiting a file"
  (let ((tag-root-dir (locate-dominating-file default-directory "TAGS")))
    (if tag-root-dir (expand-file-name "TAGS" tag-root-dir) nil)))

(defsubst ctags-update-check-interval()
  (> (- (float-time (current-time))
        ctags-update-last-update-time)
     ctags-update-delay-seconds))

(defsubst ctags-update-triggered-by-tags(tags)
  "`ctags-update' should not be called when TAGS file call `after-save-hook'.
this return t if current buffer file name is TAGS."
  (and
   (buffer-file-name)
   (or
    (string-equal (ctags-update-file-truename tags)
                  (ctags-update-file-truename (buffer-file-name)))
    (string-equal (ctags-update-file-truename (concat tags ".tmp"))
                  (ctags-update-file-truename (buffer-file-name))))))

(defun ctags-update-process-sentinel(proc _change)
  (let (tags tmp-tags)
    (setq tags (process-name proc))
    (setq tmp-tags (concat tags ".tmp"))
    (when (zerop (process-exit-status proc))
      (kill-buffer (process-buffer proc) )
      (rename-file tmp-tags tags t)
      (message "%s is updated." tags))
    (when (file-exists-p tmp-tags)
      (delete-file tmp-tags ))))

(defun ctags-update-how-to-update(is-interactive)
  "return a tagfile"
  (let (tags)
    (cond
     ((> (prefix-numeric-value current-prefix-arg) 1)  ;C-u or C-uC-u ,generate new tags in selected directory
      (setq tags (expand-file-name "TAGS" (read-directory-name "Generate TAGS in dir:"))))
     (is-interactive
      (setq tags (ctags-update-find-tags-file))
      (unless tags
        (setq tags (expand-file-name "TAGS" (read-directory-name "Generate TAGS in dir:")))))
     (t
      (setq tags (ctags-update-find-tags-file))
      (unless tags
        (setq ctags-update-last-update-time
              (- (float-time (current-time)) ctags-update-delay-seconds 1))
        (when ctags-update-prompt-create-tags
          (setq tags
                (expand-file-name
                 "TAGS" (read-directory-name
                         "Generate TAGS in dir(or disable `ctags-auto-update-mode'):")))
          ))))
    tags))

;;;###autoload
(defun ctags-update(&optional args)
  "ctags-update in parent directory using `exuberant-ctags'.
1. you can call this function directly,
2. enable `ctags-auto-update-mode',
3. with prefix `C-u' then you can generate a new TAGS file in selected directory,
4. with prefix `C-uC-u' save the command to kill-ring instead of execute it."
  (interactive "P")
  (let (tags proc)
    (setq tags (ctags-update-how-to-update (called-interactively-p 'interactive)))
    (when tags
      (when (get-process tags)          ;process name == tags
        (user-error "Another ctags-update process is already running"))

      (when (or (called-interactively-p 'interactive)
                (and (ctags-update-check-interval) ;updating interval reach
                     (not (ctags-update-triggered-by-tags tags))))
        (setq ctags-update-last-update-time (float-time (current-time)));;update time
        (let ((orig-default-directory default-directory)
              (default-directory (file-name-directory tags)))
          (when (ctags-update-native-w32-p)
            (setq default-directory orig-default-directory))
          (cond
           ;;with prefix `C-uC-u' save the command to kill-ring
           ;; sometime the directory you select need root privilege
           ;; so save the command to kill-ring,
           ((= (prefix-numeric-value current-prefix-arg) 16)
            (kill-new (format "cd %s && %s" (ctags-update-get-system-path default-directory)
                              (ctags-update-get-command
                               ctags-update-command (ctags-update-command-args tags))))
            (message "save ctags-upate command to king-ring. (C-y) yank it back."))
           (t (setq proc (apply 'start-process ;;
                                tags " *ctags-update*"
                                ctags-update-command
                                (ctags-update-command-args tags (concat tags ".tmp"))))
              (set-process-query-on-exit-flag proc nil)
              (set-process-sentinel proc 'ctags-update-process-sentinel))))))))

;;;###autoload
(define-minor-mode ctags-auto-update-mode
  "auto update TAGS using `exuberant-ctags' in parent directory."
  :lighter ctags-update-lighter
  :keymap ctags-auto-update-mode-map
  ;; :global t
  :init-value nil
  :group 'ctags-update
  (if ctags-auto-update-mode
      (add-hook 'after-save-hook 'ctags-update nil t)
    (remove-hook 'after-save-hook 'ctags-update t)))

;;;###autoload
(defun turn-on-ctags-auto-update-mode()
  "turn on `ctags-auto-update-mode'."
  (interactive)
  (ctags-auto-update-mode 1))

;;;###autoload
(define-global-minor-mode ctags-global-auto-update-mode
  ctags-auto-update-mode
  turn-on-ctags-auto-update-mode)

(provide 'ctags-update)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; tab-width: 4
;; End:

;;; ctags-update.el ends here
