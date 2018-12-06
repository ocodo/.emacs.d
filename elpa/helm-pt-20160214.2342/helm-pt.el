;;; helm-pt.el --- platinum searcher helm interface

;; Copyright (C) 2015 by Rich Alesi

;; Author: Rich Alesi
;; URL: https://github.com/ralesi/helm-pt
;; Version: 20150307.141210
;; Package-Requires: ((helm "1.5.6") (cl-lib "0.5") (emacs "24.4"))

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

;; a helm interface for the platinum searcher, allowing both ADHOC
;; running, and integrating into projectile.

;; the platinum searcher can be found at
;; <https://github.com/monochromegane/the_platinum_searcher>

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-utils)
(require 'helm-files)
(require 'subr-x)

(defgroup helm-pt nil
  "the platinum searcher interface for helm."
  :group 'helm)

(defcustom helm-pt-command "pt"
  "Default executable for platinum searcher."
  :type 'string
  :group 'helm-pt)

(defcustom helm-pt-args '("--smart-case")
  "User arguments."
  :type 'list
  :group 'helm-pt)

(defcustom helm-pt-ignore-arguments '("archive-contents")
  "List of default patterns to ignore."
  :type 'list
  :group 'helm-pt)

(defcustom helm-pt-insert-at-point t
  "Insert thing at point as search pattern.
You can set value same as `thing-at-point'."
  :type 'boolean
  :group 'helm-pt)

(defvar helm-pt-default-directory nil)

(defvar helm-pt-split-line-regexp "^\\([[:lower:][:upper:]]?:?.*?\\):\\([0-9]+\\):\\(.*\\)")

(defvar helm-source-pt nil
  "Source to scan directory tree with the platinum searcher.")

(setq helm-source-pt
      (helm-build-async-source
          "Platinum Searcher"
        ;; :mode-line (helm-pt-unique-path (or helm-pt-default-directory default-directory))
        :header-name (lambda (name)
                       (concat name))
        :candidates-process 'helm-pt--process
        :filter-one-by-one 'helm-pt-filter-one-by-one
        :nohighlight t
        ;; :map 'helm-do-pt-map
        :candidate-number-limit 9999
        :action (helm-make-actions
                 "Find File" 'helm-pt-action
                 )
        :persistent-action 'helm-pt--persistent-action
        :persistent-help "Jump to line"
        :requires-pattern 3
        ))

(defun helm-pt--command (pattern)
  (let ((ignore-args
         (mapcar (lambda (val) (concat "--ignore=" val))
                 helm-pt-ignore-arguments))
        (default-directory (or helm-pt-default-directory
                               (helm-default-directory))))
    (mapconcat 'identity
               (append (list  helm-pt-command)
                       helm-pt-args
                       ignore-args
                       '("-e" "--nogroup" "--nocolor" "--")
                       (list (shell-quote-argument (string-join (split-string pattern) ".*")))
                       (list (shell-quote-argument default-directory))) " ")))

(defvar helm-do-pt-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-l") 'helm-pt--do-pt-up-one-level)
    map)
  "Keymap for `helm-do-pt'.")

(defun helm-pt--process ()
  "Launch async process to supply candidates."
  (let ((debug-on-error t)
        (cmd-line (helm-pt--command helm-pattern)))
    ;; Start pt process.
    (prog1            ; This function should return the process first.
        (start-file-process-shell-command
         "*helm pt*" helm-buffer cmd-line)
      ;; Init sentinel.
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       #'(lambda (process event)
           ;; need to FIX this and actually recognize exit-code
           (let ((noresult (= (process-exit-status process) 2)))
             (helm-process-deferred-sentinel-hook
              process event
              (or helm-ff-default-directory
                  (helm-default-directory)
                  default-directory)
              )
             ;; error handling
             (cond ((and noresult
                         (with-current-buffer helm-buffer
                           (insert (concat "* Exit with code 2, no result found,"
                                           " command line was:\n\n "
                                           (format "%s" 
                                                   (helm-pt--command helm-pattern)))))))
                   ((string= event "finished\n")
                    (with-helm-window
                      (force-mode-line-update)))
                   ;; Catch error output in log.
                   (t (helm-log
                       "Error: %s %s"
                       (replace-regexp-in-string "\n" "" event))))))))))

(defun helm-pt-unique-path (path)
  (let* ((listl (split-string path "/" t))
         (lastl (car (last listl)))
         (butl (butlast listl)))
    (concat
     (string-join (cl-loop for paths in
                           butl
                           collect
                           (downcase (cl-subseq paths 0 1))
                           ) "/")
     "/"
     lastl)))

(defun helm-pt--persistent-action (candidate)
  (helm-pt-action candidate)
  (helm-highlight-current-line))

(defun helm-pt-split-line (line)
  "Split a pt output line."
  (when (string-match helm-pt-split-line-regexp line)
    ;; Don't use split-string because buffer/file name or string
    ;; may contain a ":".
    (cl-loop for n from 1 to 3 collect (match-string n line))))

(defun helm-pt-action (candidate)
  (let* ((split   (helm-pt-split-line  candidate))
         (fname   (car split))
         (lineno  (string-to-number (nth 1 split)))
         (str     (nth 2 split)))
    (find-file fname)
    (helm-goto-line lineno)
    ))

(defun helm-pt--candidate-transformer (candidate)
  (let* ((root   (lambda () (or helm-ff-default-directory
                                (helm-default-directory)
                                default-directory)))
         (split (helm-pt-split-line candidate))
         (fname  (if (and root split)
                     (expand-file-name (car-safe split) root)
                   (car-safe split)))
         (proj-fname (if (and fname (projectile-project-p))
                         (replace-regexp-in-string
                          (regexp-quote (projectile-project-root))
                          ""
                          fname)
                       fname))
         (lineno (nth 1 split))
         (str    (nth 2 split)))
    (when (and proj-fname lineno str)
      (concat (propertize proj-fname 'face 'helm-grep-file)
              ":"
              (propertize lineno 'face 'helm-grep-lineno)
              ":"
              str
              )
      )))

(defun helm-pt-filter-one-by-one (candidate)
  "`filter-one-by-one' transformer function for `helm-do-pt'."
  (let* ((root (or helm-pt-default-directory
                   helm-ff-default-directory
                   (helm-default-directory)
                   default-directory
                   ))
         (split (helm-pt-split-line candidate))
         (fname  (if (and root split)
                     (expand-file-name (car-safe split) root)
                   (car-safe split)))
         (working-fname (if fname
                            (replace-regexp-in-string
                             (regexp-quote root)
                             ""
                             fname)
                          fname))
         (lineno (nth 1 split))
         (str    (nth 2 split)))
    (when (and working-fname lineno str)
      (cons (concat (propertize working-fname 'face 'helm-grep-file)
                    ":"
                    (propertize lineno 'face 'helm-grep-lineno)
                    ":"
                    (helm-pt--highlight-candidate (replace-regexp-in-string "^[ \t]+" "\t" str))
                    )
            candidate))))

(defun helm-pt--highlight-candidate (candidate)
  (let ((limit (1- (length candidate)))
        (last-pos 0))
    (while (and (< last-pos limit)
                (string-match helm-pattern candidate last-pos))
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'helm-match
                         candidate)
      (setq last-pos (1+ (match-end 0)))))
  candidate)

;;;###autoload
(defun helm-do-pt (&optional basedir)
  "Helm source for platinum searcher."
  (interactive)
  (unless (executable-find "pt")
    (error "pt not available"))
  (setq helm-pt-default-directory basedir)
  (helm
   :sources '(helm-source-pt)
   :fuzzy-match t
   :buffer "*helm pt*"
   :input (when helm-pt-insert-at-point
            (if (region-active-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
              (thing-at-point 'symbol)))
   :truncate-lines t))

;;;###autoload
(defun helm-projectile-pt ()
  "Helm version of projectile-pt."
  (interactive)
  (require 'projectile)
  (unless (fboundp 'projectile-project-root)
    (error "not in a project"))
  (let ((helm-pt-ignore-arguments
         (append helm-pt-ignore-arguments
                 projectile-globally-ignored-files
                 projectile-globally-ignored-directories
                 (projectile-project-ignored))))
    (helm-do-pt (projectile-project-root))))

(provide 'helm-pt)

;;; helm-pt.el ends here
