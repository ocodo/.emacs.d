;;; helm-git-grep.el --- helm for git grep, an incremental git-grep(1)

;; Copyright (C) 2013 mechairoi
;;               2013, 2014  Yasuyuki Oka <yasuyk@gmail.com>

;; This is a fork of anything-git-grep.el wrote by mechairoi.

;; Author: mechairoi
;; Maintainer: Yasuyuki Oka <yasuyk@gmail.com>
;; Version: 20140222.1822
;; X-Original-Version: 0.6.2
;; URL: https://github.com/yasuyk/helm-git-grep
;; Package-Requires: ((helm "1.5.9"))
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

;; Add the following to your Emacs init file:
;;
;; (require 'helm-git-grep) ;; Not necessary if installed by package.el
;; (global-set-key (kbd "C-c g") 'helm-git-grep)
;; ;; Invoke `helm-git-grep' from isearch.
;; (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
;; ;; Invoke `helm-git-grep' from other helm.
;; (eval-after-load 'helm
;;   '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))

;; For more information, See the following URL:
;; https://github.com/yasuyk/helm-git-grep

;; Original version is anything-git-grep, and port to helm.
;; https://github.com/mechairoi/anything-git-grep

;;; Code:

(eval-when-compile (require 'cl))
(require 'helm)
(require 'helm-files)
(require 'helm-elscreen) ;; helm-elscreen-find-file

(declare-function elscreen-get-conf-list "ext:elscreen.el" (type))
(declare-function wgrep-setup-internal "ext:wgrep")

(defgroup helm-git-grep nil
  "Helm for git grep."
  :prefix "helm-git-grep-"
  :group 'helm)

(defcustom helm-git-grep-candidate-number-limit 300
  "Limit candidate number of `helm-git-grep'.

Set it to nil if you don't want this limit."
  :group 'helm-git-grep
  :type '(choice (const :tag "Disabled" nil) integer))

(defcustom helm-git-grep-max-length-history 100
  "Max number of elements to save in `helm-git-grep-history'."
  :group 'helm-git-grep
  :type 'integer)

(defcustom helm-git-grep-use-ioccur-style-keys t
  "Use Arrow keys to jump to occurrences."
  :group 'helm-git-grep
  :type  'boolean)

(defcustom helm-git-grep-ignore-case t
  "Ignore case when matching."
  :group 'helm-git-grep
  :type  'boolean)

(defcustom helm-git-grep-showing-leading-and-trailing-lines nil
  "Show leading and trailing lines."
  :group 'helm-git-grep
  :type  'boolean)

(defcustom helm-git-grep-showing-leading-and-trailing-lines-number 1
  "Number of showing leading and trailing lines option."
  :group 'helm-git-grep
  :type  'integer)

(defvar helm-git-grep-history nil "The history list for `helm-git-grep'.")

(defvar helm-git-grep-exclude-file-p nil)
(defvar helm-git-grep-exclude-file-history nil)

(defun helm-git-grep-git-string (&rest args)
  "Execute Git with ARGS, returning the first line of its output.
If there is no output return nil.  If the output begins with a
newline return an empty string."
  (with-temp-buffer
    (apply 'process-file "git" nil (list t nil) nil
           (append '("--no-pager") args))
    (unless (= (point-min) (point-max))
      (goto-char (point-min))
      (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position)))))

(defun helm-git-grep-get-top-dir ()
  "Get the git root directory."
  (let ((cwd (expand-file-name (file-truename default-directory))))
    (when (file-directory-p cwd)
      (let* ((default-directory (file-name-as-directory cwd))
             (cdup (helm-git-grep-git-string "rev-parse" "--show-cdup")))
        (when cdup
          (file-name-as-directory (expand-file-name cdup cwd)))))))

(defun helm-git-grep-showing-leading-and-trailing-lines-option (&optional strp)
  "Get <num> option."
  (if helm-git-grep-showing-leading-and-trailing-lines
      (format "-%d" helm-git-grep-showing-leading-and-trailing-lines-number)
    (when strp "")))

(defun helm-git-grep-args (exclude-file-pattern)
  "Create arguments of `helm-git-grep-process' in `helm-git-grep'."
  (delq nil
        (append
         (list "--no-pager" "grep" "--full-name" "-n" "--no-color"
               (if helm-git-grep-ignore-case "-i" nil)
               (helm-git-grep-showing-leading-and-trailing-lines-option))
         (nbutlast
          (apply 'append
                 (mapcar
                  (lambda (x) (list "-e" x "--and"))
                  (split-string helm-pattern " +" t))))
         (when exclude-file-pattern exclude-file-pattern))))

(defun helm-git-submodule-grep-command ()
  "Create command of `helm-git-submodule-grep-process' in `helm-git-grep'."
  (list "git" "--no-pager" "submodule" "--quiet" "foreach"
        (format "git grep --full-name -n --no-color %s %s %s | sed s!^!$path/!"
               (if helm-git-grep-ignore-case "-i" "")
               (helm-git-grep-showing-leading-and-trailing-lines-option t)
                (mapconcat (lambda (x)
                             (format "-e %s " (shell-quote-argument x)))
                           (split-string helm-pattern " +" t)
                           "--and "))))

(defun helm-git-grep-process ()
  "Retrieve candidates from result of git grep."
  (helm-aif (helm-attr 'default-directory)
      (let ((default-directory it))
        (apply 'start-process "git-grep-process" nil "git"
               (helm-git-grep-args (helm-attr 'exclude-file-pattern))))
    '()))

(defun helm-git-submodule-grep-process ()
  "Retrieve candidates from result of git grep submodules."
  (helm-aif (helm-attr 'default-directory)
      (let ((default-directory it))
        (apply 'start-process "git-submodule-grep-process" nil
               (helm-git-submodule-grep-command)))
    '()))

(define-compilation-mode helm-git-grep-mode "Helm Git Grep"
  "Set up `wgrep' if exist."
  (require' grep)
  (set (make-local-variable 'compilation-error-face) grep-hit-face)
  (set (make-local-variable 'compilation-error-regexp-alist) grep-regexp-alist)
  (when (require 'wgrep nil t)
    (wgrep-setup-internal)))

(defun helm-git-grep-save-results-1 ()
  "Save helm git grep result in a `helm-git-grep-mode' buffer."
  (let ((prompt "GrepBufferName: ")
        (buf "*hggrep*")
        new-buf)
    (when (get-buffer buf)
      (setq new-buf (read-string prompt buf))
      (loop for b in (helm-buffer-list)
            when (and (string= new-buf b)
                      (not (y-or-n-p
                            (format "Buffer `%s' already exists overwrite? "
                                    new-buf))))
            do (setq new-buf (read-string prompt "*hggrep ")))
      (setq buf new-buf))
    (with-current-buffer (get-buffer-create buf)
      (setq buffer-read-only t)
      (let ((default-dir (helm-attr 'default-directory))
            (inhibit-read-only t))
        (erase-buffer)
        (insert (format "-*- mode: grep; default-directory: \"%s\" -*-\n\n"
                        default-dir)
                (format "Git Grep Results for `%s':\n\n" helm-input))
        (save-excursion
          (insert (with-current-buffer helm-buffer
                    (goto-char (point-min)) (forward-line 1)
                    (buffer-substring (point) (point-max)))))
        (setq default-directory default-dir)
        (helm-git-grep-mode)
        (pop-to-buffer buf)))
    (message "Helm Git Grep Results saved in `%s' buffer" buf)))


(defun helm-git-grep-action (candidate &optional where mark)
  "Define a default action for `helm-git-grep' on CANDIDATE.
WHERE can be one of `other-window', elscreen, `other-frame'.
if MARK is t, Set mark."
  (let* ((lineno (nth 0 candidate))
         (fname (or (with-current-buffer helm-buffer
                      (get-text-property (point-at-bol) 'help-echo))
                    (nth 2 candidate))))
    (case where
      (other-window (find-file-other-window fname))
      (elscreen     (helm-elscreen-find-file fname))
      (other-frame  (find-file-other-frame fname))
      (grep         (helm-git-grep-save-results-1))
      (t            (find-file fname)))
    (unless (or (eq where 'grep))
      (helm-goto-line lineno))
    (when mark
      (set-marker (mark-marker) (point))
      (push-mark (point) 'nomsg))
    ;; Save history
    (unless (or helm-in-persistent-action
                (string= helm-input ""))
      (setq helm-git-grep-history
            (cons helm-pattern
                  (delete helm-pattern helm-git-grep-history)))
      (when (> (length helm-git-grep-history)
               helm-git-grep-max-length-history)
        (setq helm-git-grep-history
              (delete (car (last helm-git-grep-history))
                      helm-git-grep-history))))))

(defun helm-git-grep-other-window (candidates)
  "Jump to result in other window from helm git grep with CANDIDATES."
  (helm-git-grep-action candidates 'other-window))

(defun helm-git-grep-other-frame (candidates)
  "Jump to result in other frame from helm git grep with CANDIDATES."
  (helm-git-grep-action candidates 'other-frame))

(defun helm-git-grep-jump-elscreen (candidates)
  "Jump to result in elscreen from helm git grep with CANDIDATES."
  (require 'elscreen)
  (if (elscreen-get-conf-list 'screen-history)
      (helm-git-grep-action candidates 'elscreen)
    (error "Elscreen is not running")))

(defun helm-git-grep-save-results (candidates)
  "Save helm git grep result in a `helm-git-grep-mode' buffer with CANDIDATES."
  (helm-git-grep-action candidates 'grep))

(defvar helm-git-grep-actions
  (delq
   nil
   `(("Find File" . helm-git-grep-action)
     ("Find file other frame" . helm-git-grep-other-frame)
     ,(and (locate-library "elscreen")
           '("Find file in Elscreen"
             . helm-git-grep-jump-elscreen))
     ("Save results in grep buffer" . helm-git-grep-save-results)
     ("Find file other window" . helm-git-grep-other-window)))
  "Actions for `helm-git-grep'.")

(defun helm-git-grep-filtered-candidate-transformer-file-line (candidates source)
  "Transform CANDIDATES to `helm-git-grep-mode' format.

Argument SOURCE is not used."
  (delq nil (mapcar 'helm-git-grep-filtered-candidate-transformer-file-line-1
                    candidates)))

(defun helm-git-grep-filtered-candidate-transformer-display
  (filename separator lineno content)
  (let ((colonp (string= separator ":")))
    (format "%s%s%s%s%s"
            (if colonp
                (propertize filename 'face compilation-info-face)
              filename)
            separator
            (if colonp
                (propertize lineno 'face compilation-line-face)
              lineno)
            separator
            content)))

(defun helm-git-grep-filtered-candidate-transformer-file-line-1 (candidate)
  "Transform CANDIDATE to `helm-git-grep-mode' format."
  (when (string-match "^\\(.+?\\)\\([:\\-]\\)\\([0-9]+\\)[:\\-]\\(.*\\)$" candidate)
    (let ((filename (match-string 1 candidate))
          (separator (match-string 2 candidate))
          (lineno (match-string 3 candidate))
          (content (match-string 4 candidate)))
      (cons (helm-git-grep-filtered-candidate-transformer-display
             filename separator lineno content)
            (list (string-to-number lineno) content
                  (expand-file-name
                   filename
                   (or (helm-interpret-value (helm-attr 'default-directory))
                       (and (helm-candidate-buffer)
                            (buffer-local-value
                             'default-directory (helm-candidate-buffer))))))))))

(defun helm-git-grep-exclude-files (exclude-file-pattern)
  "Execute `git ls-files | grep -v EXCLUDE-FILE-PATTERN'.

returning its output as a list of lines.
Signal an error if the program returns with a non-zero exit status."
  (with-temp-buffer
    (let ((status (apply
                   'call-process-shell-command
                   (format "git ls-files | grep -v -E '%s'" exclude-file-pattern)
                    nil (current-buffer) nil)))
      (unless (eq status 0)
        (error "helm-git-grep-exclude-files exited with status %s" status))
      (goto-char (point-min))
      (let (lines)
        (while (not (eobp))
          (setq lines (cons (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))
                            lines))
          (forward-line 1))
        lines))))

(defun helm-git-grep-read-exclude-file-pattern ()
  (when helm-git-grep-exclude-file-p
    (let ((pattern (read-string "Exclude files matching the pattern (regular expression): "
                                nil
                                'helm-git-grep-exclude-file-history)))
      (unless (string= pattern "")
        (helm-attrset 'exclude-file-pattern
                      (helm-git-grep-exclude-files pattern))))))

(defun helm-git-grep-init ()
  "Init `default-directory' attribute for `helm-git-grep' sources."
  (let ((default-directory (helm-git-grep-get-top-dir)))
    (helm-attrset 'default-directory default-directory)
    (helm-git-grep-read-exclude-file-pattern)))

(defun helm-git-grep-persistent-action (candidate)
  "Persistent action for `helm-git-grep'.
With a prefix arg record CANDIDATE in `mark-ring'."
  (if current-prefix-arg
      (helm-git-grep-action candidate nil 'mark)
      (helm-git-grep-action candidate))
  (helm-highlight-current-line))

;;;###autoload
(defun helm-git-grep-run-persistent-action ()
  "Run grep persistent action from `helm-git-grep'."
  (interactive)
  (helm-attrset 'jump-persistent 'helm-git-grep-persistent-action)
  (helm-execute-persistent-action 'jump-persistent))

;;;###autoload
(defun helm-git-grep-run-default-action ()
  "Run grep default action from `helm-git-grep'."
  (interactive)
  (helm-quit-and-execute-action 'helm-git-grep-action))

;;;###autoload
(defun helm-git-grep-run-other-window-action ()
  "Run grep goto other window action from `helm-git-grep'."
  (interactive)
  (helm-quit-and-execute-action 'helm-git-grep-other-window))

;;;###autoload
(defun helm-git-grep-run-other-frame-action ()
  "Run grep goto other frame action from `helm-git-grep'."
  (interactive)
  (helm-quit-and-execute-action 'helm-git-grep-other-frame))

;;;###autoload
(defun helm-git-grep-run-elscreen-action ()
  "Run grep goto elscreen action from `helm-git-grep'."
  (interactive)
  (helm-quit-and-execute-action 'helm-git-grep-jump-elscreen))

;;;###autoload
(defun helm-git-grep-run-save-buffer ()
  "Run grep save results action from `helm-git-grep'."
  (interactive)
  (helm-quit-and-execute-action 'helm-git-grep-save-results))

;;;###autoload
(defun helm-git-grep-toggle-ignore-case ()
  "Toggle ignore case option for git grep command from `helm-git-grep'."
  (interactive)
  (setq helm-git-grep-ignore-case (not helm-git-grep-ignore-case))
  (helm-run-after-quit (lambda () (helm-git-grep-1 helm-input))))

;;;###autoload
(defun helm-git-grep-toggle-showing-trailing-leading-line ()
  "Toggle show leading and trailing lines option for git grep."
  (interactive)
  (setq helm-git-grep-showing-leading-and-trailing-lines
        (not helm-git-grep-showing-leading-and-trailing-lines))
  (helm-run-after-quit (lambda () (helm-git-grep-1 helm-input))))

(defvar helm-git-grep-help-message
  "== Helm Git Grep Map ==\
\nHelm Git Grep tips:

You can toggle ignore case option of git grep.
You can save your results in a helm-git-grep-mode buffer, see below.

\nSpecific commands for Helm Grep:
\\<helm-git-grep-map>
\\[helm-goto-next-file]\t->Next File.
\\[helm-goto-precedent-file]\t\t->Precedent File.
\\[helm-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[helm-git-grep-toggle-ignore-case]\t\t->Toggle ignore case option.
\\[helm-git-grep-run-other-window-action]\t\t->Jump other window.
\\[helm-git-grep-run-other-frame-action]\t\t->Jump other frame.
\\[helm-git-grep-run-persistent-action]\t\t->Run persistent action (Same as `C-z').
\\[helm-git-grep-run-default-action]\t\t->Run default action (Same as RET).
\\[helm-git-grep-run-save-buffer]\t\t->Save to a `helm-git-grep-mode' enabled buffer.
\\[helm-git-grep-help]\t\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-git-grep-help ()
  "Help command for `helm-git-grep'."
  (interactive)
  (let ((helm-help-message helm-git-grep-help-message))
    (helm-help)))

;;;###autoload
(defvar helm-git-grep-mode-line-string"\
\\<helm-git-grep-map>\
\\[helm-git-grep-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend"
  "String displayed in mode-line in `helm-git-grep'.")

(defvar helm-git-grep-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-goto-precedent-file)
    (define-key map (kbd "C-c i")    'helm-git-grep-toggle-ignore-case)
    (define-key map (kbd "C-c n")    'helm-git-grep-toggle-showing-trailing-leading-line)
    (define-key map (kbd "C-c e")    'helm-git-grep-run-elscreen-action)
    (define-key map (kbd "C-c o")    'helm-git-grep-run-other-window-action)
    (define-key map (kbd "C-c C-o")  'helm-git-grep-run-other-frame-action)
    (define-key map (kbd "C-w")      'helm-yank-text-at-point)
    (define-key map (kbd "C-x C-s")  'helm-git-grep-run-save-buffer)
    (when helm-git-grep-use-ioccur-style-keys
      (define-key map (kbd "<right>")  'helm-git-grep-run-persistent-action)
      (define-key map (kbd "<left>")  'helm-git-grep-run-default-action))
    (define-key map (kbd "C-c ?")    'helm-git-grep-help)
    (delq nil map))
  "Keymap used in Git Grep sources.")

(define-helm-type-attribute 'git-grep
  `((default-directory . nil)
    (requires-pattern . 3)
    (volatile)
    (delayed)
    (filtered-candidate-transformer
     helm-git-grep-filtered-candidate-transformer-file-line)
    (action . ,helm-git-grep-actions)
    (history . ,'helm-git-grep-history)
    (persistent-action . helm-git-grep-persistent-action)
    (persistent-help . "Jump to line (`C-u' Record in mark ring)")
    (keymap . ,helm-git-grep-map)
    (mode-line . helm-git-grep-mode-line-string)
    (init . helm-git-grep-init)))

(defvar helm-source-git-grep
  '((name . "Git Grep")
    (candidates-process . helm-git-grep-process)
    (type . git-grep)))

(defvar helm-source-git-submodule-grep
  '((name . "Git Submodule Grep")
    (candidates-process . helm-git-submodule-grep-process)
    (type . git-grep)))

(defun helm-git-grep-1 (&optional input)
  "Execute helm git grep.
Optional argument INPUT is initial input."
  (helm :sources '(helm-source-git-grep
                   helm-source-git-submodule-grep)
        :buffer (if helm-git-grep-ignore-case "*helm git grep [i]*" "*helm git grep*")
        :input input
        :keymap helm-git-grep-map
        :candidate-number-limit helm-git-grep-candidate-number-limit))

;;;###autoload
(defun helm-git-grep ()
  "Helm git grep.

if submodules exists, grep submodules too."
  (interactive)
  (helm-git-grep-1))

;;;###autoload
(defun helm-git-grep-at-point ()
  "Helm git grep with symbol at point.

if submodules exists, grep submodules too."
  (interactive)
  (let* ((symbol (thing-at-point 'symbol))
         (input (if symbol (concat symbol " ") nil)))
    (helm-git-grep-1 input)))

;;;###autoload
(defun helm-git-grep-with-exclude-file-pattern ()
  "Helm git grep with exclude file pattern.

file pattern is iterpreted as an POSIX extended regular expression.

if submodules exists, don't grep submodules."
  (interactive)
  (let ((helm-git-grep-exclude-file-p t))
    (helm :sources helm-source-git-grep
          :buffer (if helm-git-grep-ignore-case "*helm git grep [i]*" "*helm git grep*")
          :keymap helm-git-grep-map
          :candidate-number-limit helm-git-grep-candidate-number-limit)))

;;;###autoload
(defun helm-git-grep-from-isearch ()
  "Invoke `helm-git-grep' from isearch."
  (interactive)
  (let ((input (if isearch-regexp
                   isearch-string
                   (regexp-quote isearch-string))))
    (isearch-exit)
    (helm-git-grep-1 input)))

;;;###autoload
(defun helm-git-grep-from-helm ()
  "Invoke `helm-git-grep' from other helm."
  (interactive)
  (helm-quit-and-execute-action
   '(lambda (unused)
      (helm-git-grep-1 helm-input))))


;;;###autoload
(define-obsolete-function-alias 'helm-git-grep-from-here 'helm-git-grep-at-point "0.5")

(provide 'helm-git-grep)

;;; helm-git-grep.el ends here
