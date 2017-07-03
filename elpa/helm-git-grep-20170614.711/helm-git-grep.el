;;; helm-git-grep.el --- helm for git grep, an incremental git-grep(1)

;; Copyright (C) 2013 mechairoi
;;               2013-2016 Yasuyuki Oka

;; This is a fork of anything-git-grep.el written by mechairoi.

;; Author: mechairoi
;; Maintainer: Yasuyuki Oka <yasuyk@gmail.com>
;; Version: 0.10.1
;; Package-Version: 20170614.711
;; URL: https://github.com/yasuyk/helm-git-grep
;; Package-Requires: ((helm-core "2.2.0"))

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

(declare-function wgrep-setup-internal "ext:wgrep")


(defgroup helm-git-grep nil
  "Helm for git grep."
  :prefix "helm-git-grep-"
  :group 'helm)

(defcustom helm-git-grep-sources
  '(helm-git-grep-source helm-git-grep-submodule-source)
  "Default helm sources for `helm-git-grep'.

If you don't want to search in submodules, \
Set only `helm-git-grep-source' like this:

    (setq helm-git-grep-sources '(helm-git-grep-source))"
  :group 'helm-git-grep
  :type '(repeat (choice symbol)))

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

(defcustom helm-git-grep-wordgrep nil
  "Wordgrep when matching."
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

(defcustom helm-git-grep-at-point-deactivate-mark nil
  "Deactivate the mark when `helm-git-grep-at-point' is invoked."
  :group 'helm-git-grep
  :type  'boolean)

(defcustom helm-git-grep-base-directory 'root
  "Base directory for search by git-grep(1).
Possible value are:
    root: git root directory
    current: current directory (default directory of current buffer)"
  :group 'helm-git-grep
  :type '(choice (const :tag "RootDirectory" root)
                 (const :tag "CurrentDirectory" current)))

(defcustom helm-git-grep-pathspecs nil
  "List of strings: patterns used to limit paths in git-grep(1) commands.

The pathspecs are interpreted by Git in the order given; a
pathspec starting with \":!:\" is treated as an exclusion.
Exclusions must be preceded by at least one inclusion.

Examples:

   (setq helm-git-grep-pathspecs '(\"*.txt\" \"*.rst\")
   ;; search only files matching *.txt or *.rst

   (setq helm-git-grep-pathspecs '(\"*\"  \":!:*.dvi\")
   ;; search all files except those matching *.dvi

Each pathspec need not be quoted by single quotes, because
`helm-git-grep' runs git with `start-process', which does not use
an inferior shell.

For more information about pathspec,
see https://git-scm.com/docs/gitglossary#def_pathspec.

You can see the files matched by your pathspec with:
`helm-git-grep-ls-files-limited-by-pathspec'."
  :group 'helm-git-grep
  :type '(repeat string  :tag "Pathspec"))

(defcustom helm-git-grep-doc-order-in-name-header
  '(pathspec basedir wordgrep ignorecase)
  "List of doc in name header for git-grep(1).
list of following possible values:
    pathspec: if `helm-git-grep-pathspecs' is not nil, \
availability of `helm-git-grep-pathspecs' and key of toggle command.
    basedir: value of `helm-git-grep-base-directory' \
and key of toggle command.
    wordgrep: if `helm-git-grep-wordgrep' is t, show [w] \
and key of toggle command.
    ignorecase: if `helm-git-grep-ignore-case' is t, show [i] \
and key of toggle command."
  :group 'helm-git-grep
  :type '(repeat (choice (const :tag "PathSpecs" pathspec)
                         (const :tag "BaseDirectory" basedir)
                         (const :tag "WordGrep" wordgrep)
                         (const :tag "IgnoreCase" ignorecase))))


;;; Faces
;;
;;
(defgroup helm-git-grep-faces nil
  "Customize the appearance of helm-git-grep."
  :prefix "helm-git-grep-"
  :group 'helm-git-grep)

(defface helm-git-grep-match
  '((default (:inherit helm-match)))
  "Face used to highlight git-grep(1) matches."
  :group 'helm-git-grep-faces)

(defface helm-git-grep-file
  '((default (:inherit compilation-info)))
  "Face used to highlight git-grep(1) results filenames."
  :group 'helm-git-grep-faces)

(defface helm-git-grep-line
  '((default (:inherit compilation-line-number)))
  "Face used to highlight git-grep(1) number lines."
  :group 'helm-git-grep-faces)


;; Internal.
;;
;;
(defconst helm-git-grep-pathspec-disabled-message
  (format "%s is nil, namely not activated."
          (symbol-name 'helm-git-grep-pathspecs)))

(defconst helm-git-grep-doc-order-in-name-header-plist
  '(pathspec
    (:doc
     "[helm-git-grep-pathspec-toggle-availability]:Tog.pathspec%s"
     :function
     (lambda (doc)
       (when helm-git-grep-pathspecs
         (format doc
                 (if helm-git-grep-pathspec-available "" "[disabled]")))))
    basedir
    (:doc
     "[helm-git-grep-toggle-base-directory]:Tog.basedir[%s]"
     :function
     (lambda (doc)
       (format doc (symbol-name helm-git-grep-base-directory))))
    wordgrep
    (:doc
     "[helm-git-grep-toggle-wordgrep]:Tog.wordgrep%s"
     :function
     (lambda (doc) (format doc (if helm-git-grep-wordgrep "[w]" ""))))
    ignorecase
    (:doc
     "[helm-git-grep-toggle-ignore-case]:Tog.ignorecase%s"
     :function
     (lambda (doc) (format doc (if helm-git-grep-ignore-case "[i]" ""))))))

(defvar helm-git-grep-history nil "The history list for `helm-git-grep'.")

(defvar helm-git-grep-pathspec-available t
  "Return t if `helm-git-grep-pathspec' is available in git-grep(1).")



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

(defun helm-git-grep-base-directory ()
  "Get the base directory where on execute git grep."
  (cond ((eq helm-git-grep-base-directory 'root) (helm-git-grep-get-top-dir))
        ((eq helm-git-grep-base-directory 'current) default-directory)))

(defun helm-git-grep-pathspec-args ()
  "Create arguments about pathspec."
  (when (and helm-git-grep-pathspecs helm-git-grep-pathspec-available)
    (append '("--") helm-git-grep-pathspecs)))

(defun helm-git-grep-get-top-dir ()
  "Get the git root directory."
  (let ((cwd (expand-file-name (file-truename default-directory))))
    (when (file-directory-p cwd)
      (let* ((default-directory (file-name-as-directory cwd))
             (cdup (helm-git-grep-git-string "rev-parse" "--show-cdup")))
        (when cdup
          (file-name-as-directory (expand-file-name cdup cwd)))))))

(defun helm-git-grep-showing-leading-and-trailing-lines-option (&optional strp)
  "Get num option or empty string if STRP is t."
  (if helm-git-grep-showing-leading-and-trailing-lines
      (format "-%d" helm-git-grep-showing-leading-and-trailing-lines-number)
    (when strp "")))

(defun helm-git-grep-args ()
  "Create arguments of `helm-git-grep-process' in `helm-git-grep'."
  (delq nil
        (append
         (list "--no-pager" "grep" "--null" "-n" "--no-color"
               (if helm-git-grep-ignore-case "-i" nil)
               (if helm-git-grep-wordgrep "-w" nil)
               (helm-git-grep-showing-leading-and-trailing-lines-option))
         (nbutlast
          (apply 'append
                 (mapcar
                  (lambda (x) (list "-e" x "--and"))
                  (split-string helm-pattern " +" t))))
         (helm-git-grep-pathspec-args))))

(defun helm-git-grep-submodule-grep-command ()
  "Create command of `helm-git-submodule-grep-process' in `helm-git-grep'."
  (list "git" "--no-pager" "submodule" "--quiet" "foreach"
       (format "git grep -n --no-color %s %s %s %s | sed s!^!$path/!"
               (if helm-git-grep-ignore-case "-i" "")
               (if helm-git-grep-wordgrep "-w" "")
               (helm-git-grep-showing-leading-and-trailing-lines-option t)
                (mapconcat (lambda (x)
                             (format "-e %s " (shell-quote-argument x)))
                           (split-string helm-pattern " +" t)
                           "--and "))))

(defun helm-git-grep-process ()
  "Retrieve candidates from result of git grep."
  (helm-aif (helm-attr 'base-directory)
      (let ((default-directory it))
        (apply 'start-process "git-grep-process" nil "git" (helm-git-grep-args))) '()))

(defun helm-git-grep-submodule-grep-process ()
  "Retrieve candidates from result of git grep submodules."
  (helm-aif (helm-attr 'base-directory)
      (let ((default-directory it))
        (apply 'start-process "git-submodule-grep-process" nil
               (helm-git-grep-submodule-grep-command)))
    '()))

(define-compilation-mode helm-git-grep-mode "Helm Git Grep"
  "Set up `wgrep' if exist."
  (set (make-local-variable 'compilation-error-face) compilation-info-face)
  (set (make-local-variable 'compilation-error-regexp-alist) grep-regexp-alist)
  (when (require 'wgrep nil t)
    (wgrep-setup-internal)))
(put 'helm-git-grep-mode 'mode-class 'special)
(put 'helm-git-grep-mode 'helm-only t)

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
      (let ((default-dir (helm-git-grep-base-directory))
            (inhibit-read-only t))
        (erase-buffer)
        (insert (format "-*- mode: grep; default-directory: \"%s\" -*-\n\n"
                        default-dir)
                (format "Git Grep Results by: git %s\n\n"
                        (helm-git-grep-concat-string-list
                         (helm-git-grep-args))))
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
WHERE can be one of `other-window', `other-frame'.
if MARK is t, Set mark."
  (let* ((lineno (nth 0 candidate))
         (fname (or (with-current-buffer helm-buffer
                      (get-text-property (point-at-bol) 'help-echo))
                    (nth 2 candidate))))
    (case where
          (other-window (find-file-other-window fname))
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

(defun helm-git-grep-save-results (candidates)
  "Save helm git grep result in a `helm-git-grep-mode' buffer with CANDIDATES."
  (helm-git-grep-action candidates 'grep))

(defvar helm-git-grep-actions
  (delq
   nil
   '(("Find File" . helm-git-grep-action)
     ("Find file other frame" . helm-git-grep-other-frame)
     ("Save results in grep buffer" . helm-git-grep-save-results)
     ("Find file other window" . helm-git-grep-other-window)))
  "Actions for `helm-git-grep'.")

(defun helm-git-grep-filtered-candidate-transformer-file-line (candidates source)
  "Transform CANDIDATES to `helm-git-grep-mode' format.

Argument SOURCE is not used."
  (delq nil (mapcar 'helm-git-grep-filtered-candidate-transformer-file-line-1
                    candidates)))

(defun helm-git-grep-filtered-candidate-transformer-display
    (filename lineno content)
  "Propertize FILENAME LINENO CONTENT and concatenate them."
  (format "%s:%s:%s"
      (propertize filename 'face 'helm-git-grep-file)
      (propertize lineno 'face 'helm-git-grep-line)
      (helm-git-grep-highlight-match content)))

(defun helm-git-grep-highlight-match (content)
  "Highlight matched text with `helm-git-grep-match' face in CONTENT."
  (dolist (input (delete "" (split-string helm-input)))
    (if (string-match (format ".*\\(%s\\).*" input) content)
        (put-text-property (match-beginning 1) (match-end 1)
                           'face 'helm-git-grep-match content)))
  content)

(defun helm-git-grep-filtered-candidate-transformer-file-line-1 (candidate)
  "Transform CANDIDATE to `helm-git-grep-mode' format."
  ; truncate any very long lines
  (when (> (length candidate) (window-width))
    (setq candidate (substring candidate 0 (window-width))))

  (when (string-match "^\\(.+\\)\x00\\([0-9]+\\)\x00\\(.*\\)$" candidate)
    (let ((filename (match-string 1 candidate))
          (lineno (match-string 2 candidate))
          (content (match-string 3 candidate)))
      (cons (helm-git-grep-filtered-candidate-transformer-display
             filename lineno content)
            (list (string-to-number lineno) content
                  (expand-file-name
                   filename
                   (or (helm-interpret-value (helm-attr 'base-directory))
                       (and (helm-candidate-buffer)
                            (buffer-local-value
                             'default-directory (helm-candidate-buffer))))))))))

(defun helm-git-grep-init ()
  "Initialize base-directory attribute for `helm-git-grep' sources."
  (let ((base-directory (helm-git-grep-base-directory)))
    (helm-attrset 'base-directory base-directory)))

(defun helm-git-grep-persistent-action (candidate)
  "Persistent action for `helm-git-grep'.
With a prefix arg record CANDIDATE in `mark-ring'."
  (if current-prefix-arg
      (helm-git-grep-action candidate nil 'mark)
      (helm-git-grep-action candidate))
  (helm-highlight-current-line))

(defun helm-git-grep-get-region-substring ()
  "Return the contents of region as a string."
  (buffer-substring (region-beginning) (region-end)))

(defun helm-git-grep-get-input-symbol ()
  "Get input symbol."
  (if (not mark-active)
      (thing-at-point 'symbol)
    (when (use-region-p)
      (helm-git-grep-get-region-substring))))

(defun helm-git-grep-get-isearch-input-symbol ()
  "Get input symbol from `isearch-regexp' or `isearch-string'."
  (if isearch-regexp isearch-string (regexp-quote isearch-string)))

(defun helm-git-grep-rerun-with-input ()
  "Rerun `helm-git-grep'  with current input for setting some option."
  (helm-run-after-exit (lambda () (helm-git-grep-1 helm-input))))

(defun helm-git-grep-doc-list-in-name-header ()
  "Create doc in header header for `helm-git-grep'."
  (mapcar
   (lambda (type)
     (when type
       (let* ((plist
               (plist-get helm-git-grep-doc-order-in-name-header-plist type))
              (doc (plist-get plist :doc))
              (func (plist-get plist :function))
              (ret (funcall func doc)))
         (when ret
           (substitute-command-keys
            (format "\\<helm-git-grep-map>\\%s" ret))))))
   helm-git-grep-doc-order-in-name-header))

(defun helm-git-grep-concat-string-list (list)
  "Concatenate string LIST separated by a space."
   (mapconcat 'identity(delq nil list) " "))

(defun helm-git-grep-header-name (name)
  "Create header NAME for `helm-git-grep'."
  (concat
   name "  |  "
   (helm-git-grep-concat-string-list (helm-git-grep-doc-list-in-name-header))))

(defun helm-git-grep-run-persistent-action ()
  "Run grep persistent action from `helm-git-grep'."
  (interactive)
  (helm-attrset 'jump-persistent 'helm-git-grep-persistent-action)
  (helm-execute-persistent-action 'jump-persistent))
(put 'helm-git-grep-run-persistent-action 'helm-only t)

(defun helm-git-grep-run-default-action ()
  "Run grep default action from `helm-git-grep'."
  (interactive)
  (helm-exit-and-execute-action 'helm-git-grep-action))
(put 'helm-git-grep-run-default-action 'helm-only t)

(defun helm-git-grep-run-other-window-action ()
  "Run grep goto other window action from `helm-git-grep'."
  (interactive)
  (helm-exit-and-execute-action 'helm-git-grep-other-window))
(put 'helm-git-grep-run-other-window-action 'helm-only t)

(defun helm-git-grep-run-other-frame-action ()
  "Run grep goto other frame action from `helm-git-grep'."
  (interactive)
  (helm-exit-and-execute-action 'helm-git-grep-other-frame))
(put 'helm-git-grep-run-other-frame-action 'helm-only t)

(defun helm-git-grep-run-save-buffer ()
  "Run grep save results action from `helm-git-grep'."
  (interactive)
  (helm-exit-and-execute-action 'helm-git-grep-save-results))
(put 'helm-git-grep-run-save-buffer 'helm-only t)

(defun helm-git-grep-toggle-ignore-case ()
  "Toggle ignore case option for git grep command from `helm-git-grep'."
  (interactive)
  (setq helm-git-grep-ignore-case (not helm-git-grep-ignore-case))
  (helm-git-grep-rerun-with-input))
(put 'helm-git-grep-toggle-ignore-case 'helm-only t)

(defun helm-git-grep-toggle-wordgrep ()
  "Toggle wordgrep option for git grep command from `helm-git-grep'."
  (interactive)
  (setq helm-git-grep-wordgrep (not helm-git-grep-wordgrep))
  (helm-git-grep-rerun-with-input))
(put 'helm-git-grep-toggle-wordgrep 'helm-only t)

(defun helm-git-grep-toggle-showing-trailing-leading-line ()
  "Toggle show leading and trailing lines option for git grep."
  (interactive)
  (setq helm-git-grep-showing-leading-and-trailing-lines
        (not helm-git-grep-showing-leading-and-trailing-lines))
  (helm-git-grep-rerun-with-input))
(put 'helm-git-grep-toggle-showing-trailing-leading-line 'helm-only t)

(defun helm-git-grep-toggle-base-directory ()
  "Toggle a value of `helm-git-grep-base-directory'\
for git grep command from `helm-git-grep'."
  (interactive)
  (setq helm-git-grep-base-directory
        (if (eq helm-git-grep-base-directory 'root) 'current 'root))
  (helm-git-grep-rerun-with-input))
(put 'helm-git-grep-toggle-base-directory 'helm-only t)

(defun helm-git-grep-pathspec-toggle-availability ()
  "Toggle availability of `helm-git-grep-pathspecs',\
if `helm-git-grep-pathspecs' is not nil."
  (interactive)
  (hack-dir-local-variables-non-file-buffer)
  (if helm-git-grep-pathspecs
      (progn
        (setq helm-git-grep-pathspec-available
              (not helm-git-grep-pathspec-available))
        (helm-git-grep-rerun-with-input))
    (message helm-git-grep-pathspec-disabled-message)))
(put 'helm-git-grep-pathspec-toggle-availability 'helm-only t)

;;;###autoload
(defun helm-git-grep-ls-files-limited-by-pathspec ()
  "Show result of `git ls-files' to check files limited by pathspec \
which is defined by `helm-git-grep-pathspecs'."
  (interactive)
  (hack-dir-local-variables-non-file-buffer)
  (if helm-git-grep-pathspecs
      (let ((buf (get-buffer-create "*helm-git-grep ls-files*"))
            (args (helm-git-grep-pathspec-args)))
        (with-current-buffer buf
          (erase-buffer)
          (insert (format "git ls-files %s\n\n"
                          (helm-git-grep-concat-string-list args))))
        (when (apply 'call-process "git" nil buf nil
                         (append '("ls-files") args)))
          (display-buffer buf))
    (message helm-git-grep-pathspec-disabled-message)))

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
\\[helm-git-grep-ls-files-limited-by-pathspec]\t\t->Show result of `git ls-files'\
 to check files limited by pathspec.
\\[helm-git-grep-pathspec-toggle-availability]\t\t->Toggle pathspec availability.
\\[helm-git-grep-toggle-base-directory]\t\t->Toggle base directory for search.
\\[helm-git-grep-toggle-ignore-case]\t\t->Toggle ignore case option.
\\[helm-git-grep-toggle-wordgrep]\t\t->Toggle wordgrep option.
\\[helm-git-grep-run-other-window-action]\t\t->Jump other window.
\\[helm-git-grep-run-other-frame-action]\t\t->Jump other frame.
\\[helm-git-grep-run-persistent-action]\t\t->Run persistent action (Same as `C-z').
\\[helm-git-grep-run-default-action]\t\t->Run default action (Same as RET).
\\[helm-git-grep-run-save-buffer]\t\t->Save to a `helm-git-grep-mode' enabled buffer.
\\[helm-git-grep-help]\t\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

(defun helm-git-grep-help ()
  "Help command for `helm-git-grep'."
  (interactive)
  (let ((helm-help-message helm-git-grep-help-message))
    (helm-help)))
(put 'helm-git-grep-help 'helm-only t)

(defvar helm-git-grep-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-goto-precedent-file)
    (define-key map (kbd "C-c l")    'helm-git-grep-ls-files-limited-by-pathspec)
    (define-key map (kbd "C-c p")    'helm-git-grep-pathspec-toggle-availability)
    (define-key map (kbd "C-c b")    'helm-git-grep-toggle-base-directory)
    (define-key map (kbd "C-c i")    'helm-git-grep-toggle-ignore-case)
    (define-key map (kbd "C-c w")    'helm-git-grep-toggle-wordgrep)
    (define-key map (kbd "C-c n")    'helm-git-grep-toggle-showing-trailing-leading-line)
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

(eval `(defclass helm-git-grep-class (helm-source-async)
         ((header-name :initform helm-git-grep-header-name)
          (default-directory :initform nil)
          (requires-pattern :initform 2)
          (volatile :initform t)
          (filtered-candidate-transformer
           :initform helm-git-grep-filtered-candidate-transformer-file-line)
          (action :initform ,helm-git-grep-actions)
          (history :initform ,'helm-git-grep-history)
          (persistent-action :initform helm-git-grep-persistent-action)
          (persistent-help
           :initform "Jump to line (`C-u' Record in mark ring)")
          (keymap :initform ,helm-git-grep-map)
          (init :initform helm-git-grep-init))))

(defvar helm-git-grep-source
  (helm-make-source "Git Grep" 'helm-git-grep-class
    :candidates-process 'helm-git-grep-process))

(defvar helm-git-grep-submodule-source
  (helm-make-source "Git Submodule Grep" 'helm-git-grep-class
    :candidates-process 'helm-git-grep-submodule-grep-process))

(defun helm-git-grep-1 (&optional input)
  "Execute helm git grep.
Optional argument INPUT is initial input."
  (helm-set-local-variable 'helm-git-grep-pathspecs helm-git-grep-pathspecs)
  (helm :sources helm-git-grep-sources
        :buffer "*helm git grep*"
        :input input
        :keymap helm-git-grep-map
        :candidate-number-limit helm-git-grep-candidate-number-limit))

;;;###autoload
(defun helm-git-grep ()
  "Helm git grep.

`helm-git-grep-sources' is used as helm sources."
  (interactive)
  (helm-git-grep-1))

;;;###autoload
(defun helm-git-grep-at-point ()
  "Helm git grep with symbol at point.

Use region as input instead of the thing at point
if region exists.

`helm-git-grep-sources' is used as helm sources."
  (interactive)
  (let* ((symbol (helm-git-grep-get-input-symbol))
         (input (if symbol (concat symbol " ") "")))
    (when (and helm-git-grep-at-point-deactivate-mark mark-active)
      (deactivate-mark)) ;; remove any active regions
    (helm-git-grep-1 input)))

;;;###autoload
(defun helm-git-grep-from-isearch ()
  "Invoke `helm-git-grep' from isearch."
  (interactive)
  (let ((input (helm-git-grep-get-isearch-input-symbol)))
    (isearch-exit)
    (helm-git-grep-1 input)))

;;;###autoload
(defun helm-git-grep-from-helm ()
  "Invoke `helm-git-grep' from other helm."
  (interactive)
  (helm-exit-and-execute-action
   '(lambda (unused)
      (helm-git-grep-1 helm-input))))


;;; Obsolete
;;
;;
;;;###autoload
(defconst helm-git-grep-with-exclude-file-pattern-obsolete-message
  "use `helm-git-grep-pathspecs' to exclude files form search result.")

;;;###autoload
(defun helm-git-grep-with-exclude-file-pattern ()
  "Obsolete."
  (interactive)
  (message helm-git-grep-with-exclude-file-pattern-obsolete-message))

;;;###autoload
(define-obsolete-function-alias 'helm-git-grep-from-here 'helm-git-grep-at-point "0.5")
;;;###autoload
(make-obsolete
 'helm-git-grep-with-exclude-file-pattern
 helm-git-grep-with-exclude-file-pattern-obsolete-message "0.10.0")

(define-obsolete-function-alias 'helm-git-submodule-grep-command
  'helm-git-grep-submodule-grep-command "0.10.0")

(define-obsolete-function-alias 'helm-git-submodule-grep-process
  'helm-git-grep-submodule-grep-process "0.10.0")

(define-obsolete-variable-alias 'helm-source-git-grep
  'helm-git-grep-source "0.10.0")


(provide 'helm-git-grep)

;;; helm-git-grep.el ends here
