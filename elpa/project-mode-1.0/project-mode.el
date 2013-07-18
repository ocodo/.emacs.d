;;; project-mode.el --- Define code projects. Full-text search, etc.

;; Copyright 2010-2012 Benjamin Cluff

;; Author: Benjamin Cluff <psyllo@gmail.com>
;; URL: https://github.com/psyllo/emacsenations
;; Created: 03-Feb-2010
;; Version: 1.0
;; Package-Requires: ((levenshtein "1.0"))
;;
;; Synopsis:
;;   * Finding/opening files is greatly simplified (see key bindings)
;;   * Regex search all project files.
;;   * TAGS files to do function definition lookups etc.
;;

(require 'cl)
(require 'levenshtein)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions used to setup the custom variables.

(defun project-add-to-search-exclusion-regexes (regexes)
  (setq project-search-exclusion-regexes-default
        (append project-search-exclusion-regexes-default
                regexes)))

(defun project-directories-to-regexes-for-search-exclusion (dir-names-list)
  "Convert values to regexes that can be matched against an absolute path.
   Arg `DIR-NAMES-LIST' should be like '(\"lib\" \".svn\")."
  (let ((path-boundary "[\\\\/]"))
    (mapcar (lambda (s)
              (concat path-boundary
                      (if (string-match "^\\." s)
                          (concat "\\" s)
                        s)
                      path-boundary))
            dir-names-list)))

(defun project-add-directories-to-search-exclusion-regexes (dir-names-list)
  "Convert with `PROJECT-DIRECTORIES-TO-REGEXES-FOR-SEARCH-EXCLUSION'
   then pass to `PROJECT-ADD-TO-SEARCH-EXCLUSION-REGEXES'."
  (project-add-to-search-exclusion-regexes
   (project-directories-to-regexes-for-search-exclusion
    dir-names-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup project-mode nil
  "Project mode allows users to do fuzzy and regex searches on
   file names and text within a defined set of directories and
   files that make up the project.  Multiple projects can be
   loaded at the same time and the user can switch back and forth
   between them."
  :prefix "project-"
  :group 'programming)

(defcustom project-menu-string "Project"
  "The string that appears in the menu.")

(defcustom project-search-exclusion-regexes-default
  (append
   ;; Convert values copied from `GREP-FIND-IGNORED-DIRECTORIES'
   (project-directories-to-regexes-for-search-exclusion
    '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs"))
   ;; Convert values copied from `GREP-FIND-IGNORED-FILES' to regexes
   ;; that can be matched against an absolute path.
   '("[\\\\/].#" "\\.o$" "~$" "\\.bin$" "\\.lbin$" "\\.so$" "\\.a$" "\\.ln$"
     "\\.blg$" "\\.bbl$" "\\.elc$" "\\.lof$" "\\.glo$" "\\.idx$" "\\.lot$"
     "\\.fmt$" "\\.tfm$" "\\.class$" "\\.fas$" "\\.lib$" "\\.mem$" "\\.x86f$"
     "\\.sparcf$" "\\.fasl$" "\\.ufsl$" "\\.fsl$" "\\.dxl$" "\\.pfsl$"
     "\\.dfsl$" "\\.p64fsl$" "\\.d64fsl$" "\\.dx64fsl$" "\\.lo$" "\\.la$"
     "\\.gmo$" "\\.mo$" "\\.toc$" "\\.aux$" "\\.cp$" "\\.fn$" "\\.ky$"
     "\\.pg$" "\\.tp$" "\\.vr$" "\\.cps$" "\\.fns$" "\\.kys$" "\\.pgs$"
     "\\.tps$" "\\.vrs$" "\\.pyc$" "\\.pyo$")
   ;; More files to exclude
   '("\\.jar$" "\\.class$" "\\.exe$" "\\.png$"
     "\\.gif$" "\\.jpg$" "\\.jpeg$" "\\.ico$"
     "\\.rtf$" "\\.tar$" "\\.tgz$" "\\.gz$"
     "\\.bz2$" "\\.zip$" "\\.rar$" "\\.cab$"
     "\\.dll$" "\\.pdf$" "\\.tmp$" "\\.log$"
     "\\.msi$" "\\.war$" "\\bTAGS$"))
  "File paths that match these regexes will be excluded from any type of search"
  :group 'project-mode)

(defcustom project-fuzzy-match-tolerance-default 20
  "Precentage. The higher the more tolerant fuzzy matches will be."
  :group 'project-mode)

(defcustom project-tags-form-default '(".*" ('etags))
  "Used to create a TAGS file. It is recommended that you use
  `PROJECT-ADD-TO-TAGS-FORM' to add to this form when writing an
   extension to project-mode. Useful for when extending project
   mode. The form must be like the following:
   '(\".groovy$\" ('elisp (\"regex1\" group-num)
                          (\"regex2\" group-num))
     \".clj$\"    ('etags \"-r 'etags regex argument'\"
                          \"-R 'etags regex exclusion'\")
     \".c$\"      ('etags) ; generate using etags language auto-detect
     \".js$\"     ('ignore))"
  :group 'project-mode)

(defcustom project-extension-for-saving ".proj"
  "Appended to the file name of saved projects."
  :group 'project-mode)

(defcustom project-proj-files-dir "~/.emacs.d"
  "Where project files are saved."
  :group 'project-mode)

(defcustom project-path-cache-save-p nil
  "If nil the path-cache of a project will not be saved to the project file."
  :group 'project-mode)

(defcustom project-fuzzy-search-dash-underscore-are-equal-p t
  "If non-nil dashes and underscore are equal for search purposes."
  :group 'project-mode)

(define-minor-mode project-mode
  "Toggle project mode.
   With no argument, this command toggles the mode.
   Non-null prefix argument turns on the mode.
   Null prefix argument turns off the mode."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Project"
  ;; This mode is best as a global minor mode
  :global t
  ;; The minor mode bindings.
  :keymap
  '(("\M-+n" . project-new)
    ("\M-+o" . project-open)
    ("\M-+a" . project-show-current-name)
    ("\M-+p" . project-edit-search-paths)
    ("\M-+c" . project-edit-path-cache)
    ("\M-+s" . project-save)
    ("\M-+\C-s" . project-save-all)
    ("\M-+l" . project-load-and-select)
    ([C-f5] . project-refresh)
    ("\M-+f" . project-fuzzy-search)
    ("\M-+x" . project-regex-search)
    ("\M-+e" . project-exact-search)
    ("\M-+t" . project-search-text)
    ([C-f3] . project-search-text-next)
    ([C-f4] . project-search-text-previous)
    ("\M-+yf" . project-filesystem-search)
    ("\M-+yz" . project-im-feeling-lucky-fuzzy)
    ([C-f7] . project-java-stacktrace-next)
    ([C-f8] . project-java-stacktrace-previous))
  :group 'project-mode)

;;; Hooks
(add-hook 'project-mode-hook 'project-mode-menu)

(defvar *project-current* nil
  "For project-mode. The project name string of the currently active project.
   You should almost always use the `PROJECT-CURRENT' function instead if this.")

(defvar *project-list* nil
  "For project-mode. List of projects. Projects are symbols that are uninterned and their plists contain project specific data.")

(defvar project-windows-or-msdos-p (or (string-match "^windows.*" (symbol-name system-type))
                                       (string-match "^ms-dos.*" (symbol-name system-type)))
  "Predicate indicating if this `SYSTEM-TYPE' is windows for the purpose of using the correct directory separator.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive commands

(defun project-new (project-name search-path)
  (interactive "MNew Project Name: 
DAdd a search directory to project: ")
  (when (project-find project-name)
    (error "A project by that name already exists. Project not created."))
  (let ((project (project-create project-name)))
    (project-select project)
    (project-search-paths-add project search-path)
    (project-refresh)))

(defun project-open (&optional project-name)
  (interactive)
  (if (not project-name)
      (let ((listified-project-list (mapcar (lambda (x) (list x)) *project-list*)))
        (let ((choice (completing-read "Select project: " listified-project-list nil nil nil)))
          (project-select choice)))
    (project-select project-name)))

(defun project-load-and-select (project-name)
  (interactive "MLoad project by name: ")
  (project-load project-name)
  (project-open project-name))

(defun project-load-all nil
  (interactive)
  (dolist (file (directory-files project-proj-files-dir))
    (when (string-match (concat project-extension-for-saving "$") file)
      (project-load-file (project-append-to-path project-proj-files-dir file))))
  (message "Done loading all projects"))

(defun project-show-current-name nil
  (interactive)
  (if (project-current)
      (message (concat "Current project: " (project-current-name)))
    (message "No project is currently selected.")))

(defun project-save nil
  (interactive)
  (project-ensure-current)
  (message (concat "Saving project '" (project-current-name) "'"))
  (project-write (project-current)))

(defun project-save-all nil
  (interactive)
  (dolist (project *project-list*)
    (project-write project))
  (message "Done saving all projects."))

(defun project-goto-line (line)
  (progn (goto-char (point-min))
         (forward-line (1- line))))

(defun project-file-list-edit-buffer-save nil
  (interactive)
  (project-ensure-current)
  (save-excursion
    (goto-char (point-min))
    (let ((buf (current-buffer))
          (button (next-button (point) t)))
      (let ((new-paths (project-buffer-lines-to-list buf)))
        (project-file-list-edit-buffer-save-handler buf new-paths)
        (kill-buffer buf)))))

(defun project-add-search-path (dir)
  (interactive "DAdd a search directory to project: ")
  (project-ensure-current)
  (project-search-paths-add (project-current) dir))

(defun project-im-feeling-lucky-fuzzy (file-name)
  (interactive "MI'm-feeling-lucky FUZZY search: ")
  (project-ensure-current)
  (let ((best-match (car (project-search-fuzzy (project-current) file-name))))
    (when best-match
      (find-file best-match))))

(defun project-im-feeling-lucky-regex (regex)
  (interactive "MI'm-feeling-lucky REGEX search: ")
  (project-ensure-current)
  (let ((best-match (car (project-search-regex (project-current) regex))))
    (when best-match
      (find-file best-match))))

(defun project-filesystem-search (file-name-regex)
  (interactive "MFile system REGEX search: ")
  (project-ensure-current)
  (let ((matches (project-search-filesystem (project-current) file-name-regex)))
    (when matches
      (let ((choice (completing-read "Choose: " matches nil nil nil)))
        (when choice
          (find-file choice))))))

(defun project-fuzzy-search (name)
  (interactive "MFind file FUZZY: ")
  (project-ensure-current)
  (let ((matches (project-search-fuzzy (project-current) name)))
    (if matches
        (if (= 1 (length matches))
            (find-file (car matches))
          (progn
            (setq matches (mapcar (lambda (x) (list x)) matches))
            (let ((choice (completing-read "Choose: " matches nil nil nil)))
              (when choice
                (find-file choice)))))
      (message "No reasonable matches found."))))

(defun project-regex-search (regex)
  (interactive "MFind file REGEX: ")
  (project-ensure-current)
  (let ((matches (project-search-regex (project-current) regex)))
    (when matches
      (if (> (length matches) 1)
          (progn
            (setq matches (mapcar (lambda (x) (list x)) matches))
            (let ((choice (completing-read "Choose: " matches nil nil nil)))
              (when choice
                (find-file choice))))
        (find-file (car matches))))))

(defun project-exact-search (file-name)
  (interactive "MFind file EXACT: ")
  (project-ensure-current)
  (let ((matches (project-search-exact (project-current) file-name)))
    (when matches
      (if (> (length matches) 1)
          (progn
            (setq matches (mapcar (lambda (x) (list x)) matches))
            (let ((choice (completing-read "Choose: " matches nil nil nil)))
              (when choice
                (find-file choice))))
        (find-file (car matches))))))

(defun project-search-text (regex)
  (interactive "MFull-text REGEX: ")
  (project-ensure-current)
  (when (not (> (length (replace-regexp-in-string " " "" regex)) 0))
    (error "Regex cannot be merely empty or just whitespace."))
  (let ((matches nil))
    (dolist (path (project-path-cache-get (project-current)))
      (project-run-regex-on-file path regex
                                 (lambda (p)
                                   (setq matches
                                         (append matches (list (list path p)))))))
    (when matches
      (let ((buf (generate-new-buffer (concat "*" (project-current-name) "-full-text-search-results*"))))
        (project-full-text-search-results-buffer-set (project-current) buf)
        (pop-to-buffer buf)
        (dolist (match matches)
          (insert-button (concat (first match)
                                 ":" (number-to-string (second match)))
                         'action 'project-file-offset-button-handler)
          (insert "\n"))
        (goto-char (point-min))))))

(defun project-search-text-next nil
  (interactive)
  (project-ensure-current)
  (let ((buf (project-full-text-search-results-buffer-get (project-current))))
    (when buf
      (set-buffer buf)
      (if (not (= (point) (point-min)))
          (forward-line)
        (beginning-of-line))
      (push-mark (point) t t)
      (end-of-line)
      (project-open-file-for-match-selection)))
  nil)

(defun project-search-text-previous nil
  (interactive)
  (project-ensure-current)
  (let ((buf (project-full-text-search-results-buffer-get (project-current))))
    (when buf
      (set-buffer buf)
      (forward-line -1)
      (push-mark (point) t t)
      (end-of-line)
      (project-open-file-for-match-selection)))
  nil)

(defun project-java-stacktrace-parse-line nil
  (save-excursion
    (let ((bound (progn
                   (end-of-line)
                   (point)))
          line-num
          file-name)
      (beginning-of-line)
      (when (re-search-forward "(\\(.+?\\):\\([0-9]+\\))" bound t)
        (setq file-name (match-string-no-properties 1))
        (setq line-num (string-to-number (match-string-no-properties 2))))
      (when (and file-name line-num)
        (list file-name line-num)))))

(defun project-java-stacktrace-open (file-name line-num)
  (when (and file-name
             line-num
             (project-exact-search file-name))
    (project-goto-line line-num)))

(defun project-java-stacktrace-next-n (n)
  (project-ensure-current)
  (let* ((file-loc (project-java-stacktrace-parse-line))
         (file-name (first file-loc))
         (line-num (second file-loc)))
    (if (and file-name line-num)
        (progn
          (project-stacktrace-buffer-set (project-current) (current-buffer))
          (project-java-stacktrace-open file-name line-num))
      (let ((stack-buf (project-stacktrace-buffer-get (project-current))))
        (if stack-buf
            (progn
              (set-buffer stack-buf)
              (forward-line n)
              (let* ((file-loc (project-java-stacktrace-parse-line))
                     (file-name (first file-loc))
                     (line-num (second file-loc)))
                (if (and file-name line-num)
                    (project-java-stacktrace-open file-name line-num)
                  (error "This line does not look like a Java stacktrace."))))
          (error (concat "Stacktrace buffer not set in project "
                         (project-current-name) ".")))))))

(defun project-java-stacktrace-next nil
  (interactive)
  (project-ensure-current)
  (project-java-stacktrace-next-n -1))

(defun project-java-stacktrace-previous nil
  (interactive)
  (project-ensure-current)
  (project-java-stacktrace-next-n 1))

(defun project-open-file-for-match-selection nil
  (interactive)
  (let ((match-line (buffer-substring-no-properties (region-beginning) (region-end))))
    (when (string-match ":[0-9]+$" match-line)
      (let ((file (substring match-line 0 (string-match ":[0-9]+$" match-line)))
            (p (substring match-line (string-match "[0-9]+$" match-line))))
        (when file
          (find-file file)
          (when p
            (goto-char (string-to-number p))))))))

(defun project-open-file-on-line nil
  "Open a file from the current line of text."
  (interactive)
  (beginning-of-line)
  (push-mark (point) t t)
  (end-of-line)
  (let ((file-path (buffer-substring-no-properties (region-beginning) (region-end))))
    (when file-path
      (find-file file-path))))

(defun project-edit-path-cache nil
  (interactive)
  (project-ensure-current)
  (project-create-file-list-edit-buffer (concat "*" (project-current-name) "-edit-path-cache*")
                                        (project-path-cache-get (project-current))))

(defun project-edit-search-paths nil
  (interactive)
  (project-ensure-current)
  (project-create-file-list-edit-buffer (concat "*" (project-current-name) "-edit-search-paths*")
                                        (project-search-paths-get (project-current))))

(defun project-path-cache-refresh nil
  (interactive)
  (project-ensure-current)
  (project-path-cache-create (project-current)))

(defun project-tags-refresh nil
  (interactive)
  (project-ensure-current)
  (message "Refreshing tags...")
  (project-write-tags (project-path-cache-get (project-current))
                      (project-tags-file (project-current))
                      nil
                      (project-tags-form-get (project-current)))
  (when (file-exists-p (project-tags-file (project-current)))
    (visit-tags-table (project-tags-file (project-current))))
  (message "Done refreshing tags."))

(defun project-refresh nil
  (interactive)
  (project-ensure-current)
  (project-path-cache-refresh)
  (when (not (project-disable-auto-tags-get (project-current)))
    (project-tags-refresh))
  (message (concat "Done refreshing project '" (project-current-name) "'")))

(defun project-add-to-tags-form (project file-regex s-exp)
  "The `PROJECT' arg can be nil in which case only the
`PROJECT-TAGS-FORM-DEFAULT' will be updated. Duplicates will not
be added."
  (interactive)
  (let ((new-entry (list file-regex s-exp))
        (new-tags-form (list file-regex s-exp)))
    (dolist (x (project-list-partition project-tags-form-default 2))
      (when (not (equal x new-entry))
        (setq new-tags-form (append new-tags-form x))))
    (setq project-tags-form-default new-tags-form)
    (when project
      (let ((s (project-tags-form-get project)))
        (project-tags-form-set project new-entry))))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-interactive functions

;;; For using 'editing' buffers
(defun project-create-file-list-edit-buffer (buffer-name files)
  (when (get-buffer buffer-name)
    (kill-buffer (get-buffer buffer-name)))
  (let ((buf (pop-to-buffer buffer-name)))
    (local-set-key "\C-c\C-c" 'project-file-list-edit-buffer-save)
    (dolist (file files)
      (let ((but (insert-button file
                                'action (lambda (but)
                                          (find-file-other-window
                                           (button-label but))))))
        (insert "\n")))
    buf))

(defun project-buffer-lines-to-list (buffer)
  (save-excursion
    (goto-char (point-min))
    (set-buffer buffer)
    (let (ret-val start end (continue-p t))
      (while continue-p
        (setq start (point))
        (end-of-line)
        (setq end (point))
        (let ((line (buffer-substring-no-properties start end)))
          (when (and line
                     (> (length line) 0))
            (setq ret-val (append ret-val (list line)))))
        (setq continue-p (= 0 (forward-line))))
      ret-val)))

(defun project-file-list-edit-buffer-save-handler (buffer paths)
  (if (string-match "path-cache" (buffer-name buffer))
      (project-path-cache-set (project-current) paths)
    (when (string-match "search-paths" (buffer-name buffer))
      (project-search-paths-set (project-current) paths))))

;;; Tags
(defun project-write-tags (path-cache tags-file append-p tags-form)
  (when (not (evenp (length tags-form)))
    (error "Invalid `TAGS-FORM' parameter"))
  (when (and (not append-p)
             (file-exists-p tags-file))
    (with-temp-buffer
      (write-file tags-file))) ; truncate tags file
  (dolist (file path-cache)
    (let ((tags-form tags-form))
      (while (and (stringp file)
                  (first tags-form)
                  (second tags-form))
        (let ((path-regex (first tags-form))
              (regexes (second tags-form)))
          (setq tags-form (cddr tags-form)) ; move ahead 2
          (when (string-match path-regex file)
            (let ((tag-gen-method (car regexes))
                  (regexes (if (stringp (car regexes))
                               regexes
                             (cdr regexes))))
              (if (and (not (stringp tag-gen-method))
                       (equal 'etags (second tag-gen-method)))
                  (project-write-tags-for-file-with-etags file tags-file t regexes)
                (project-write-tags-for-file-with-elisp file tags-file t regexes)))))))))

(defun project-write-tags-for-file-with-elisp (input-file tags-file append-p regexes)
  (let ((tags (project-generate-tags-for-file-with-elisp input-file regexes)))
    (when tags
      (let ((data (mapconcat 'identity tags "\n")))
        (with-temp-buffer
          (insert "\n"
                  input-file "," (number-to-string (length data)) "\n"
                  data "\n")
          (write-region (point-min) (point-max) tags-file append-p))))))

(defun project-generate-tags-for-file-with-elisp (file regexes)
  "Generates a list of tag file entry lines for one file for the given regexes."
  (let (ret-val)
    (with-temp-buffer
      (insert-file-contents file)
      (let (entries)
        (dolist (regex regexes)
          (goto-char (point-min))
          (while (re-search-forward (first regex) nil t)
            (let (byte-offset line match)
              (setq match (match-string (second regex)))
              (setq byte-offset (- (point) (length match)))
              (setq line (line-number-at-pos))
              (setq entries (append entries (list (concat match ""
                                                          (number-to-string line) ","
                                                          (number-to-string byte-offset))))))))
        entries))))

(defun project-write-tags-for-file-with-etags (input-file tags-file append-p &optional regex-args)
  (let ((cmd-string (combine-and-quote-strings (append (list "etags" (when append-p "-a")
                                                             "-o" tags-file
                                                             input-file)
                                                       regex-args))))
    (call-process-shell-command cmd-string)))

(defun project-tags-file (project)
  (project-append-to-path (project-default-directory project) "TAGS"))

(defun project-tags-form-get (project)
  (or (get project 'tags-form)
      project-tags-form-default))

(defun project-tags-form-set (project value)
  (put project 'tags-form value))

(defun project-disable-auto-tags-get (project)
  (get project 'disable-auto-tags))

(defun project-disable-auto-tags-set (project value)
  "Project-mode can automatically handle the generation of tags
   files from the files listed in the path-cache if
   `TAGS-FORM' is populated correctly."
  (put project 'disable-auto-tags value))

(defun project-enable-auto-tags-for-other-file-types-get (project)
  (get project 'enable-auto-tags-for-other-file-types))

(defun project-enable-auto-tags-for-other-file-types-set (project value)
  "Generate tags for file types found in path-cache and that have
   not already been processed using `TAGS-FORM'."
  (put project 'enable-auto-tags-for-other-file-types value))

(defun project-put (project sym val)
  (put project sym val))

(defun project-get (project sym)
  (get project sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other

(defun project-load (project-name)
  (message (concat "Loading project from file: " (project-file project-name)))
  (project-load-file (project-file project-name))
  (message (concat "Done loading project from file: " (project-file project-name))))

(defun project-load-file (project-file)
  (with-temp-buffer
    (insert-file-contents project-file)
    (goto-char (point-min))
    (eval (read (current-buffer)))))

(defun project-file (project)
  (let ((project (if (symbolp project)
                     (project-name project)
                   project)))
    (project-append-to-path project-proj-files-dir
                            (concat project project-extension-for-saving))))

(defun project-write (project)
  (let ((data (project-as-data project)))
    (when data
      (with-temp-buffer
        (insert (pp-to-string data))
        (write-file (project-file project))))))

(defun project-as-data (project)
  `(progn
     (let ((project (project-create ,(project-name project))))
       (project-search-paths-set              project  ',(project-search-paths-get              project))
       (project-tags-form-set                 project  ',(project-tags-form-get                 project))
       (project-search-exclusion-regexes-set  project  ',(project-search-exclusion-regexes-get  project))
       (project-fuzzy-match-tolerance-set     project  ,(project-fuzzy-match-tolerance-get      project))
       ,(when project-path-cache-save-p
          `(project-path-cache-set            project  ',(project-path-cache-get                project))))))

(defun project-search-exclusion-regexes-get (project)
  (or (get project 'search-exclusion-regexes)
      project-search-exclusion-regexes-default))

(defun project-search-exclusion-regexes-set (project value)
  (put project 'search-exclusion-regexes value))

(defun project-fuzzy-match-tolerance-get (project)
  (or (get project 'fuzzy-match-tolerance)
      project-fuzzy-match-tolerance-default))

(defun project-fuzzy-match-tolerance-set (project value)
  (put project 'fuzzy-match-tolerance value))

(defun project-ensure-current nil
  (when (not (project-current))
    (error "No project selected.")))

(defun project-ensure-path-cache (project)
  (let ((paths (project-path-cache-get project)))
    (when (not paths)
      (project-path-cache-create project))))

(defun project-run-regex-on-file (file regex match-handler)
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((p nil))
          (while (condition-case nil
                     (setq p (re-search-forward regex))
                   (error nil))
            (funcall match-handler p))))
    (message (concat file " exists in project path-cache but not on file system."))))

(defun project-find (project)
  "If project found return it, else nil.
  `PROJECT' can be a string or symbol."
  (when (stringp project)
    (setq project (make-symbol project)))
  (let ((projects *project-list*))
    (while (and (car projects)
                (not (project-equal (car projects) project)))
      (setq projects (cdr projects)))
    (when (project-equal (car projects) project)
      (car projects))))

(defun project-select (project)
  (let ((project (project-find project)))
    (if project
        (progn
          (setq *project-current* project)
          (let ((new-default-path (project-default-directory project)))
            (when new-default-path
              (cd new-default-path)
              (when (and (project-search-paths-get project)
                         (not (project-path-cache-get project)))
                (project-path-cache-create project))
              (let ((tags-file (project-tags-file project)))
                (when (file-exists-p tags-file)
                  (visit-tags-table tags-file))))))
      (message "That project doesn't exist."))))

(defun project-current nil
  (project-find *project-current*))

(defun project-current-name nil
  (let ((p (project-find (project-current))))
    (when p
      (project-name p))))

(defun project-name (project)
  (symbol-name project))

(defun project-create (project-name)
  "Creates a new project and adds it to the list"
  (let ((project (project-find (make-symbol project-name))))
    (when (not project)
      (setq project (make-symbol project-name))
      (setq *project-list* (append *project-list* (list project))))
    project))

(defun project-equal (project-sym1 project-sym2)
  (equal (project-name project-sym1) (project-name project-sym2)))

(defun project-properties-set (project new-plist)
  (setplist project new-plist))

(defun project-properties-get (project)
  (symbol-plist project))

(defun project-path-cache-create (project)
  (let ((matches nil))
    (message "Creating project path-cache...")
    (dolist (path (project-search-paths-get project))
      (project-filesystem-traverse :query nil
                                   :looking-at path
                                   :test
                                   (lambda (query file-path)
                                     (let ((regexes (append (project-search-exclusion-regexes-get project)))
                                           (add-p t))
                                       (while (and (car regexes)
                                                   add-p)
                                         (when (string-match (car regexes) file-path)
                                           (setq add-p nil))
                                         (setq regexes (cdr regexes)))
                                       add-p))
                                   :match-handler
                                   (lambda (add-p file-path)
                                     (if add-p
                                         (setq matches (append matches (list file-path)))
                                       (setq add-p nil)))))
    (message (concat "Done creating project path-cache. Cached "
                     (number-to-string (length matches)) " file paths."))
    (project-path-cache-set project matches)))

(defun project-path-cache-set (project paths-list)
  (put project 'path-cache paths-list))

(defun project-path-cache-get (project)
  (get project 'path-cache))

(defun project-search-paths-set (project paths-list)
  (when (not (listp paths-list))
    (error "`PROJECT-SEARCH-PATHS-SET' accepts only a LIST."))
  (put project 'search-paths paths-list))

(defun project-search-paths-get (project)
  (get project 'search-paths))

(defun project-default-directory (project)
  (car (get project 'search-paths)))

(defun project-search-paths-add (project &rest new-paths)
  (when (stringp new-paths)
    (setq new-paths (list (project-fix-dir-separators-in-path-if-windows new-paths))))
  (put project 'search-paths (append (get project 'search-paths) new-paths)))

(defun* project-path-cache-traverse (&key (project nil)
                                          (name nil)
                                          (test nil)
                                          (match-handler nil))
  (project-ensure-path-cache project)
  (dolist (path (project-path-cache-get project))
    (let* ((file-path (project-path-file-name path))
           (test-results (funcall test name file-path)))
      (when test-results
        (funcall match-handler test-results path)))))

(defun project-search-filesystem (project file-name-regex)
  (let (matches)
    (dolist (dir (project-search-paths-get project))
      (project-filesystem-traverse :query file-name-regex
                                   :looking-at dir
                                   :test (lambda (query x) (string-match query x))
                                   :match-handler
                                   (lambda (test-result file-path)
                                     (setq matches (append matches (list file-path))))))
    matches))

(defun project-search-fuzzy (project file-name &optional tolerance)
  (when (not tolerance)
    (setq tolerance (project-fuzzy-match-tolerance-get project)))
  (let ((matches nil))
    (project-path-cache-traverse :project project
                                 :name file-name
                                 :test 'project-fuzzy-distance-pct-for-files
                                 :match-handler
                                 (lambda (test-result file-path)
                                   (when (<= test-result tolerance)
                                     (setq matches (append matches (list file-path))))))
    (sort matches (lambda (a b)
                    (when (< (project-fuzzy-distance-pct-for-files a file-name)
                             (project-fuzzy-distance-pct-for-files b file-name))
                      t)))))

(defun* project-search-regex (project regex)
  (let ((matches nil))
    (project-path-cache-traverse :project project
                                 :name regex
                                 :test (lambda (regex x) (string-match regex x))
                                 :match-handler
                                 (lambda (test-result file-path)
                                   (setq matches (append matches (list file-path)))))
    (sort matches (lambda (a b)
                    (let ((a-pos (string-match regex a))
                          (b-pos (string-match regex b)))
                      (if (and a-pos
                               b-pos)
                          (if (= a-pos b-pos) ; when earliest match is a tie take the shortest string
                              (<= (length a)
                                  (length b))
                            (<= a-pos
                                b-pos))
                        (if a
                            t
                          nil)))))))

(defun* project-search-exact (project file-name)
  (let ((matches nil))
    (project-path-cache-traverse :project project
                                 :name file-name
                                 :test (lambda (file-name x) (string-equal file-name x))
                                 :match-handler
                                 (lambda (test-result file-path)
                                   (setq matches (append matches (list file-path)))))
    matches))

(defun project-full-text-search-results-buffer-get (project)
  (get (project-current) 'project-full-text-search-results-buffer))


(defun project-full-text-search-results-buffer-set (project buf)
  (put (project-current) 'project-full-text-search-results-buffer buf))

(defun project-stacktrace-buffer-get (project)
  (get (project-current) 'project-stacktrace-buffer))

(defun project-stacktrace-buffer-set (project buf)
  (put (project-current) 'project-stacktrace-buffer buf))

(defun project-file-path-normalize-for-fuzzy-search (s)
  (if project-fuzzy-search-dash-underscore-are-equal-p
      (replace-regexp-in-string "-" "_" s)
    s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions that have no knowledge of the concept of projects

(defun* project-fuzzy-distance-pct-for-files (file1 file2 &optional (ignore-ext t))
  (if ignore-ext
      (project-fuzzy-distance-pct (project-file-path-normalize-for-fuzzy-search
                                   (project-file-strip-extension file1))
                                  (project-file-path-normalize-for-fuzzy-search
                                   (project-file-strip-extension file2)))
    (project-fuzzy-distance-pct file1 file2)))

(defun* project-filesystem-traverse (&key (query nil)
                                          (looking-at nil)
                                          (parent-dir nil)
                                          (test nil)
                                          (match-handler nil))
  (when  (and looking-at
              (> (length looking-at) 0)
              (not (string-equal "." looking-at))
              (not (string-equal ".." looking-at)))
    (let ((file-path (project-append-to-path parent-dir looking-at)))
      ;; TODO: Check for cyclical references. Following a symlink
      ;; directory could result in infinite recursion of that
      ;; directory contains another symlink back the first directory.
      (if (file-directory-p file-path)
          ;; Handle directory
          (when (funcall test query (project-add-trailing-dirsep file-path))
            (dolist (file (directory-files file-path))
              (project-filesystem-traverse :query query :looking-at file :parent-dir file-path
                                           :test test :match-handler match-handler)))
        ;; Handle file
        (when (and test match-handler)
          (let ((test-results (funcall test query looking-at)))
            (when test-results
              (funcall match-handler test-results file-path))))))))

(defun project-file-line-button-handler (but)
  "Examines the button label for the file path and line number.
   The button label should look like '/path/foo/bar.txt:29'
   Where '29' is the line number"
  (let ((colon-pos (string-match ":[0-9]+" (button-label but))))
    (let ((file-path (substring (button-label but) 0 colon-pos))
          (line (string-to-number
                 (substring (button-label but) (+ 1 colon-pos) (length (button-label but))))))
      (find-file file-path)
      (project-goto-line line)
      (push-mark (point) t t)
      (end-of-line))))

(defun project-file-offset-button-handler (but)
  "Examines the button lable for the file path and offset number.
   The button label should looke like '/path/foo/bar.txt:825'
   Where '825' is the offset in the buffer."
  (let ((colon-pos (string-match ":[0-9]+" (button-label but))))
    (let ((file-path (substring (button-label but) 0 colon-pos))
          (offset (string-to-number
                   (substring (button-label but) (+ 1 colon-pos) (length (button-label but))))))
      (find-file file-path)
      (goto-char offset))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions. Function independently.

(defun project-list-partition (l n)
  (assert (zerop (mod (length l) n)))
  (loop for l on l by #'(lambda (l) (nthcdr n l)) collect (subseq l 0 n)))

(defun project-remove-trailing-dirsep (dir-path)
  (when dir-path
    (substring dir-path 0 (string-match "[\\\\/]*$" dir-path))))

(defun project-add-trailing-dirsep (dir-path)
  (if (string-match "[^\\\\/]$" dir-path)
      (concat dir-path "/")
    dir-path))

(defun project-path-file-name (path)
  (replace-regexp-in-string ".*[\\\\/]+" "" path))

(defun project-append-to-path (dir-path str-or-list)
  (when dir-path
    (setq dir-path (if (listp dir-path)
                       (mapconcat 'identity dir-path "/")
                     dir-path)))
  (if (stringp str-or-list)
      (if (and dir-path str-or-list)
          (concat (project-remove-trailing-dirsep dir-path) "/" str-or-list)
        (if dir-path
            (project-remove-trailing-dirsep dir-path)
          (if str-or-list
              (project-remove-trailing-dirsep str-or-list))))
    (when (listp str-or-list)
      (let ((retVal dir-path))
        (dolist (x str-or-list)
          (setq retVal (project-append-to-path retVal x)))
        retVal))))

(defun project-fix-dir-separators-in-path-if-windows (path)
  (when project-windows-or-msdos-p
    (replace-regexp-in-string "\\\\" "/" path)))

(defun project-fuzzy-distance-pct (str1 str2)
  (let ((distance (levenshtein-distance str1 str2)))
    (/ (* distance 100)
       (length (if (< (length str1) (length str2))
                   str1
                 str2)))))

(defun project-strip-file-extensions (file-path extensions-regex-list)
  (let ((new-file-path file-path))
    (while (and (car extensions-regex-list)
                (string-equal file-path new-file-path))
      (setq new-file-path (replace-regexp-in-string (car extensions-regex-list)
                                                    "" file-path))
      (setq extensions-regex-list (cdr extensions-regex-list)))
    new-file-path))


(defun project-file-strip-extension (file-path)
  (if (string-match "[^^]\\.[^.]+$" file-path)
      (substring file-path 0 (string-match "\\.[^.]+$" file-path))
    file-path))


(defun project-file-get-extension (file-path)
  (when (string-match "[^^]\\.[^.]+$" file-path)
    (substring file-path (string-match "\\.[^.]+$" file-path))))

(defun project-buffer-name-without-<x> nil
  (substring (buffer-name) 0 (string-match "\\(<[0-9]+>\\|$\\)" (buffer-name))))

(defun project-path-as-list (file-or-dir)
  (split-string file-or-dir "[\\\\/]"))

(defun project-list-as-path (l)
  (mapconcat 'identity l "/"))

(defun project-find-dir-with-dir-for-file (file-name parent-dir-name)
  (let ((parts (project-path-as-list file-name)))
    (block nil
      (while (setq parts (butlast parts))
        (let ((dir (project-append-to-path parts parent-dir-name)))
          (when (file-exists-p dir)
            (return (project-list-as-path parts))))))))

(defun project-dir-in-file-path-p (file-name dir-name)
  (let ((parts (project-path-as-list file-name)))
    (block nil
      (dolist (part parts)
        (when (equal part dir-name)
          (return t))))))

(defun project-file-basename (path)
  (substring path
             (string-match "[^\\\\/]+$" path)
             (length path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu

(defun project-mode-menu nil
  (if (not project-mode)
      (global-unset-key [menu-bar projmenu])
    (progn
      (define-key-after
        global-map
        [menu-bar projmenu]
        (cons project-menu-string (make-sparse-keymap))
        'tools)

      ;; Stacktrace
      (define-key
        global-map
        [menu-bar projmenu projstackt]
        (cons "Stacktraces" (make-sparse-keymap)))

      (define-key
        global-map
        [menu-bar projmenu projstackt jstackprev]
        '("Java Stacktrace Previous" . project-java-stacktrace-previous))

      (define-key
        global-map
        [menu-bar projmenu projstackt jstacknext]
        '("Java Stacktrace Next" . project-java-stacktrace-next))

      ;; Searching
      (define-key
        global-map
        [menu-bar projmenu projsrch]
        (cons "Search" (make-sparse-keymap)))

      (define-key
        global-map
        [menu-bar projmenu projsrch srchfs]
        '("Regex File Name (filesystem)" . project-filesystem-search))

      (define-key
        global-map
        [menu-bar projmenu projsrch lckyreg]
        '("I'm feeling lucky regex" . project-im-feeling-lucky-regex))

      (define-key
        global-map
        [menu-bar projmenu projsrch lckyfuz]
        '("I'm feeling lucky fuzzy" . project-im-feeling-lucky-fuzzy))

      (define-key
        global-map
        [menu-bar projmenu projsrch srchtpm]
        '("Full-Text Prev Match" . project-search-text-previous))

      (define-key
        global-map
        [menu-bar projmenu projsrch srchtnm]
        '("Full-Text Next Match" . project-search-text-next))

      (define-key
        global-map
        [menu-bar projmenu projsrch srchregexft]
        '("Regex Full-Text" . project-search-text))

      (define-key
        global-map
        [menu-bar projmenu projsrch srchexactfn]
        '("Exact File Name" . project-exact-search))

      (define-key
        global-map
        [menu-bar projmenu projsrch srchregexfn]
        '("Regex File Name" . project-regex-search))

      (define-key
        global-map
        [menu-bar projmenu projsrch srchfuz]
        '("Fuzzy File Name" . project-fuzzy-search))

      ;; Refresh
      (define-key
        global-map
        [menu-bar projmenu projref]
        (cons "Refresh" (make-sparse-keymap)))

      (define-key
        global-map
        [menu-bar projmenu projref projtref]
        '("Refresh Project Tags" . project-tags-refresh))

      (define-key
        global-map
        [menu-bar projmenu projref projpcref]
        '("Refresh Project Path Cache" . project-path-cache-refresh))

      (define-key
        global-map
        [menu-bar projmenu projref projrefall]
        '("Refresh All" . project-refresh))

      ;; Project info
      (define-key
        global-map
        [menu-bar projmenu curproj]
        (cons "Current Project" (make-sparse-keymap)))

      (define-key
        global-map
        [menu-bar projmenu curproj pvcp]
        '("Edit Project Path Cache" . project-edit-path-cache))

      (define-key
        global-map
        [menu-bar projmenu curproj pvsp]
        '("Edit Project Search Paths" . project-edit-search-paths))

      (define-key
        global-map
        [menu-bar projmenu curproj pscn]
        '("View Project Name" . project-show-current-name))

      ;; Top
      (define-key
        global-map
        [menu-bar projmenu projloadall]
        '("Load All Projects" . project-load-all))

      (define-key
        global-map
        [menu-bar projmenu  projload]
        '("Load Project" . project-load-and-select))

      (define-key
        global-map
        [menu-bar projmenu  projsaveall]
        '("Save All Projects" . project-save-all))

      (define-key
        global-map
        [menu-bar projmenu projsave]
        '("Save Project" . project-save))

      (define-key
        global-map
        [menu-bar projmenu projopen]
        '("Open Project" . project-open))

      (define-key
        global-map
        [menu-bar projmenu projnew]
        '("New Project" . project-new))))

  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'project-mode)

;;; project-mode.el ends here
