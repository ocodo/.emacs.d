;;; elgrep.el --- Searching files for regular expressions -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Tobias Zawada

;; Author: Tobias Zawada <naehring@smtp.1und1.de>
;; Keywords: tools, matching, files, unix
;; Package-Version: 20180302.527
;; Version: 1
;; URL: https://github.com/TobiasZawada/elgrep
;; Package-Requires: ((emacs "25.1"))

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

;; Open the `elgrep-menu' via menu item "Tools" -> "Search files (Elgrep)...".
;; There are menu items for the directory, the file name regexp for filtering
;; and the regexp for grepping.
;; Furthermore, you can also switch on recursive grep.
;;
;; Run M-x elgrep to search a single directory for files with file
;; name matching a given regular expression for text matching a given
;; regular expression.
;; With prefix arg it searches the directory recursively.

;;; Code:

(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(require 'cl-lib)
(require 'easymenu)
(require 'grep) ; also provides "compile"

;; For safity reasons `elgrep-edit' exploits the text properties
;; `elgrep-context', `elgrep-context-begin', `elgrep-context-end'
;; This implies that `elgrep-edit' does not work anymore with
;; `grep-mode' allone.
;; Therefore, we introduce a new mode for listing the elgrep matches:
(define-derived-mode elgrep-mode grep-mode "elgrep"
  "Major mode for elgrep buffers.
See `elgrep' and `elgrep-menu' for details."
  (setq header-line-format (substitute-command-keys "Quit (burry-buffer): \\[quit-window]; go to occurence: \\[compile-goto-error]; elgrep-edit-mode: \\[elgrep-edit-mode]")))

(define-key elgrep-mode-map (kbd "C-c C-e") #'elgrep-edit-mode)
(easy-menu-define nil elgrep-mode-map
  "Menu for `elgrep-mode'."
  '("Elgrep"
    ["Elgrep-edit" elgrep-edit-mode t]))

(defvar elgrep-file-name-re-hist nil
  "History of file-name regular expressions for `elgrep' (which see).")
(defvar elgrep-re-hist nil
  "History of regular expressions for `elgrep' (which see).")

(defun elgrep-insert-file-contents (filename &optional visit)
  "Like `insert-file-contents' for FILENAME.
It uses `pdftotext' (poppler) for pdf-files (with file extension pdf).
VISIT is passed as second argument to `insert-file-contents'."
  (if (string-match (downcase filename) "\.pdf\\'")
      (call-process "pdftotext" filename (current-buffer) visit "-" "-")
    (insert-file-contents filename visit)))

(defun elgrep-dired-files (files)
  "Print FILES in `dired-mode'."
  (insert "  " default-directory ":\n  elgrep 0\n")
  (dolist (file files)
    (let ((a (file-attributes file 'string)))
      (insert (format "  %s %d %s %s %6d %s %s\n"
		      (nth 8 a) ; file modes like ls -l
		      (nth 1 a) ; number of links to file
		      (nth 2 a) ; uid as string
		      (nth 3 a) ; gid as string
		      (nth 7 a) ; size in bytes
		      (format-time-string "%d. %b %Y" (nth 5 a)) ; modification time
		      file))))
  (dired-mode)
  (dired-build-subdir-alist))

(defmacro elgrep-line-position (num-or-re pos-op search-op)
  "If NUM-OR-RE is a number then act like (POS-OP (1+ NUM-OR-RE)).
Thereby POS-OP is `line-end-position' or `line-beginning-position'.
If NUM-OR-RE is a regular expression search with SEARCH-OP for that RE
and return `line-end-position' or `line-beginning-position'
of the line with the match, respectively."
  `(if (stringp ,num-or-re)
       (save-excursion
	 (save-match-data
	   (,search-op ,num-or-re nil t) ;; t=noerror
	   (,pos-op)
	   ))
     (,pos-op (and ,num-or-re (1+ ,num-or-re)))))

(defun elgrep-classify (classifier list &rest options)
  "Use CLASSIFIER to map the LIST entries to class denotators.
Returns the list of equivalence classes.  Each equivalence class
is a cons whose `car' is the class denotator and the cdr is the
list of members.

Accept a plist of OPTIONS.
Keywords supported: :test"
  (let ((test (or (plist-get options :test) 'equal)))
    (let (classify-res)
      (dolist (classify-li list)
	(let* ((classify-key (funcall classifier classify-li))
	       (classify-class (cl-assoc classify-key classify-res :test test)))
	  (if classify-class
	      (setcdr classify-class (cons classify-li (cdr classify-class)))
	    (setq classify-res (cons (list classify-key classify-li) classify-res)))))
      classify-res)))

(defun elgrep-default-filename-regexp (&optional dir)
  "Create default filename regexp from the statistical analysis of files in DIR which defaults to `default-directory'."
  (unless dir (setq dir default-directory))
  (let* ((filelist (cl-delete-if (lambda (file) (string-match "\\.\\(~\\|bak\\)\\'" file))
				 (directory-files dir)))
	 (ext (car-safe (cl-reduce (lambda (x y) (if (> (length x) (length y)) x y)) (elgrep-classify 'file-name-extension filelist))));; most often used extension
	 )
    (concat "\\." ext "\\'")))

(defvar elgrep-w-dir)
(defvar elgrep-w-file-name-re)
(defvar elgrep-w-re)
(defvar elgrep-w-recursive)
(defvar elgrep-w-c-beg)
(defvar elgrep-w-c-end)
(defvar elgrep-w-exclude-file-re)
(defvar elgrep-w-dir-re)
(defvar elgrep-w-exclude-dir-re)
(defvar elgrep-w-search-fun)

(defun elgrep-widget-value-update-hist (wid)
  "Get value of widget WID and update its :prompt-history variable."
  (when-let ((ret (widget-value wid))
	     (hist-var (widget-get wid :prompt-history))
	     (hist-length (or (get hist-var 'history-length) history-length)))
    (unless (string-equal ret (car-safe (symbol-value hist-var)))
      (set hist-var (cons ret (symbol-value hist-var)))
      (when (> (length (symbol-value hist-var)) hist-length)
	(setf (nthcdr hist-length (symbol-value hist-var)) nil)))
    ret))

(defun elgrep-menu-elgrep ()
  "Start `elgrep' with the start-button from `elgrep-menu'."
  (interactive "@")
  (elgrep (elgrep-widget-value-update-hist elgrep-w-dir)
          (elgrep-widget-value-update-hist elgrep-w-file-name-re)
          (elgrep-widget-value-update-hist elgrep-w-re)
          :recursive (widget-value elgrep-w-recursive)
          :c-beg (- (widget-value elgrep-w-c-beg))
          :c-end (widget-value elgrep-w-c-end)
          :exclude-file-re (elgrep-widget-value-update-hist elgrep-w-exclude-file-re)
          :dir-re (elgrep-widget-value-update-hist elgrep-w-dir-re)
          :exclude-dir-re (elgrep-widget-value-update-hist elgrep-w-exclude-dir-re)
          :interactive t
          :search-fun (widget-value elgrep-w-search-fun)))

(defmacro elgrep-menu-with-buttons (buttons &rest body)
  "Define BUTTONS and execute BODY with keymaps for widgets.
BUTTONS is a list of button definitions (KEYMAP-NAME FUNCTION).
See definition of `elgrep-menu' for an example."
  (declare (debug (sexp body))
	   (indent 1))
  (append (list #'let (mapcar (lambda (button)
				(list (car button) '(make-sparse-keymap)))
			      buttons))
	  (mapcar (lambda (button)
		    `(progn
		       (define-key ,(car button) (kbd "<down-mouse-1>") ,(cadr button))
		       (define-key ,(car button) (kbd "<down-mouse-2>") ,(cadr button))
		       (define-key ,(car button) (kbd "<return>") ,(cadr button))))
		  buttons)
	  body))

(defvar-local elgrep-menu-hist-pos nil
  "Current position in text widget history.
Used in `elgrep-menu-hist-up' and `elgrep-menu-hist-down'.")

(let (hist) ;; We exploit lexical binding here!
  (defun elgrep-menu-hist-move (dir)
    "Move in :prompt-history of widget at point in direction DIR which can be -1 or +1."
    (when-let ((wid (widget-at))
               (histvar (widget-get wid :prompt-history)))
      (unless hist (setq hist (cons (widget-value wid) (symbol-value histvar))))
      (unless (memq last-command '(elgrep-menu-hist-up elgrep-menu-hist-down))
        (setq hist (cons (widget-value wid) (symbol-value histvar)))
        (setq elgrep-menu-hist-pos 0))
      (let ((start elgrep-menu-hist-pos))
        (while
            (progn
              (setq elgrep-menu-hist-pos (mod (+ elgrep-menu-hist-pos dir) (length hist)))
              (condition-case nil
                  (progn
                    (widget-value-set wid (nth elgrep-menu-hist-pos hist))
                    nil)
                (error (/= elgrep-menu-hist-pos start)))))))))

(defun elgrep-menu-hist-up ()
  "Choose next item in :prompt-history of widget at point."
  (interactive)
  (elgrep-menu-hist-move 1))

(defun elgrep-menu-hist-down ()
  "Choose next item in :prompt-history of widget at point."
  (interactive)
  (elgrep-menu-hist-move -1))

(defvar elgrep-menu-hist-map (let ((map (copy-keymap widget-field-keymap)))
			       (define-key map (kbd "<M-up>") #'elgrep-menu-hist-up)
                               (define-key map (kbd "ESC <up>") #'elgrep-menu-hist-up)
			       (define-key map (kbd "<M-down>") #'elgrep-menu-hist-down)
                               (define-key map (kbd "ESC <down>") #'elgrep-menu-hist-down)
			       map)
  "Widget menu used for text widgets with history.
Binds M-up and M-down to one step in history up and down, respectively.")

(defun elgrep-wid-dir-to-internal (wid value)
  "Assert that the value of WID is a dir and return VALUE."
  (cl-assert (and (stringp value)
                  (file-directory-p value))
             nil
             (format "The value of widget %S must be a directory" (widget-get wid :format)))
  value)

;;;###autoload
(defun elgrep-menu (&optional reset)
  "Present a menu with most of the parameters for `elgrep'.
Reset the menu entries if RESET is non-nil.
You can adjust the parameters there and start `elgrep'."
  (interactive)
  (if (and (buffer-live-p (get-buffer "*elgrep-menu*"))
	   (null reset))
      (switch-to-buffer "*elgrep-menu*")
    (switch-to-buffer "*elgrep-menu*")
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (elgrep-menu-with-buttons ((elgrep-menu-start-map #'elgrep-menu-elgrep)
			       (elgrep-menu-bury-map #'bury-buffer)
			       (elgrep-menu-reset-map (lambda () (interactive "@") (elgrep-menu t))))
      (buffer-disable-undo)
      (let ((caption "Elgrep Menu"))
	(widget-insert (concat caption "
" (make-string (length caption) ?=) "

Hint: Try <M-tab> for completion, and <M-up>/<M-down> for history access.

")))
      (setq-local elgrep-w-re (widget-create 'editable-field
					     :prompt-history 'elgrep-re-hist
					     :keymap elgrep-menu-hist-map
					     :format "Regular Expression: %v" ""))
      (setq-local elgrep-w-dir (widget-create 'directory
                                              :prompt-history 'file-name-history
                                              :keymap elgrep-menu-hist-map
                                              :value-to-internal #'elgrep-wid-dir-to-internal
                                              :format "Directory: %v" default-directory))
      (setq-local elgrep-w-file-name-re (widget-create 'regexp
						       :prompt-history 'elgrep-file-name-re-hist
						       :keymap elgrep-menu-hist-map
						       :format "File Name Regular Expression: %v" (elgrep-default-filename-regexp default-directory)))
      (setq-local elgrep-w-exclude-file-re (widget-create 'regexp
                                                          :prompt-history 'regexp-history
                                                          :keymap elgrep-menu-hist-map
                                                          :format "Exclude File Name Regular Expression (ignored when empty): %v" ""))
      (setq-local elgrep-w-dir-re (widget-create 'regexp
                                                 :prompt-history 'regexp-history
                                                 :keymap elgrep-menu-hist-map
                                                 :format "Directory Name Regular Expression: %v" ""))
      (setq-local elgrep-w-exclude-dir-re (widget-create 'regexp
                                                         :prompt-history 'regexp-history
                                                         :keymap elgrep-menu-hist-map
                                                         :format "Exclude Directory Name Regular Expression (ignored when empty): %v" ""))
      (widget-insert  "Recurse into subdirectories ")
      (setq-local elgrep-w-recursive (widget-create 'checkbox nil))
      (setq-local elgrep-w-c-beg (widget-create 'number :format "\nContext Lines Before The Match: %v" 0))
      (setq-local elgrep-w-c-end (widget-create 'number :format "Context Lines After The Match: %v" 0))
      (setq-local elgrep-w-search-fun (widget-create 'function :format "Search function: %v " #'re-search-forward))
      (widget-insert "\n")
      (widget-create 'push-button (propertize "Start elgrep" 'keymap elgrep-menu-start-map 'mouse-face 'highlight 'help-echo "<down-mouse-1> or <return>: Start elgrep with the specified parameters"))
      (widget-insert " ")
      (widget-create 'push-button (propertize "Burry elgrep menu" 'keymap elgrep-menu-bury-map 'mouse-face 'highlight 'help-echo "<down-mouse-1> or <return>: Bury elgrep-menu."))
      (widget-insert " ")
      (widget-create 'push-button (propertize "Reset elgrep menu" 'keymap elgrep-menu-reset-map 'mouse-face 'highlight 'help-echo "<down-mouse-1> or <return>: Reset elgrep-menu.")))
    (use-local-map widget-keymap)
    (local-set-key "q" #'bury-buffer)
    (widget-setup)
    (buffer-enable-undo)))

(defun elgrep-get-formatter ()
   "Return a formatter for elgrep-lines.
The formatter is a function with two arguments FNAME and PARTS.
FNAME is the file name where the match occurs.
PARTS is a list of parts.
Each PART is a property list with members

:match (the actual match)

:context (the match including context lines)

:line (the line in the source code file)

:line-beg (the beginning position of the context in the source code file)

:beg (the beginning position of the match)

:end (the end position of the match)

The formatter is actually a capture
that remembers the last file name and the line number
such that the same line number is not output multiple times."
   (let ((last-file "")
	 (last-line 0)
	 (output-beg 0))
     (lambda (fname parts)
       (when (consp parts)
	 (let* ((part (car parts))
		(line (plist-get part :line)))
	   (unless (and (string-equal last-file fname)
			(= last-line line))
	     (insert (propertize (format "%s:%d:" fname line)
				 'elgrep-context-begin (plist-get part :context-beg)
				 'elgrep-context-end (plist-get part :context-end)
				 'elgrep-context (plist-get part :context)
				 ))
	     (setq output-beg (point))
	     (insert (plist-get part :context) ?\n))
	   (let ((context-beg (plist-get part :context-beg)))
	     (cl-loop for part in parts do
		      (let ((match-beg (+ (- (plist-get part :beg) context-beg) output-beg))
			    (match-end (+ (- (plist-get part :end) context-beg) output-beg)))
			(when (and (>= match-beg output-beg)
				   (<= match-end (point-max)))
			  (put-text-property match-beg match-end 'font-lock-face 'match)))))
	   (setq last-file fname
		 last-line line)
	   )))))

(defvar compilation-last-buffer) ; defined in "compile.el"

(defun elgrep-list-matches (filematches &rest options)
  "Insert FILEMATCHES as returned by `elgrep' in current buffer.
OPTIONS is a plist of options as for `elgrep'."
  (let ((opt-list (car-safe options)))
    (when (listp opt-list)
      (setq options opt-list)))
  (setq compilation-last-buffer (current-buffer))
  (unless (plist-get options :no-header)
    (insert (format "-*- mode: elgrep; default-directory: %S -*-\n" default-directory)))
  (let ((formatter (or (plist-get options :formatter)
		       (elgrep-get-formatter))))
    (dolist (filematch filematches)
      (let ((fname (car filematch))
	    stack)
	(dolist (match (cdr filematch))
	  (let ((part (car-safe match)))
	    (if (or (null stack)
		    (eq (plist-get (car stack) :line) (plist-get part :line)))
		(push part stack)
	      (funcall formatter fname stack)
	      (setq stack (list part)))
	    ))
	(when stack
	  (funcall formatter fname stack))
	))))

;;;###autoload
(defun elgrep (dir file-name-re re &rest options)
  "In path DIR grep files with name matching FILE-NAME-RE for text matching RE.
This is done via Emacs Lisp (no dependence on external grep).
Return list of filematches.

Each filematch is a cons (file . matchdata).
file is the file name.
matchdata is a list of matches.
Each match is a list of sub-matches.
Each submatch is a plist of :match, :context, :line,
:linestart, :beg and :end.

OPTIONS is a plist
Flags:

:abs absolute file names
t: full absolute file names;
nil: (default) file names relative to `default-directory'
of the last visited buffer

:interactive
t: call as interactive

:c-beg context begin (line beginning)
Lines before match defaults to 0. Can also be a regular expression.
Then this re is searched for in backward-direction
starting at the current elgrep-match.

:c-end context end (line end)
Lines behind match defaults to 0
Then this re is searched for in forward-direction
starting at the current elgrep-match.

:c-op
Context operation gets beginning and end position of context as arguments.
Defaults to `buffer-substring-no-properties'.

:recursive
t: also grep recursively subdirectories in dir
\(also if called interactively with prefix arg)

:formatter
Formatting function to call for each match
if called interactively with non-nil RE.
Inputs: format string \"%s:%d:%s\n\", file-name, line number,

:exclude-file-re
Regular expression matching the files that should not be grepped.

:dir-re
Regular expression matching the directories
that should be entered in recursive grep.
Defaults to \"\".

:exclude-dir-re
Regular expression matching the directories
that should not be entered in recursive grep.
If this is the empty string no directories are excluded.
Defaults to \"^\\.\".

:search-fun
Function to search forward for occurences of RE
with the same arguments as `re-search-forward'.
It is actually not required that REGEXP is a regular expression.
t just must be be understood by :search-fun.
Defaults to `re-search-forward'.

:keep-elgrep-buffer
Keep buffer <*elgrep*> even when there are no matches.

:no-header
Avoid descriptive header into <*elgrep*> buffer."
  (interactive (let ((dir (read-directory-name "Directory:")))
		 (append (list dir
			       (let ((default-file-name-regexp (elgrep-default-filename-regexp dir)))
				 (read-regexp (concat "File-name regexp (defaults:\"\" and \"" default-file-name-regexp "\"):") (list "" default-file-name-regexp) 'elgrep-file-name-re-hist)
				 )
			       (read-regexp "Emacs regexp:" nil 'elgrep-re-hist))
			 (when current-prefix-arg (list :recursive t)))))
  (when (and (stringp re) (= (length re) 0))
    (setq re nil))
  (unless dir
    (setq dir default-directory))
  (setq dir (expand-file-name (directory-file-name (substitute-in-file-name dir))))
  (with-current-buffer (get-buffer-create "*elgrep*")
    (buffer-disable-undo)
    (setq default-directory dir)
    (let ((files (directory-files dir (plist-get options :abs) file-name-re))
	  filematches
	  (inhibit-read-only t)
	  (c-op (or (plist-get options :c-op) 'buffer-substring-no-properties))
	  (exclude-file-re (plist-get options :exclude-file-re))
          (search-fun (or (plist-get options :search-fun) #'re-search-forward)))
      (when (and exclude-file-re (null (string-equal exclude-file-re "")))
	(setq files (cl-remove-if (lambda (fname) (string-match exclude-file-re fname)) files)))
      (cl-loop for file in files do
	    (when (file-regular-p file)
	      (if re
		  (progn
		    (erase-buffer)
		    (elgrep-insert-file-contents (if (plist-get options :abs) file (expand-file-name file dir)))
		    (let (filematch
			  (last-line-number 1)
			  (last-pos (point-min)))
		      (goto-char (point-min))
		      (while (funcall search-fun re nil t)
			(let* ((n (/ (length (match-data)) 2))
			       (matchdata (cl-loop for i from 0 below n
						   collect
						   (let ((context-beginning (save-excursion
									      (goto-char (match-beginning 0))
									      (elgrep-line-position (plist-get options :c-beg) line-beginning-position re-search-backward)))
							 (context-end (elgrep-line-position (plist-get options :c-end) line-end-position re-search-forward)))
						     (list :match (match-string-no-properties i)
							   :context (funcall c-op context-beginning context-end)
							   :line (prog1
								     (setq last-line-number
									   (+ last-line-number
									      (count-lines last-pos (line-beginning-position))))
								   (setq last-pos (line-beginning-position)))
							   :context-beg context-beginning
							   :context-end context-end
							   :beg (match-beginning i)
							   :end (match-end i))))))
			  (setq filematch (cons matchdata filematch))))
		      (when filematch
			(setq filematches (cons (cons file (nreverse filematch)) filematches)))))
		;; no re given; just register file with dummy matchdata
		(setq filematches (cons (list file) filematches)))))
      (setq filematches (nreverse filematches))
      (when (plist-get options :recursive)
	(setq files (cl-loop for file in (directory-files dir)
			     if (and (file-directory-p (expand-file-name file dir))
				     (let ((dir-re (plist-get options :dir-re))
					   (exclude-dir-re (plist-get options :exclude-dir-re)))
				       (and (or (null dir-re)
						(string-match dir-re file))
					    (null
					     (and exclude-dir-re
						  (null (string-equal exclude-dir-re ""))
						  (string-match exclude-dir-re file)))))
				     (null (string-match "^\\.[.]?\\'" file)))
			     collect file))
	(dolist (file files)
	  (setq filematches
		(append
		 (if (plist-get options :abs)
		     (apply 'elgrep (expand-file-name file dir) file-name-re re :keep-elgrep-buffer t options)
		   (let ((files (apply 'elgrep (expand-file-name file dir) file-name-re re :keep-elgrep-buffer t options)))
		     ;;(debug)
		     (cl-loop for f in files do
			      (setcar f (file-relative-name (expand-file-name (car f) file))))
		     files))
		 filematches))))
      (when (or (plist-get options :interactive) (called-interactively-p 'any))
	(cl-assert (string-equal (buffer-name) "*elgrep*") nil "Expected buffer <*elgrep*> got %s." (current-buffer))
	(if filematches
	    (progn
	      (unless (plist-get options :abs)
		(setq default-directory dir))
	      (delete-region (point-min) (point-max))
	      (if re
		  (progn
		    (elgrep-list-matches filematches options)
		    (elgrep-mode))
		(elgrep-dired-files (mapcar 'car filematches)))
	      (display-buffer (current-buffer)))
	  (unless (plist-get options :keep-elgrep-buffer)
	    (kill-buffer))
	  (message "elgrep: No matches for \"%s\" in files \"%s\" of dir \"%s\"." re file-name-re dir)))
      filematches)))

;;;###autoload
(easy-menu-add-item global-map '("menu-bar" "tools") ["Search Files (Elgrep)..." elgrep-menu t] "grep")

(defun elgrep-first-error-no-select (&optional n)
  "Restart at first error.
Visit corresponding source code.
With prefix arg N, visit the source code of the Nth error."
  (interactive "p")
  (let ((next-error-highlight next-error-highlight-no-select))
    (next-error n t))
  (pop-to-buffer next-error-last-buffer))

(defun elgrep-save (really-save)
  "Apply modifications in the current elgrep buffer to the client buffers.
Save modified client buffers if REALLY-SAVE is non-nil.
Interactively, REALLY-SAVE is set to the prefix arg."
  (interactive "p")
  (save-excursion
    (goto-char (point-max))
    (let ((last-pos (point))
	  files-to-save)
      (while (and
	      (null (bobp))
	      (condition-case nil
		  (progn (compilation-next-error -1) ;; barfs if the buffer does not contain any message at all
			 t)
		(error nil)))
	(let* ((edited-str (buffer-substring-no-properties (next-single-property-change (point) 'compilation-message) (1- last-pos)))
	       (msg (get-text-property (point) 'compilation-message))
	       (context-begin (get-text-property (point) 'elgrep-context-begin))
	       (context-end (get-text-property (point) 'elgrep-context-end))
	       (context (get-text-property (point) 'elgrep-context))
	       (loc (compilation--message->loc msg))
	       (file-struct (compilation--loc->file-struct loc))
	       (name (caar file-struct))
	       (dir (cadar file-struct))
	       (path (if dir (expand-file-name name (file-name-directory dir)) name)))
	  (setq last-pos (point))
	  (with-current-buffer (find-file-noselect path)
	    (goto-char context-begin)
	    (let* ((original-str (buffer-substring-no-properties  context-begin context-end)))
	      (when (and (string-equal context original-str) ;; It is still the old context...
			 (null (string-equal edited-str original-str))) ;; and this has been changed in *elgrep*.
		(kill-region context-begin context-end)
		(insert edited-str)
		(unless (member path files-to-save)
		  (push path files-to-save))
		)))))
      (when really-save
	(while files-to-save
	  (with-current-buffer (get-file-buffer (car files-to-save))
	    (save-buffer))
	  (setq files-to-save (cdr files-to-save))))
      files-to-save)))

(defvar elgrep-edit-mode-map (let ((map (copy-keymap global-map)))
                               (define-key map [remap save-buffer] #'elgrep-save)
			       (define-key map (kbd "C-c C-n") #'next-error-no-select)
			       (define-key map (kbd "C-c C-p") #'previous-error-no-select)
			       (define-key map (kbd "C-c C-f") #'elgrep-first-error-no-select)
                               map)
  "Keymap used in function `elgrep-edit-mode'.
Ovwerrides `compilation-mode-map'.")
(defvar-local elgrep-saved-major-mode nil)

(defun elgrep-enrich-text-property (refprop prop-list)
  "Enrich intervals with text property REFPROP through the list of text properties PROP-LIST."
  (let (interval) ;; This should not be necessary!
    (cl-loop for interval being the intervals property refprop
	     when (get-text-property (car interval) refprop)
	     do (add-text-properties (car interval) (cdr interval) prop-list))))

(defvar-local elgrep-edit-previous-header nil
  "Elgrep-edit-mode is a minor mode that can be switched on and off.
When it is switched off it should restore the old header line which is preserved here.")

(define-minor-mode elgrep-edit-mode
  "Mode for editing compilation buffers (especially elgrep buffers)."
  nil
  " e"
  nil
  (cl-assert (derived-mode-p 'elgrep-mode) nil "Major mode not derived from compilation mode.")
  (if elgrep-edit-mode
      (progn
	(unless elgrep-edit-previous-header ; Protect against re-entry of function `elgrep-edit-mode' with non-nil `elgrep-edit-mode'.
	  (setq elgrep-edit-previous-header header-line-format))
	(setq header-line-format (substitute-command-keys "Exit elgrep-edit-mode: \\[elgrep-edit-mode]; Save modifications: \\[elgrep-save]"))
        (setq buffer-read-only nil)
        (define-key (current-local-map) [remap self-insert-command] nil)
        (elgrep-enrich-text-property 'compilation-message '(read-only t intangible t))
        (when (eq buffer-undo-list t)
          (setq buffer-undo-list nil)
          (set-buffer-modified-p nil)))
    (setq header-line-format elgrep-edit-previous-header
	  elgrep-edit-previous-header nil)
    (define-key (current-local-map) [remap self-insert-command] 'undefined)))

(defalias 'elgrep-edit #'elgrep-edit-mode)

(provide 'elgrep)
;;; elgrep.el ends here
