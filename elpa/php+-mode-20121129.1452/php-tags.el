;;; php-tags.el --- Create and load tag files.

;; Version: 3.0
;; Created: 6-15-2009
;; Copyright © 2009 Brian Zwahr
;; Author(s): 
;; Brian Zwahr <echosa@gmail.com>

;;; *****
;;; About
;;; *****

;; php-tags.el is a part of the php+-mode suite and is used for helping make
;; tags for php files and projects easier. It uses projects setup with
;; php-project.

;;; *****
;;; Usage
;;; *****

;; Requirements:
;; php-project
;; php-funcs

;; Use customize to set the tag-shell-command to your ctags
;; executable, the directory where you would like your ctags stored,
;; and the arguments to be used by ctags.
;;
;; Use php-create-tag-file (available in the php+-mode menu) to create a
;; tag file for the current project.  When using php+-mode, tags are
;; automatically loaded when switching to a buffer of a php file
;; associated with a project.

;; *************************************************************************

;;; ************
;;; Requirements
;;; ************
(require 'etags)
(require 'php-project)
(require 'php-funcs)

;; ******
;; CUSTOM
;; ******
(defgroup php-tags nil
  "Customizations for php-tags."
  :group 'php)

(defcustom tag-shell-command "ctags"
  "Shell command to execute for creating tags."
  :group 'php-tags
  :type 'string)

(defcustom php-tags-recursive t
  "Whether the tagging should be recursive into the directory."
  :group 'php-tags
  :type 'boolean)

(defcustom php-tags-relative t
  "Whether the tags should be relative."
  :group 'php-tags
  :type 'boolean)

(defcustom php-tags-totals nil
  "Whether the tagging should include totals."
  :group 'php-tags
  :type 'boolean)

(defcustom php-tag-file-extensions (list ".php")
  "File extensions to include when tagging."
  :group 'php-tags
  :type '(repeat :tag "File extension" (string)))

(defcustom php-tag-ignore-patterns (list "\.svn")
  "File of directory patterns to ignore when tagging."
  :group 'php-tags
  :type '(repeat :tag "File extension" (string)))

(defcustom php-tag-arguments (list 
                              "--PHP-kinds=cfid"
                              "--regex-PHP=\"/(abstract |final )class ([^ ]*)/\\1/c/\""
                              "--regex-PHP=\"/(public |static |final |abstract |protected |private )+function ([^ (]*)/\\2/f/\"")
  "A list of arguments for the tag executable."
  :group 'php-tags
  :type '(repeat :tag "Argument" (string)))

;; *********
;; FUNCTIONS
;; *********
(defun load-tags ()
  "Loads the proper tag file for the current buffer."
  (interactive)
  (when (php-project-buffer-project)
    (let ((filename (php-project-tags-file)))
      (when (and filename 
                 (not (equal filename ""))
                 (not (equal tags-file-name filename))
                 (file-exists-p filename))
        (setq tags-file-name nil)
        (setq tags-table-list nil)
        (visit-tags-table filename)))))

(defun create-tag-file (directory &optional add-dirs tags-file extra-dirs args)
  "Creates a tag file named 'name' from the files in 'directory'. The tag file 
is saved in 'php-tags-directory', which can be set in customize."
  (interactive
   (list (read-directory-name "Root directory: "
                              (second (split-string (pwd) " ")))))
  (let ((file
         (expand-file-name 
          (or tags-file
              (read-file-name "Tag file: " directory "TAGS" 'confirm))))
        (other-dirs (list))
        cmd)
    (when add-dirs
      (add-to-list
       'other-dirs
       (read-directory-name "Extra directory to be parsed for the tags file: "))
      (while (y-or-n-p "Add another directory to be parsed for the tags file? ")
        (add-to-list
         'other-dirs
         (read-directory-name
          "Extra directory to be parsed for the tags file: "))))
    (setq cmd (concat "cd " (convert-standard-filename directory) 
                      " && " (convert-standard-filename tag-shell-command)
                      " -e -f " (convert-standard-filename file)
                      " " args " . "
                      (when extra-dirs (mapconcat 'identity extra-dirs " "))
                      " "
                      (mapconcat 'identity other-dirs " ")))
    (message cmd)
    (make-directory (convert-standard-filename (file-name-directory file)) t)
    (message "Creating tags file: %s" file)
    (if (eq 0 (shell-command cmd))
        (progn
          (message "Created tags file: %s" file)
          (when (y-or-n-p 
                 (concat "Would you like to load the newly created tags file "
                         file " ? "))
            (setq tags-completion-table nil)
            (let ((tags-buffer (or (file-name-nondirectory file) "TAGS")))
              (when (get-buffer tags-buffer)
                (kill-buffer tags-buffer)))
            (visit-tags-table file)))
      (message "Tags creation failed. Check *Shell Command Output* buffer."))))

(defun php-create-tag-file (add-dirs)
  (interactive "P")
  (let ((args (concat (when php-tags-recursive " -R")
                      " --tag-relative=" (if php-tags-relative "yes" "no")
                      " --totals=" (if php-tags-totals "yes" "no")
                      " --langmap=php:"
                      (mapconcat 'identity php-tag-file-extensions "")
                      (mapconcat
                       (lambda (str)
                         (concat " --exclude=" (shell-quote-argument str)))
                       php-tag-ignore-patterns "")
                      " "
                      (mapconcat 'identity php-tag-arguments " "))))
    (create-tag-file 
     (php-project-directory)
     add-dirs
     (php-project-tags-file)
     (php-project-tags-directories)
     args)))

(defun php-create-tag-file-with-dirs ()
  "This function runs (php-create-tag-file) with a t argument to prompt for 
directories to add."
  (interactive)
  (php-create-tag-file t))

(defun php-find-tag (tagname)
  "Finds the tag name in the proper TAGS file."
  (interactive (find-tag-interactive "Find tag: "))
  (load-tags)
  (find-tag tagname))

(defun php-tags-customize ()
  "This functions opens the customize buffer for php-tags."
  (interactive)
  (customize-group "php-tags"))

(provide 'php-tags)
