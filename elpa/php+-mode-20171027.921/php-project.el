;;; php-project.el --- Create project name/directory relations.

;; Version: 2.0
;; Created: 10-1-2009
;; Copyright © 2009 Brian Zwahr
;; Author(s): 
;; Brian Zwahr <echosa@gmail.com>

;;; *****
;;; About
;;; *****

;; php-project.el is a part of the php+-mode suite and is used for setting 
;; directories as "projects" that allow various parts of the php+-mode suite
;; to work on php files with the knowlege of which projects they belong to. 
;; This allows emacs to automatically load proper tag files, run proper unit 
;; tests, create files (e.g. controllers, models, etc) in proper locations, etc.

;;; *****
;;; Usage
;;; *****

;; Use customize to set your project directories. The project needs the 
;; following information:
;; Project Nickname - This is just a nickname for the project. It will also
;;     be used as the name for the tags file.
;; Project Directory - This is the base directory for the project. For projects
;;     using the Zend Framework, this will typically be the directory where 
;;     application, library, etc. are.
;; PHPUnit Subdirectory - This is the subdirectory of the project directory
;;     where your phpunit config is. For example, if under the
;;     project directory you have a folder called tests where your phpunit xml
;;     config is located, enter "tests" here.
;; PHPUnit Config File Name - This is the name of the file that phpunit will 
;;     run when called to run on the entire project.
;; phpcs/phpmd Subdirectories - This is a list of subdirectories, separated by
;;     spaces, which should be included when phpcs is run for the project
;;     directory. For examples, if you want phpcs to run on the application and
;;     library folders of your project, enter "application library" here.

;; *************************************************************************

;; ************
;; REQUIREMENTS
;; ************
(require 'dired)

;; ******
;; CUSTOM
;; ******
(defgroup php-project nil
  "Customizations for php-project."
  :group 'php)

(defcustom php-project-list nil
  "Holds the relations of php projects in the form of ('name'
'directory' 'phpunit directory' 'phpunit config file' 'php test
subdirectories'). The name of the project is also used as the
name for the project's tag file."
  :group 'php-project
  :type '(alist :key-type (string :tag "Project Nickname") 
                :value-type 
                (list :tag "Properties"
                 (directory :tag "Project Directory") 
                 (file :tag "TAGS File")
                 (repeat :tag "Extra TAGS Directories"
                         (directory :tag "Directory"))
                 (file :tag "PHPUnit Config File" :must-match t)
                 (repeat :tag "PHPCS/PHPMD Directories"
                         (directory :tag "Directory"))
                 (list :tag "PHP Doc Overrides"
                       (cons :tag "Author"
                             (string :tag "Name") 
                             (string :tag "Email Address"))
                       (string :tag "Copyright")
                       (string :tag "License")
                       (string :tag "Version")
                       (string :tag "PHP Version")
                       (string :tag "Link")
                       (string :tag "Category")
                       (string :tag "Package")
                       (string :tag "Subpackage"))
                 (string :tag "Zend Framework Global Namespace")
                 (string :tag "Zend Framework Default Module"))))

; variable declarations for compiler
(defvar php-doc-default-author)
(defvar php-doc-default-copyright)
(defvar php-doc-default-license)
(defvar php-doc-default-version)
(defvar php-doc-default-php-version)
(defvar php-doc-default-link)
(defvar php-doc-default-category)
(defvar php-doc-default-package)
(defvar php-doc-default-subpackage)

;; *********
;; FUNCTIONS
;; *********
(defun php-project-show-directory ()
  "Shows the current project directory in the echo area."
  (interactive)
  (message (php-project-directory)))

(defun php-project-dired-directory (&optional project)
  "Opens the current project directory in dired."
  (interactive)
  (if project
      (dired (php-project-directory project))
    (dired (php-project-directory))))

(defun* php-project-ask-for-project (&optional (prompt "Project: " promptp))
  (assoc (completing-read prompt php-project-list nil t) php-project-list))

(defun php-project-open ()
  "Opens a project directory in dired."
  (interactive)
  (let ((nickname (car (php-project-ask-for-project "Project to open: "))))
    (php-project-dired-directory (assoc nickname php-project-list))))

(defun php-project-close ()
  "Closes all file buffers related to the project."
  (interactive)
  (let ((project (php-project-ask-for-project "Project to close: ")))
    (dolist (buffer (buffer-list))
      (when (eq (php-project-buffer-project (buffer-file-name buffer)) project)
        (kill-buffer buffer)))))

(defun php-project-open-phpunit-config ()
  "Opens the phpunit config file for the project."
  (interactive)
  (when (php-project-phpunit-config)
    (find-file (php-project-phpunit-config))))

(defun php-project-buffer-project (&optional file)
  "Returns the project list for the buffer file."
  (interactive)
  (let ((filename (if file file (buffer-file-name))))
    (when filename
      (catch 'found
        (dolist (project php-project-list)
          (let ((project-dir (expand-file-name (second project))))
            (when (string-match (concat "^" project-dir) filename)
              (throw 'found project))))))))

(defun php-project-add (nickname directory)
  "Adds a project directory listing to php-project."
  (interactive (list (read-string "Project nickname: ")
                     (read-directory-name "Project directory: ")))
  (let* ((tags-info (php-project-tags-file-setup directory))
         (tags-file (car tags-info))
         (tags-dirs (cdr tags-info))
         (phpunit-config (php-project-phpunit-info directory))
         (php-test-dirs (php-project-php-test-info directory))
         (php-doc-info (php-project-get-doc-info))
         (namespace (read-string "Global Namespace? "))
         (default-module (read-string "Default Module? "))
         (project
          (list nickname directory tags-file tags-dirs phpunit-config
                php-test-dirs php-doc-info namespace default-module)))
    (set-variable 'php-project-list (add-to-list 'php-project-list project))
    (customize-save-variable 'php-project-list php-project-list)
    (message (concat "Added " nickname " to projects list."))))

(defun php-project-remove ()
  "Removes a project directory listing from php-project."
  (interactive)
  (let ((nickname (car (php-project-ask-for-project "Project to remove: "))))
    (set-variable 'php-project-list (assq-delete-all nickname php-project-list))
    (customize-save-variable 'php-project-list php-project-list)
    (message (concat "Removed " nickname " from projects list."))))

(defun php-project-tags-file-setup (start-dir)
  "Get the tags file for adding a project."
  (let ((file "")
        dirs)
    (when (y-or-n-p "Add TAGS file? ")
      (setq file (read-file-name "TAGS file: " start-dir))
      (while (y-or-n-p "Add another directory for the TAGS file? ")
        (add-to-list 'dirs (read-directory-name "Directory: " start-dir))))
    (cons file dirs)))

(defun php-project-get-doc-info ()
  "Gets the project specific php-doc info."
  (if (y-or-n-p "Add project specific PHPDoc information? ")
      (let (info)
        (add-to-list 'info
                     (cons (read-string "Author Name? " 
                                        (car php-doc-default-author))
                           (read-string "Author Email? "
                                        (cdr php-doc-default-author))) t)
        (add-to-list 'info (read-string "Copyright: "
                                        php-doc-default-copyright) t)
        (add-to-list 'info (read-string "License: "
                                        php-doc-default-license) t)
        (add-to-list 'info (read-string "Version: "
                                        php-doc-default-version) t)
        (add-to-list 'info (read-string "PHP Version: "
                                        php-doc-default-php-version) t)
        (add-to-list 'info (read-string "Link: "
                                        php-doc-default-link) t)
        (add-to-list 'info (read-string "Category: "
                                        php-doc-default-category) t)
        (add-to-list 'info (read-string "Package: "
                                        php-doc-default-package) t)
        (add-to-list 'info (read-string "Subpackage: "
                                        php-doc-default-subpackage) t)
        info)
    '(("" . "") "" "" "" "" "" "" "" "")))

(defun php-project-phpunit-info (start-dir)
  "Get the phpunit info for adding a project."
  (let ((dir "")
        (config ""))
    (when (y-or-n-p "Add PHPUnit config file? ")
      (setq config (read-file-name "PHPUnit Config File: " start-dir)))
    config))

(defun php-project-php-test-info (start-dir)
  "Get directies for phpcs/phpmd."
  (let (dirs)
    (while (y-or-n-p "Add specific directory for phpcs/phpmd to test? ")
        (add-to-list 'dirs (read-directory-name "Directory: " start-dir)))
    dirs))

(defun php-project-nickname (&optional project)
  "Returns the nickname for the given project."
  (first (if project project (php-project-buffer-project))))

(defun php-project-directory (&optional project)
  "Returns the directory for the given project."
  (file-name-as-directory 
   (second 
    (if project
        project
      (let ((buffer-project (php-project-buffer-project)))
        (if buffer-project
            buffer-project
          (php-project-ask-for-project "Project to open: ")))))))

(defun php-project-get-project (project)
  "Returns the php-project-list value for the given project."
  (assoc project php-project-list))

(defun php-project-library-directory (&optional project)
  "Returns the library directory for the given project."
  (concat (php-project-directory) "library/"))

(defun php-project-tags-file (&optional project)
  "Returns the tags file for the given project."
  (third (if project project (php-project-buffer-project))))

(defun php-project-tags-directories (&optional project)
  "Returns the extra included directories for tags for the given
project."
  (fourth (if project project (php-project-buffer-project))))

(defun php-project-phpunit-config (&optional project)
  "Returns the phpunit config name for the given project."
  (fifth (if project project (php-project-buffer-project))))

(defun php-project-cs/md-directories (&optional project)
  "Returns the phpcs/phpmd directories for the given project."
  (sixth (if project project (php-project-buffer-project))))

(defun php-project-php-doc (&optional project)
  "Returns the PHP doc configuration for the given project"
  (seventh (if project project (php-project-buffer-project))))

(defun php-project-zend-global-namespace (&optional project)
  "Returns the global namespace the given project."
  (eighth (if project project (php-project-buffer-project))))

(defun php-project-zend-default-module-name (&optional project)
  "Returns the default module for the given project."
  (ninth (if project project (php-project-buffer-project))))

(defun php-project-vc-dir ()
  "Runs vc-dir on project directory."
  (interactive)
  (when (fboundp 'vc-dir)
    (let ((dir (expand-file-name (php-project-directory))))
      (when (file-exists-p dir)
        (vc-dir dir)))))

(defun php-project-query-replace-regexp (old new &optional file-match)
  "This function runs a query replace on the project files.
FILE-MATCH should be a regexp must match."
  (interactive `(,(read-string "Replace string: ")
                 ,(read-string "With: ")
                 ,(let ((file-match "\\(\\.php\\|\\.phtml\\|\\.inc\\)$"))
                    (if current-prefix-arg 
                        (read-string "File match regexp: " file-match)
                      file-match))))
  (unless file-match
    (setq file-match "\\(\\.php\\|\\.phtml\\|\\.inc\\)$"))
  (let ((b (current-buffer)))
    (find-grep-dired (php-project-directory) old)
    (while (not (looking-at-p "  find finished"))
      (goto-char (point-min))
      (beginning-of-line 0)
      (sit-for 1))
    (if (dired-mark-files-regexp file-match)
        (dired-do-query-replace-regexp old new)
      (switch-to-buffer b))))

(defun php-project-customize ()
  "This function opens the customize buffer for php-project."
  (interactive)
  (customize-group "php-project"))

(provide 'php-project)
