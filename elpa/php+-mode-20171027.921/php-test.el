;;; php-test.el --- Tests php files with various utilities.

;; Version: 2.0
;; Created: 8-25-2009
;; Copyright © 2009 Brian Zwahr
;; Author(s): 
;; Brian Zwahr <echosa@gmail.com>

;;; *****
;;; About
;;; *****

;; php-test.el is a part of the php+-mode suite and allows phpunit and phpcs
;; to be run directly from emacs on either a single file or the entire project.
;; Also, this sets up php files to be run though php lint upon saving. 
;; This uses setting set in php-project's customize.

;;; *****
;;; Usage
;;; *****

;; Requirements:
;; php-project
;; Also requires that phpunit and phpcs be installed before those functions
;; can be used.

;; Use customize to set the executables for php, phpunit, and phpcs 
;; and the file extensions that should be linted with php.
;; php-lint should be run automatically when an appropriate file is saved,
;; but can also be run manually.
;; php lint, phpunit, and phpcs can all be run from the php+-mode menu or the
;; available keyboard commands.
;; phpunit and phpcs can be run on either a single file (the current buffer)
;; or on the entire project. Concerning the latter, as far as phpcs is concerned
;; this means running all directories listed in the project setup as phpcs
;; subdirectorites through phpcs. For phpunit, this means running phpunit with
;; the phpunit config file set in the project properties.

;; *************************************************************************

;;; ************
;;; Requirements
;;; ************
(require 'files)
(require 'php-parse)
(require 'php-project)

;; ******
;; CUSTOM
;; ******
(defgroup php-test nil
  "Customizations for testing PHP."
  :group 'php)

(defcustom phpunit-shell-command "phpunit"
  "Shell command to execute phpunit."
  :group 'php-test
  :type 'string)

(defcustom phpcs-shell-command "phpcs"
  "Shell command to execute phpcs."
  :group 'php-test
  :type 'string)

(defcustom jslint-shell-command "jslint"
  "Shell command to execute jslint."
  :group 'php-test
  :type 'string)

(defcustom phpcs-standard "PEAR"
  "Standard to place after the --standard phpcs option. If nil, the default is 
used."
  :group 'php-test
  :type 'string)

(defcustom phpmd-shell-command "phpmd"
  "Shell command to execute phpmd."
  :group 'php-test
  :type 'string)

(defcustom phpmd-format 'text
  "Output format for phpmd."
  :group 'php-test
  :type '(radio (const :tag "Text" text)
                (const :tag "XML" xml)
                (const :tag "HTML" html)))

(defcustom phpmd-rulesets '(codesize design naming unusedcode)
  "Output format for phpmd."
  :group 'php-test
  :type '(set (const codesize)
              (const design)
              (const naming)
              (const unusedcode)))

(defcustom php-test-file-extensions (list "php" "inc")
  "File extensions that should be run through php lint."
  :type '(repeat :tag "File extension" (string))
  :group 'php-test)

(defcustom js-file-extensions '("js")
  "File extensions that are JavaScript."
  :type '(repeat :tag "File extension" (string))
  :group 'js)

(defcustom css-file-extensions '("css")
  "File extensions that are CSS."
  :type '(repeat :tag "File extension" (string))
  :group 'css)

(defcustom php-executable "php"
  "Path of the php executable. Used for running php lint"
  :type 'string
  :group 'php-test)

(defcustom php-test-ask-save t
  "Ask to save unsaved buffers before compiling."
  :type 'boolean
  :group 'php-test)

(defcustom php-test-show-command nil
  "Whether or not to show the compile command in the compilation buffer."
  :type 'boolean
  :group 'php-test)

(defcustom php-test-compile-tests '(lint phpcs phpmd)
  "Which tests to include during php-compile."
  :group 'php-test
  :type '(set (const lint)
              (const phpcs)
              (const phpmd)))

;; *********
;; VARIABLES
;; *********
(defvar php-test-last-user-cmd nil
  "Last test command run specifically by the user.")

;; *********
;; FUNCTIONS
;; *********
(defun* php-compile-cmd (whole-project &optional all-groups
                                       &key lint phpcs phpmd phpunit
                                       phpunit-single)
  "Creates the compile command string."
  (concat "EXIT_STATUS=0; "
          (when lint (php-lint-cmd whole-project))
          (when phpcs (phpcs-cmd whole-project))
          (when phpmd (phpmd-cmd whole-project))
          (when phpunit 
            (if whole-project
                (if all-groups
                    (phpunit-cmd t nil t)
                  (phpunit-cmd t))
              (phpunit-cmd)))
          (when phpunit-single (phpunit-cmd nil t))
          (php-compile-print-divider-command)
          "exit $EXIT_STATUS;"))

(defun* php-compile-run-cmd (cmd &optional error-alist)
  "Runs the given compile command."
  (let ((compilation-error-regexp-alist
         (or error-alist
             `(("\\(.*\\) in \\(.*\\) on line \\([0-9]+\\)$" 2 3 nil nil 3)
               ("^No syntax errors detected in \\(.*\\)" 1 nil nil nil 1
                (1 compilation-info-face t))
               ("^ \\([0-9]+\\)" nil 1 nil nil 1)
               ("^\\(/.*\\):\\([0-9]+\\)" 1 2 nil nil 2))))
        (current-buffer (buffer-name))
        unsaved-buffers)
    (unless php-test-ask-save
      (dolist (buffer (buffer-list))
        (when (and (buffer-file-name buffer) (buffer-modified-p buffer))
          (add-to-list 'unsaved-buffers buffer)
          (set-buffer buffer)
          (set-buffer-modified-p nil)))
      (pop-to-buffer current-buffer nil t))
    (compile cmd)
    (unless php-test-show-command
      (php-test-hide-command))
    (unless php-test-ask-save
      (dolist (buffer unsaved-buffers)
        (set-buffer buffer)
        (set-buffer-modified-p t)))
    (pop-to-buffer current-buffer nil t)))

(defun* php-compile-run (whole-project &optional all-groups
                                       &key lint phpcs phpmd phpunit
                                       user-called)
  "Calls the php-compile-run-cmd function with the given
arguments. The php-compile-run-cmd is separate for things like
phpunit-single."
  (let ((cmd (php-compile-cmd whole-project all-groups :lint lint
                              :phpcs phpcs :phpmd phpmd :phpunit phpunit)))
    (when user-called
      (setq php-test-last-user-cmd cmd))
    (php-compile-run-cmd cmd)))

(defun php-compile ()
  (interactive)
  (php-compile-run nil nil
                   :lint (member 'lint php-test-compile-tests)
                   :phpcs (member 'phpcs php-test-compile-tests)
                   :phpmd (member 'phpmd php-test-compile-tests)))

(defun php-compile-if-php ()
  "Checks to make sure buffer is a php file, and if so, runs php-compile."
  (when (and (not (file-remote-p buffer-file-name))
             (member (file-name-extension buffer-file-name) 
                     php-test-file-extensions))
    (php-compile)))

(defun php-compile-again ()
  (interactive)
  (when php-test-last-user-cmd
    (php-compile-run-cmd php-test-last-user-cmd)))    

(defun php-test-full-project ()
  (interactive)
  (php-compile-run t t :lint t :phpcs t :phpmd t :phpunit t :user-called t))
    
;; Lint
(defun php-lint-cmd (&optional all-p)
  (when (and php-executable (not (equal "" php-executable)))
    (let ((cmd (if all-p
                   (concat "for i in `find " (php-project-directory) 
                           " -type f -name \"*.php\" -or -name \"*.phtml\"`; "
                           "do j=`php -l \"$i\"`; done;")
                 (concat php-executable " -l -f \"" (buffer-file-name) 
                         "\"; "))))
      (php-test-cmd cmd :label "PHP Lint Results:"))))

(defun php-lint ()
  "Performs a PHP lint check on the current file."
  (interactive)
  (php-compile-run nil nil :lint t))

(defun php-lint-all ()
  (interactive)
  (php-compile-run t nil :lint t))

(defun php-lint-if-php ()
  "Checks to make sure buffer is a php file, and if so, runs php-lint."
  (when (member (file-name-extension buffer-file-name) php-test-file-extensions)
    (php-lint)))

;; PHPCS
(defun* phpcs-cmd (&optional all-p &key extensions)
  (when (and phpcs-shell-command (not (equal "" phpcs-shell-command)))
    (php-test-cmd 
     (concat phpcs-shell-command " "
             (when phpcs-standard (concat "--standard=" phpcs-standard " "))
             (if extensions
                 (concat "--extensions=" extensions " ")
               (when php-test-file-extensions 
                 (concat "--extensions="
                         (mapconcat 'concat php-test-file-extensions ",") " ")))
             (if all-p
                 (if (php-project-cs/md-directories)
                     (mapconcat 'identity (php-project-cs/md-directories) " ")
                   (php-project-directory))
               (shell-quote-argument (buffer-file-name)))
             "; ")
     :label "PHPCS Results:"
     :goto-dir (when all-p (php-project-directory)))))

(defun phpcs (&optional all-p interactive)
  "Run phpcs for either the current buffer or, if run with an argument, the
directory of the current buffer's project."
  (interactive "P")
  (let ((user-called (or (called-interactively-p 'interactive) interactive)))
    (php-compile-run all-p nil :phpcs t :user-called user-called)))

(defun phpcs-all ()
  "This is just a convenience function that runs the phpcs function with an 
argument."
  (interactive)
  (phpcs t (called-interactively-p 'interactive)))

(defun phpcs-if-php ()
  "Checks to make sure buffer is a php file, and if so, runs phpcs."
  (when (member (file-name-extension buffer-file-name) php-test-file-extensions)
    (phpcs nil)))

;; PHPMD
(defun phpmd-cmd (&optional all-p)
  (when (and phpmd-shell-command (not (equal "" phpmd-shell-command)))
    (let (targets cmd)
      (if all-p
          (if (php-project-cs/md-directories)
              (setq targets (php-project-cs/md-directories))
            (add-to-list 'targets (php-project-directory)))
        (add-to-list 'targets (buffer-file-name)))
      (dolist (target targets)
        (setq cmd 
              (concat cmd
               phpmd-shell-command " " target " " (symbol-name phpmd-format) " "
               (let (str)
                 (dolist (rule phpmd-rulesets)
                   (setq str (concat str (symbol-name rule) ",")))
                 (substring str 0 (1- (length str))))
               " "
               (when php-test-file-extensions 
                 (concat 
                  "--suffixes "
                  (mapconcat 'concat php-test-file-extensions ",")))
               "; ")))
      (php-test-cmd cmd
                    :label "PHPMD Results:"
                    :goto-dir (when all-p (php-project-directory))))))

(defun phpmd (&optional all-p interactive)
  "Run phpmd for either the current buffer or, if run with an argument, the
directory of the current buffer's project."
  (interactive "P")
  (let ((user-called (or (called-interactively-p 'interactive) interactive)))
    (php-compile-run all-p nil :phpmd t :user-called user-called)))

(defun phpmd-all (&optional interactive)
  "This is just a convenience function that runs the phpmd function with an 
argument."
  (interactive)
  (let ((user-called (or (called-interactively-p 'interactive) interactive)))
    (phpmd t (called-interactively-p 'interactive))))

(defun phpmd-if-php ()
  "Checks to make sure buffer is a php file, and if so, runs phpmd."
  (when (member (file-name-extension buffer-file-name) php-test-file-extensions)
    (phpmd nil)))

;; PHPUNIT
(defun phpunit-cmd (&optional all-p single-p all-groups)
  (when (and phpunit-shell-command 
             (not (equal "" phpunit-shell-command))
             (php-project-phpunit-config))
    (php-test-cmd
     (concat
      phpunit-shell-command
      " --configuration " (php-project-phpunit-config)
      (if all-p 
          (unless all-groups
            (let ((include-groups 
                   (read-string 
                    (concat "PHPUnit groups to include "
                            "(comma separated, no spaces)? "))))
              (if (equal "" include-groups)
                  (let ((exclude-groups
                         (read-string
                          (concat "PHPUnit groups to exclude "
                                  "(comma separated, no spaces)? "))))
                    (unless (equal "" exclude-groups)
                      (concat " --exclude-group " exclude-groups)))
                (concat " --group " include-groups))))
        (concat 
         " --filter "
         (when single-p 
           (concat (rest (assoc 'name (php-parse-current 'method))) " "))
         (rest (assoc 'name (first (rest (assoc 'classes 
                                                (php-parse-current 'script))))))
         " "
         (convert-standard-filename (buffer-file-name))))
      "; ")
     :label "PHPUnit Results:")))

(defun phpunit (&optional all-p interactive)
  "Run phpunit for either the current buffer or, if run with an argument, the 
test configuration file associated with the current buffer's project."
  (interactive "P")
  (let ((user-called (or (called-interactively-p 'interactive) interactive)))
    (php-compile-run all-p nil :phpunit t :user-called user-called)))

(defun phpunit-all ()
  "This is just a convenience function that runs the phpunit function with an 
argument."
  (interactive)
  (phpunit t (called-interactively-p 'interactive)))

(defun phpunit-single-test ()
  "This is a convenience function that runs phpunit only for the method that 
point is currently in."
  (interactive)
  (let ((cmd (php-compile-cmd nil nil :phpunit-single t)))
     (when (called-interactively-p 'interactive)
      (setq php-test-last-user-cmd cmd))
    (php-compile-run-cmd cmd)))

(defun phpunit-toggle-logging ()
  "Either comments or uncomments the logging section of the phpunit config."
  (interactive)
  (let ((buffer (current-buffer))
        function
        beg
        end)
    (php-project-open-phpunit-config)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "<logging>" nil t)
        (beginning-of-line)
        (setq beg (point))
        (re-search-forward "</logging>")
        (end-of-line)
        (setq end (point))
        (comment-or-uncomment-region beg end)
        (save-buffer)
        (goto-char beg)
        (forward-line -1)
        (if (comment-search-forward end t)
            (message "PHPUnit logging disabled.")
          (message "PHPUnit logging enabled."))))
    (switch-to-buffer buffer)))

;; Javascript
(defun js-test-cmd ()
  (when (and jslint-shell-command (not (equal "" jslint-shell-command)))
    (php-test-cmd
     (concat jslint-shell-command " " (shell-quote-argument (buffer-file-name)) 
             "; " 
             (phpcs-cmd nil :extensions "js")
             (php-compile-print-divider-command))
     :label "JS Lint Results:"
)))

(defun js-compile-if-js ()
  "Checks to make sure buffer is a js file, and if so, runs js-compile."
  (when (member (file-name-extension buffer-file-name) js-file-extensions)
    (js-compile)))

(defun js-compile (&optional path)
  (interactive)
  (let ((compilation-error-regexp-alist 
          '(("^/.* \\(.*\\)" 1) 
            ("^Error at line \\([0-9]+\\) character \\([0-9]+\\)" nil 1 2)
            ("^FILE: \\(.*\\)$" 1 nil nil nil 1)
            ("^ \\([0-9]+\\)" nil 1 nil nil 1))))
    (php-compile-run-cmd (js-test-cmd) compilation-error-regexp-alist)))

;; CSS
(defun css-test-cmd ()
  (php-test-cmd 
   (concat 
    (phpcs-cmd nil :extensions "css")
    (php-compile-print-divider-command))))

(defun css-compile (&optional path)
  (interactive)
  (let ((compilation-error-regexp-alist 
         '(("^FILE: \\(.*\\)$" 1 nil nil nil 1)
           ("^ \\([0-9]+\\)" nil 1 nil nil 1))))
    (php-compile-run-cmd (css-test-cmd) compilation-error-regexp-alist)))

(defun css-compile-if-css ()
  "Checks to make sure buffer is a css file, and if so, runs css-compile."
  (when (member (file-name-extension buffer-file-name) css-file-extensions)
    (css-compile)))

;; Utility Functions
(defun* php-test-cmd (cmd &key goto-dir label)
  (concat
   (when label
     (php-compile-print-divider-command label))
   (when goto-dir (concat "cd " (convert-standard-filename goto-dir) "; "))
   cmd
   "TEMP=$?; if [[ $EXIT_STATUS == 0 ]]; then EXIT_STATUS=$TEMP; fi; "
   (when goto-dir 
     (concat "cd " 
             (convert-standard-filename 
              (file-name-directory (buffer-file-name))) "; "))))

(defun php-compile-print-divider-command (&optional msg)
  "Creates a series of echo commands that will print a divider across a window"
  (concat "echo; echo '" 
          ;; Setting the width statically because it pulls the width of the
          ;; source buffer, not the compilation buffer
          (make-string 
           79 ;;(1- (window-width)) 
           ?\=)
          "'; "
          (when msg (concat "echo '" msg "'; "))))

(defun php-test-hide-command ()
  (let ((current-buffer (buffer-name))
        start)
    (pop-to-buffer "*compilation*" nil t)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (when (re-search-forward "EXIT_STATUS")
        (beginning-of-line)
        (delete-region (point) (line-end-position 2))))
    (pop-to-buffer current-buffer)))

(defun php-test-customize ()
  "This function opens the customize buffer for php-test."
  (interactive)
  (customize-group "php-test"))

(provide 'php-test)
