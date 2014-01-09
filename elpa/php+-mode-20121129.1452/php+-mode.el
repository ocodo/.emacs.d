;;; php+-mode.el --- A better PHP mode with Zend Framework 1 support.

;; Version: 2.1
;; Created: 8-25-2009
;; Copyright © 2009 Brian Zwahr
;; Author(s):
;; Michael Dwyer <mdwyer@ehtech.in>
;; Brian Zwahr <echosa@gmail.com>

;; *************************************************************************

;;; *****
;;; About
;;; *****

;; php+-mode is an emacs mode that makes PHP programming easier and faster.
;; Specific support for Zend Framework is also included.

;;; *****
;;; Usage
;;; *****

;; Installation:
;; Place the following in your .emacs setup:
;; (add-to-list 'load-path "/path/to/php+-mode/")
;; (require 'php+-mode)
;; (php+-mode-setup)

;; Keybindings:
;; The PHP+ menu can be used to call most functions.
;; Use C-h m from a php+-mode buffer to view the keybindings.

;; View the README for more details. It can be found at
;; https://github.com/echosa/php+-mode

;; *************************************************************************

;;; ************
;;; REQUIREMENTS
;;; ************
(require 'cl)
(require 'font-lock)
(require 'cc-mode)
(require 'cc-langs)
(require 'php-completion)
(require 'php-const)
(require 'php-doc)
(require 'php-edit)
(require 'php-font-lock)
(require 'php-format)
(require 'php-funcs)
(require 'php-help)
(require 'php-lineup)
(require 'php-parse)
(require 'php-project)
(require 'php-refactor)
(require 'php-string)
(require 'php-structure)
(require 'php-tags)
(require 'php-test)
(require 'php-utils)
(require 'string-utils)
(require 'php+-zf)

;;; *********
;;; CUSTOMIZE
;;; *********
(defgroup php+-mode nil
  "Customizations for php+-mode."
  :group 'languages)

(defcustom php-file-patterns '("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'")
 "List of file patterns for which to automatically invoke `php+-mode'."
 :type '(repeat (regexp :tag "Pattern"))
 :set (lambda (sym val)
        (set-default sym val)
        (let ((php-file-patterns-temp val))
          (while php-file-patterns-temp
            (add-to-list 'auto-mode-alist
                         (cons (car php-file-patterns-temp) 'php+-mode))
            (setq php-file-patterns-temp (cdr php-file-patterns-temp)))))
 :group 'php+-mode)

(defcustom php+-default-face 'default
  "Default face in `php+-mode' buffers."
  :type 'face
  :group 'php+-mode)

(defcustom php+-mode-php-compile-on-save nil
  "Whether or not to run php-compile on files when they are
saved."
  :type 'boolean
  :group 'php+-mode)

(defcustom php+-mode-js-compile-on-save nil
  "Whether or not to run js-compile on files when they are
saved."
  :type 'boolean
  :group 'php+-mode)

(defcustom php+-mode-css-compile-on-save nil
  "Whether or not to run css-compile on files when they are
saved."
  :type 'boolean
  :group 'php+-mode)

(defcustom php+-mode-show-trailing-whitespace nil
  "Whether or not to turn show trailing whitespace."
  :type 'boolean
  :group 'php+-mode)

(defcustom php+-mode-delete-trailing-whitespace nil
  "Whether or not to trailing whitespace from files. If non-nil, the all
trailing whitespace will be deleted from files prior to saving."
  :type 'boolean
  :group 'php+-mode)

(defcustom php+-mode-protected-underscore nil
  "Whether or not protected properties and methods begin with an
underscore."
  :type 'boolean
  :group 'php+-mode)

(defcustom php+-mode-show-project-in-modeline nil
  "Whether or not to show the buffer's project in the modeline."
  :type 'boolean
  :group 'php+-mode)

;;; *********
;;; Variables
;;; *********
(defvar php+-mode-map nil "Keymap for php+-mode.")
(defvar php-mode-abbrev-table (make-abbrev-table) "Initial abbrev table")

;;; *********
;;; FUNCTIONS
;;; *********

(defun php+-mode-delete-all-trailing-whitespace ()
  "This is just a wrapper around ``delete-trailing-whitespace''
that checks the value of php+-mode-delete-trailing-whitespace
first."
  (when php+-mode-delete-trailing-whitespace
    (delete-trailing-whitespace)))

;; *************
;;; php+-mode Setup
;;; *************
(defun php+-mode-compile-on-save ()
  (when php+-mode-php-compile-on-save
    (php-compile-if-php))
  (when php+-mode-js-compile-on-save
    (js-compile-if-js))
  (when php+-mode-css-compile-on-save
    (css-compile-if-css)))

(defun php+-mode-setup ()
  "Prepares emacs for php+-mode."
  (add-to-list 'load-path
	       (convert-standard-filename
		(concat (file-name-directory (locate-library "php+-mode.el"))
			"bundled/"))))

(defun php+-mode-customize ()
  "Opens the customize buffer for php+-mode."
  (interactive)
  (customize-group "php+-mode"))

(defun php+-mode-reload (&optional only-revert-source)
  "This function reverts all file buffers and reloads the php+-mode lisp files.
Optional argument `only-revert-source` tells the function to only
revert open php+-mode source code file buffers. "
  (interactive "P")
  (save-some-buffers)
  (when (fboundp 'ert-delete-all-tests) (ert-delete-all-tests))
  (dolist (subdir '("" "tests/"))
    (dolist (file (remove-if
                   (lambda (x) (string-match "/\\." x))
                   (file-expand-wildcards
                    (concat (file-name-directory 
                             (locate-library "php+-mode.el")) subdir "*.el"))))
      (load-file file)))
  (let ((rb (current-buffer)))
    (dolist (b (buffer-list))
      (when (and (buffer-file-name b)
                 (or (not only-revert-source)
                     (string-match
                      (file-name-directory (find-lisp-object-file-name 
                                            'php+-mode 'function))
                      (file-name-directory (buffer-file-name b)))))
        (switch-to-buffer b)
        (when (or (not only-revert-source) (eq major-mode 'emacs-lisp-mode))
          (revert-buffer t t))))
    (switch-to-buffer rb))
  (php+-define-keys)
  (php+-define-menu))

(defun php+-mode-source-line-count ()
  "This function returns the number of lines of code that make up
php+-mode, not including unittests or bundled packages."
  (interactive)
  (shell-command
   (concat "less " 
           (file-name-directory (find-lisp-object-file-name 'php+-mode 
                                                            'function))
           "*.el | grep -v '^[[:space:]]*;\\\|^[^[:space:]]*$' | wc -l"))
  (let ((b (current-buffer))
        count)
    (switch-to-buffer (get-buffer "*Shell Command Output*"))
    (setq count (replace-regexp-in-string
                 "[ \n]" ""
                 (buffer-substring-no-properties (point-min) (point-max))))
    (switch-to-buffer b)
    (message count)))

;;; ***************
;;; Keymap and Menu
;;; ***************
(defun php+-define-keys ()
  (define-key php+-mode-map "\C-cba" 'php-format-break-at-assignment-operators)
  (define-key php+-mode-map "\C-cbo" 'php-format-break-at-operators)
  (define-key php+-mode-map "\C-cbs" 'php-format-break-statement)
  (define-key php+-mode-map "\C-cb\C-s" 'php-format-break-string)
  (define-key php+-mode-map "\C-cb." 'php-format-break-at-concats)
  (define-key php+-mode-map "\C-cb," 'php-format-break-at-commas)
  (define-key php+-mode-map "\C-cb>" 'php-format-break-at-arrows)
  (define-key php+-mode-map "\C-cb\\" 'php-format-break-current)
  (define-key php+-mode-map "\C-cb\C-\\" 'php-format-break-class/interface)
  (define-key php+-mode-map "\C-cC" 'php-format-clean-up-script)
  (define-key php+-mode-map "\C-cd" 'php-change-string<->doc)
  (define-key php+-mode-map "\C-ch" 'php-change-bare-html<->heredoc)
  (define-key php+-mode-map "\C-ci" 'php-remove-this-concat)
  (define-key php+-mode-map "\C-c\M-i" 'php-implode-concats-in-statement)
  (define-key php+-mode-map "\C-cl" 'align-on)
  (define-key php+-mode-map "\C-cs" 'php-combine-scripts)
  (define-key php+-mode-map "\C-c'" 'php-change-string-quotes)
  (define-key php+-mode-map "\C-c\M-'" 'php-force-string-quotes-statement)
  (define-key php+-mode-map "\C-c(" 'php-find-current-sexp-begin)
  (define-key php+-mode-map "\C-c)" 'php-find-current-sexp-end)
  (define-key php+-mode-map "\C-c<" 'php-goto-start-of-script/html)
  (define-key php+-mode-map "\C-c>" 'php-goto-end-of-script/html)
  (define-key php+-mode-map [(control return)] 'php-format-break-statement)
  (define-key php+-mode-map "\C-cf" 'php-completion-lookup-at-point->message)
  (define-key php+-mode-map "\C-cza" 'zf-insert-action)
  (define-key php+-mode-map "\C-czb" 'zf-bootstrap)
  (define-key php+-mode-map "\C-czc" 'zf-controller)
  (define-key php+-mode-map "\C-czC" 'zf-insert-class)
  (define-key php+-mode-map "\C-czd" 'php-project-dired-directory)
  (define-key php+-mode-map "\C-czD" 'zf-create-directory-structure)
  (define-key php+-mode-map "\C-cze" 'zf-insert-method)
  (define-key php+-mode-map "\C-czE" 'php-modify-thing)
  (define-key php+-mode-map "\C-czf" 'zf-form)
  (define-key php+-mode-map "\C-czh" 'php-mark-current)
  (define-key php+-mode-map "\C-czi" 'zf-config)
  (define-key php+-mode-map "\C-czI" 'zf-interface)
  (define-key php+-mode-map "\C-czj" 'php-jump-to-thing)
  (define-key php+-mode-map "\C-czk" 'php-kill-current)
  (define-key php+-mode-map "\C-cz\M-k" 'php-kill-sexp-innard)
  (define-key php+-mode-map "\C-czl" 'zf-class)
  (define-key php+-mode-map "\C-czLc" 'zf-library-class)
  (define-key php+-mode-map "\C-czLi" 'zf-library-interface)
  (define-key php+-mode-map "\C-czm" 'zf-model)
  (define-key php+-mode-map "\C-czM" 'zf-create-module)
  (define-key php+-mode-map "\C-czn" 'zf-dbtable-model)
  (define-key php+-mode-map "\C-czN" 'zf-insert-interface)
  (define-key php+-mode-map "\C-czoi" 'zf-open-application-config)
  (define-key php+-mode-map "\C-czoo" 'php-project-open)
  (define-key php+-mode-map "\C-czou" 'php-project-open-phpunit-config)
  (define-key php+-mode-map "\C-czO" 'zf-insert-constant)
  (define-key php+-mode-map "\C-czpk" 'php-project-close)
  (define-key php+-mode-map "\C-czpd" 'php-project-show-directory)
  (define-key php+-mode-map "\C-czpvd" 'php-project-vc-dir)
  (define-key php+-mode-map "\C-czr" 'zf-insert-property)
  (define-key php+-mode-map "\C-czRm" 'php-refactor-move-thing-to-buffer)
  (define-key php+-mode-map "\C-czRM"
    'php-refactor-move-all-things-in-class/interface-to-buffer)
  (define-key php+-mode-map "\C-czRr" 'php-rearrange-current)
  (define-key php+-mode-map "\C-czRR" 'php-rearrange-innards)
  (define-key php+-mode-map "\C-cztt" 'php-compile)
  (define-key php+-mode-map "\C-cztT" 'php-test-full-project)
  (define-key php+-mode-map "\C-cztc" 'phpcs)
  (define-key php+-mode-map "\C-cztg" 'php-compile-again)
  (define-key php+-mode-map "\C-cztm" 'phpmd)
  (define-key php+-mode-map "\C-cztu" 'phpunit)
  (define-key php+-mode-map "\C-cztU" 'phpunit-single-test)
  (define-key php+-mode-map "\C-czv" 'zf-view-script-via-controller-action)
  (define-key php+-mode-map "\C-czV" 'zf-view-script)
  (define-key php+-mode-map "\C-czy" 'php-yank)
  (define-key php+-mode-map "\C-czz" 'zf-insert-dump)
  (define-key php+-mode-map "\C-czZ" 'zf-insert-dump-and-die)
  (define-key php+-mode-map "\C-cz;" 'php-comment-current)
  (define-key php+-mode-map [(control meta ?\>)] 'php-kill-chain-link)
  (define-key php+-mode-map [tab] 'php-tab-key)
  (define-key php+-mode-map [return] 'php-auto-fill)
  (define-key php+-mode-map "\M-." 'php-find-tag)
  (define-key php+-mode-map "\M-[" 'php-jump-to-previous-thing)
  (define-key php+-mode-map "\M-]" 'php-jump-to-next-thing)
  (define-key php+-mode-map "\M-;" 'php-comment-dwim)
  (define-key php+-mode-map [(super ?\[)] 'php-hide-innards)
  (define-key php+-mode-map [(super ?\{)] 'php-hide-class/interface-doc-blocks)
  (define-key php+-mode-map [(super ?\])] 'php-show-all)
  (define-key php+-mode-map [(super ?\})] 'php-show-class/interface-doc-blocks)
  (define-key php+-mode-map [(super ?\l)] 'php-goto-line)
  (define-key php+-mode-map "\C-c\C-f" 'php-search-documentation)
)

(defun php+-define-menu ()
  (define-key php+-mode-map
    [menu-bar php+]
    (cons "PHP+" (make-sparse-keymap "PHP+")))

  (define-key (lookup-key php+-mode-map [menu-bar php+])
    [create]
    (cons "Create/Open" (make-sparse-keymap "Create")))
  (define-key (lookup-key php+-mode-map [menu-bar php+ create])
    [module]
    '("Module" . zf-create-module))
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ create])
    [controller]
    '("Controller" . zf-controller)
    'module)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ create])
    [view-via]
    '("View Via Action" . zf-view-script-via-controller-action)
    'controller)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ create])
    [view]
    '("View" . zf-view-script)
    'view-via)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ create])
    [sep1]
    '("--single-line")
    'view)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ create])
    [model]
    '("Model" . zf-model)
    'sep1)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ create])
    [dbtable]
    '("Database Table Model" . zf-dbtable-model)
    'model)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ create])
    [form]
    '("Form" . zf-form)
    'dbtable)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ create])
    [sep2]
    '("--single-line")
    'form)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ create])
    [bootstrap]
    '("Bootstrap" . zf-bootstrap)
    'sep2)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ create])
    [class-file]
    '("Class" . zf-class)
    'bootstrap)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ create])
    [interface-file]
    '("Interface" . zf-interface)
    'class-file)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ create])
    [sep4]
    '("--single-line")
    'interface-file)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ create])
    [config]
    '("Config" . zf-config)
    'sep4)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ create])
    [open-config]
    '("Open application.ini" . zf-open-application-config)
    'config)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ create])
    [sep3]
    '("--single-line")
    'open-config)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ create])
    [function]
    '("Jump to Function" . php-jump-to-function)
    'sep3)

  (define-key-after (lookup-key php+-mode-map [menu-bar php+])
    [insert]
    (cons "Insert" (make-sparse-keymap "Insert"))
    'create)
  (define-key (lookup-key php+-mode-map [menu-bar php+ insert])
    [action]
    '("Action" . zf-insert-action))
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ insert])
    [method]
    '("Method" . zf-insert-method)
    'action)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ insert])
    [constant]
    '("Constant" . zf-insert-constant)
    'method)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ insert])
    [property]
    '("Property" . zf-insert-property)
    'constant)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ insert])
    [class]
    '("Class" . zf-insert-class)
    'property)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ insert])
    [interface]
    '("Interface" . zf-insert-interface)
    'class)

  (define-key-after (lookup-key php+-mode-map [menu-bar php+ insert])
    [library]
    `("Library" . ,(make-sparse-keymap "Library"))
    'interface)
  (define-key (lookup-key php+-mode-map [menu-bar php+ insert library])
    [library-class]
    '("Class" . zf-library-class))
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ insert library])
    [library-interface]
    '("Interface" . zf-library-interface)
    'library-class)

  (define-key-after (lookup-key php+-mode-map [menu-bar php+ insert])
    [dump]
    '("Dump" . zf-insert-dump)
    'library)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ insert])
    [dump-die]
    '("Dump and Die" . zf-insert-dump-and-die)
    'dump)

  (define-key-after (lookup-key php+-mode-map [menu-bar php+])
    [edit]
    (cons "Edit" (make-sparse-keymap "Edit"))
    'insert)

  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit])
    [refactor]
    `("Refactor" . ,(make-sparse-keymap "Refactor"))
    'edit)
  (define-key (lookup-key php+-mode-map [menu-bar php+ edit refactor])
    [move-thing]
    '("Move Thing to Buffer" . php-refactor-move-thing-to-buffer))
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit refactor])
    [move-all-things]
    '("Move All Things in Class/Interface to Buffer" .
      php-refactor-move-all-things-in-class/interface-to-buffer)
    'move-thing)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit refactor])
    [rearrange-current]
    '("Rearrange Current Thing" . php-rearrange-current)
    'move-all-things)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit refactor])
    [rearrange-innards]
    '("Rearrange Innards" . php-rearrange-innards)
    'rearrange-current)

  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit])
    [break]
    `("Break" . ,(make-sparse-keymap "Break"))
    'refactor)
  (define-key (lookup-key php+-mode-map [menu-bar php+ edit break])
    [break-current]
    '("Current Thing" . php-format-break-current))
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit break])
    [break-class/interface]
    '("Current Class/Interface" . php-format-break-class/interface)
    'break-current)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit break])
    [break-statement]
    '("Statement" . php-format-break-statement) 'break-class/interface)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit break])
    [break-string]
    '("String" . php-format-break-string) 'break-statement)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit break])
    [break-at-assignment-operators]
    '("At Assignment Operators" . php-format-break-at-assignment-operators)
    'break-string)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit break])
    [break-at-operators]
    '("At Operators" . php-format-break-at-operators)
    'break-at-assignment-operators)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit break])
    [break-at-concats]
    '("At Concats" . php-format-break-at-concats) 'break-at-operators)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit break])
    [break-at-commas]
    '("At Commas" . php-format-break-at-commas) 'break-at-concats)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit break])
    [break-at-arrows]
    '("At Arrows" . php-format-break-at-arrows) 'break-at-commas)

  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit])
    [mark-current]
    '("Mark Current Thing" . php-mark-current) 'break)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit])
    [kill-current]
    '("Kill Current Thing" . php-kill-current) 'mark-current)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit])
    [yank-killed]
    '("Yank Killed Thing" . php-yank) 'kill-current)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit])
    [kill-sexp-innard]
    '("Kill Current Sexp Innard" . php-kill-sexp-innard)
    'yank-killed)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit])
    [cleanup-script]
    '("Clean Up Script" . php-format-clean-up-script)
    'kill-sexp-innard)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit])
    [remove-this-concat]
    '("Remove Single Concatenation" . php-remove-this-concat)
    'cleanup-script)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit])
    [implode-concat]
    '("Implode This Concatenation" . php-implode-concat)
    'remove-this-concat)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit])
    [change-string-quotes]
    '("Toggle String Quoting" . php-change-string-quotes)
    'implode-concat)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit])
    [change-string<->doc]
    '("Toggle String/Doc" . php-change-string<->doc)
    'change-string-quotes)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit])
    [change-bare-html<->heredoc]
    '("Toggle HTML/Heredoc" . php-change-bare-html<->heredoc)
    'change-string<->doc)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit])
    [combine-scripts]
    '("Combine Consecutive Scripts" . php-combine-scripts)
    'change-short-tags)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit])
    [align-on]
    '("Align On" . align-on)
    'combine-scripts)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit])
    [modify-method]
    '("Modify Thing" . php-modify-thing)
    'align-on)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ edit])
    [modify-method-args]
    '("Modify Method Arguments" . php-modify-method-argument)
    'modify-method)

  (define-key-after (lookup-key php+-mode-map [menu-bar php+])
    [jump]
    '("Jump to Thing" . php-jump-to-thing)
    'edit)

  (define-key-after (lookup-key php+-mode-map [menu-bar php+])
    [sep1]
    '("--single-line")
    'edit)

  (define-key-after (lookup-key php+-mode-map [menu-bar php+])
    [project]
    (cons "Project" (make-sparse-keymap "Project"))
    'sep1)
  (define-key (lookup-key php+-mode-map [menu-bar php+ project])
    [show-dir]
    '("Show Project Directory" . php-project-show-directory))
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ project])
    [create-dir]
    '("Create Project Directory" . zf-create-directory-structure)
    'show-dir)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ project])
    [dired]
    '("Open Project Directory in Dired" . php-project-dired-directory)
    'create-dir)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ project])
    [project-close]
    '("Close Project" . php-project-close)
    'dired)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ project])
    [project-add]
    '("Add Project" . php-project-add)
    'project-close)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ project])
    [project-remove]
    '("Remove Project" . php-project-remove)
    'project-add)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ project])
    [php-vc-dir]
    '("Project Directory VC" . php-project-vc-dir)
    'project-remove)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ project])
    [customize]
    '("Customize Projects" . php-project-customize)
    'php-vc-dir)

  (define-key-after (lookup-key php+-mode-map [menu-bar php+])
    [tags]
    (cons "Tags" (make-sparse-keymap "Tags"))
    'project)
  (define-key (lookup-key php+-mode-map [menu-bar php+ tags])
    [create]
    '("Create Tags File" . php-create-tag-file))
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ tags])
    [create-with-dirs]
    '("Create Tags File With Extra Directories" . php-create-tag-file-with-dirs)
    'create)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ tags])
    [load]
    '("Load Tags File" . load-tags)
    'create-with-dirs)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ tags])
    [customize]
    '("Customize Tags" . php-tags-customize)
    'load)

  (define-key-after (lookup-key php+-mode-map [menu-bar php+])
    [test]
    (cons "Test" (make-sparse-keymap "Test"))
    'tags)
  (define-key (lookup-key php+-mode-map [menu-bar php+ test])
    [compile]
    '("Compile" . php-compile))
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ test])
    [test-full-project]
    '("Test Full Project" . php-test-full-project)
    'compile)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ test])
    [compile-again]
    '("Run Test Again" . php-compile-again)
    'test-full-project)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ test])
    [lint]
    '("PHP Lint" . php-lint)
    'compile-again)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ test])
    [lint-all]
    '("PHP Lint All" . php-lint-all)
    'lint)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ test])
    [phpcs]
    '("PHPCS" . phpcs)
    'lint-all)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ test])
    [phpcs-all]
    '("PHPCS All" . phpcs-all)
    'phpcs)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ test])
    [phpmd]
    '("PHPMD" . phpmd)
    'phpcs-all)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ test])
    [phpmd-all]
    '("PHPMD All" . phpmd-all)
    'phpmd)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ test])
    [phpunit]
    '("PHPUnit" . phpunit)
    'phpmd-all)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ test])
    [phpunit-all]
    '("PHPUnit All" . phpunit-all)
    'phpunit)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ test])
    [phpunit-single]
    '("PHPUnit Single Test" . phpunit-single-test)
    'phpunit-all)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ test])
    [phpunit-open]
    '("Open PHPUnit Config" . php-project-open-phpunit-config)
    'phpunit-single)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ test])
    [phpunit-logging]
    '("Toggle PHPUnit Logging" . phpunit-toggle-logging)
    'phpunit-open)
  (define-key-after (lookup-key php+-mode-map [menu-bar php+ test])
    [customize]
    '("Customize Testing" . php-test-customize)
    'phpunit-logging)

  (define-key-after (lookup-key php+-mode-map [menu-bar php+])
    [sep2]
    '("--single-line")
    'test)

  (define-key-after (lookup-key php+-mode-map [menu-bar php+])
    [customize]
    '("Customize Php+ Mode" . php+-mode-customize)
    'sep2)
  (define-key php+-mode-map [menu-bar php+ search-documentation]
    '("Search documentation" . php-search-documentation))
)

(defun php+-set-keymap-and-menu ()
  (setq php+-mode-map (make-sparse-keymap))
  (php+-define-keys)
  (php+-define-menu))

(unless php+-mode-map
  (php+-set-keymap-and-menu))

;;; *******
;;; php+-mode
;;; *******
(define-derived-mode php+-mode c-mode (concat
                                     "php+"
                                     (when (and
                                            php+-mode-show-project-in-modeline
                                            (php-project-nickname))
                                       (concat "[" (php-project-nickname) "]")))
  "Major mode for making developing Zend Framework PHP applications lazier.

\\{php+-mode-map}"
  (php+-mode-setup)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (c-add-language 'php+-mode 'c-mode)
  (set (make-local-variable 'c-basic-offset) 4)
  (set (make-local-variable 'c-opt-cpp-start) php-tags-key)
  (set (make-local-variable 'c-opt-cpp-prefix) php-tags-key)
  (c-set-offset 'cpp-macro 0)
  (set (make-local-variable 'c-block-stmt-1-key) php-block-stmt-1-key)
  (set (make-local-variable 'c-block-stmt-2-key) php-block-stmt-2-key)
  (set (make-local-variable 'c-doc-comment-style)
       '((php+-mode . javadoc)))

  (php-setup-font-locking)

  ;; Do not force newline at end of file.  Such newlines can cause
  ;; trouble if the PHP file is included in another file before calls
  ;; to header() or cookie().
  (set (make-local-variable 'require-final-newline) nil)
  (set (make-local-variable 'next-line-add-newlines) nil)

  (setq c-special-indent-hook nil)

  (turn-on-font-lock)
  (c-set-offset 'case-label '+)
  (c-set-offset 'cpp-macro 'php-cpp-macro-lineup)
  (c-set-offset 'arglist-intro 'php-arglist-intro-lineup)
  (c-set-offset 'arglist-close 'php-arglist-close-lineup)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'knr-argdecl 'php-knr-argdecl-lineup)
  (c-set-offset 'knr-argdecl-intro 'php-knr-argdecl-lineup)
  (c-set-offset 'topmost-intro 'php-topmost-intro-lineup)
  (c-set-offset 'topmost-intro-cont 'php-topmost-intro-cont-lineup)
  (c-set-offset 'c 'php-comment-lineup)
  (c-set-offset 'comment-intro 'php-comment-intro-lineup)
  (c-set-offset 'defun-close 'php-defun-close-lineup)
  (c-set-offset 'statement 'php-statement-lineup)
  (c-set-offset 'statement-cont 'php-statement-cont-lineup)
  (c-set-offset 'string 'php-string-lineup)
  (c-set-offset 'brace-list-intro 'php-brace-list-intro-lineup)
  (c-set-offset 'brace-list-entry 'php-brace-list-entry-lineup)
  (c-set-offset 'brace-list-close 'php-brace-list-close-lineup)
  (c-set-offset 'func-decl-cont 'php-func-decl-cont)

  (when (and php+-mode-show-trailing-whitespace
             (boundp 'show-trailing-whitespace))
    (setq show-trailing-whitespace t))
  (add-hook 'before-save-hook 'php+-mode-delete-all-trailing-whitespace nil t)
  (add-hook 'after-save-hook 'php+-mode-compile-on-save nil t)
  (load-tags)
  (php-text-struct-cache-initialize)
  (run-hooks 'php+-mode-hook))

(defcustom php+-mode-hook nil
  "List of functions to be executed on entry to `php+-mode'."
  :type 'hook
  :group 'php+-mode)

(provide 'php+-mode)

;;; php+-mode.el ends here
