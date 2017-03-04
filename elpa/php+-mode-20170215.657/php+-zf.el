:;;; php+-zf.el --- Zend Framework support for PHP+

;; Version: 3.0
;; Created: 8-25-2009
;; Copyright © 2009 Brian Zwahr
;; Author(s):
;; Michael Dwyer <mdwyer@ehtech.in>
;; Brian Zwahr <echosa@gmail.com>

;;; *********
;;; Custom
;;; *********
(defcustom zf-use-hyphens-in-viewscript-urls nil
  "Whether or not to use hyphens when generating viewscript urls
from controller names."
  :type 'boolean
  :group 'php+-mode)

;;; *********
;;; Constants
;;; *********
(defconst *zf-directory-structure* (list "application"
                                         "application/configs"
                                         "application/controllers"
                                         "application/forms"
                                         "application/layouts"
                                         "application/models"
                                         "application/models/DbTable"
                                         "application/modules"
                                         "application/resources"
                                         "application/views"
                                         "application/views/helpers"
                                         "application/views/scripts"
                                         "docs"
                                         "library"
                                         "public"
                                         "public/css"
                                         "public/js"
                                         "scripts"
                                         "tests")
  "A standard empty Zend Framework project directory structure.")

(defconst *zf-module-directory-structure* (list "configs"
                                                "controllers"
                                                "forms"
                                                "layouts"
                                                "models"
                                                "models/DbTable"
                                                "views"
                                                "views/helpers"
                                                "views/scripts")
  "A standard empty Zend Framework module directory structure.")

;;; **********************
;;; Zend Framework Project
;;; **********************
(defun zf-create-directory-structure (dir)
  "Creates a Zend Framework directory structure based on
*zf-directory-structure* in the current project directory. This
is used for starting a new project."
  (interactive "DRoot directory? ")
  (when (yes-or-no-p (concat "Are you sure you want to create a "
                             "Zend Framework directory structure in " dir "? "))
    (if (not (file-accessible-directory-p dir))
        (message "Cannot access the directory!")
      (dolist (directory *zf-directory-structure*)
        (make-directory (concat dir directory)))
      (message (concat "Project directory structure created in " dir))
      (when (yes-or-no-p "Add new directory to projects list? ")
        (php-project-add (read-string "Nickname? ") dir)))))

(defun zf-create-module (name)
  "Creates a module in the current project."
  (interactive "sModule name: ")
  (when (php-project-directory)
    (zf-create-module-structure
     (convert-standard-filename 
      (concat (php-project-directory) "application/modules/" name "/")))))

(defun zf-create-module-structure (dir)
  "Creates a Zend Framework module directory structure based on
*zf-module-directory-structure* in the current project directory.

This function is called by zf-create-module."
  (if (file-accessible-directory-p dir)
      (message "Module directory already exists!")
    (make-directory dir)
    (dolist (directory *zf-module-directory-structure*)
      (make-directory (concat dir directory)))
    (message (concat "Module structure created in " dir))))

;;; **************
;;; CODE INSERTION
;;; **************
(defun zf-insert-action (arg name)
  "Inserts a new action named NAME with class SCOPE into the
  buffer.  The prefix argument is currently ignored, but is left
  for possible future use."
  (interactive `(,current-prefix-arg
                 ,(read-string "Action name: ")))
  (apply 'php-insert-method
         (apply 'php-get-insert-method-arguments
                (append `(,(concat name "Action") public)
                        (unless (consp arg)
                          '(:staticp nil :abstractp nil :finalp nil))))))

(defun zf-insert-controller-completion-filterp (x)
  (string-match "^Zend_Controller_" x))

(defun zf-insert-constant (arg &optional name value)
  "Inserts a new constant named NAME with value VALUE into the
buffer.  The prefix argument is currently ignored, but is left
for possible future use."
  (interactive `(,current-prefix-arg))
  (apply 'php-insert-constant
         (apply 'php-get-insert-constant-arguments
                (append `(,name ,value)))))

(defun zf-insert-property (arg &optional name scope value)
  "Inserts a new property named NAME with class SCOPE into the
  buffer.  SCOPE may be one of 'public, `u', 'private, `i',
  'protected or `o'.  The default VALUE may be given.  Unless
  prefix argument is given, assume that the property is not
  static."
  (interactive `(,current-prefix-arg))
  (apply 'php-insert-property
         (apply 'php-get-insert-property-arguments
                (append `(,name ,scope ,value)
                        (unless (consp arg) '(:staticp nil))))))

(defun zf-insert-method (arg &optional name scope)
  "Inserts a new method named NAME with class SCOPE into the
  buffer.  SCOPE may be one of 'public, `u', 'private, `i',
  'protected or `o'.  Unless prefix argument is given, assume
  that the method is neither static, abstract nor final."
  (interactive `(,current-prefix-arg))
  (apply 'php-insert-method
         (apply 'php-get-insert-method-arguments
                (append `(,name ,scope)
                        (unless (consp arg)
                          '(:staticp nil :abstractp nil :finalp nil))))))

(defun zf-insert-interface (arg &optional name)
  "Inserts a new interface named NAME into the buffer.  Unless
  prefix argument is given, assume that the interface doesn't
  extends any interfaces."
  (interactive `(,current-prefix-arg))
  (apply 'php-insert-interface
         (apply 'php-get-insert-interface-arguments
                (append `(,(when (stringp name) (upcase-initials name)))
                        (unless (consp arg)
                          '(:extends nil))))))

(defun zf-insert-class (arg &optional name)
  "Inserts a new class named NAME into the buffer.  Unless prefix
  argument is given, assume that the class doesn't implement any
  interfaces and is neither abstract nor final."
  (interactive `(,current-prefix-arg))
  (apply 'php-insert-class
         (apply 'php-get-insert-class-arguments
                (append `(,(when (stringp name) (upcase-initials name)))
                        (unless (consp arg)
                          '(:implements nil :finalp nil :abstractp nil))))))

(defun* zf-insert-bootstrap (arg &optional (name "Bootstrap")
                                 (module (zf-default-module-name) module-p))
  "Inserts a new bootstrap named NAME into the buffer.  If MODULE
  is given, the model name reflects the module given.  Defaults
  to extending Zend_Application_Bootstrap_Bootstrap.  Unless
  prefix argument is given, assume that the model doesn't
  implement any interfaces and is neither abstract nor final."
  (interactive `(,current-prefix-arg
                 ,(read-string "Bootstrap name: " "Bootstrap")
                 ,(zf-get-module-name)))
  (if (nil-or-blank name)
      (message "You must give the bootstrap a name!")
    (apply 'php-insert-class
           (apply 'php-get-insert-class-arguments
                  (append `(,(concat
                              (when (stringp module)
                                (concat (upcase-initials module) "_"))
                              (upcase-initials name))
                            :extends-default
                            ,(if (stringp module)
                                 "Zend_Application_Module_Bootstrap"
                               "Zend_Application_Bootstrap_Bootstrap"))
                          (unless (consp arg)
                            '(:implements nil :finalp nil :abstractp nil)))))))

(defun* zf-insert-controller (arg name &optional
                                  (module (zf-default-module-name) module-p))
  "Inserts a new controller named NAME into the buffer.  If
  MODULE is given, the model name reflects the module given.
  Defaults to extending Zend_Controller_Action.  Unless prefix
  argument is given, assume that the controller doesn't implement
  any interfaces and is neither abstract nor final."
  (interactive `(,current-prefix-arg
                 ,(read-string "Controller name: ")
                 ,(zf-get-module-name)))
  (if (nil-or-blank name)
      (message "You must give the controller a name!")
    (apply 'php-insert-class
           (apply 'php-get-insert-class-arguments
                  (append `(,(concat
                              (when (and (stringp module)
                                         (not
                                          (string= module
                                                   (zf-default-module-name))))
                                (concat (upcase-initials module) "_"))
                              (upcase-initials name) "Controller")
                            :extends-default "Zend_Controller_Action")
                          (unless (consp arg)
                            '(:implements nil :finalp nil :abstractp nil)))))))

(defun* zf-insert-model (arg name &optional
                             (module (zf-default-module-name) module-p))
  "Inserts a new model named NAME into the buffer.  If MODULE is
  given, the model name reflects the module given.  Unless prefix
  argument is given, assume that the model doesn't implement any
  interfaces and is neither abstract nor final."
  (interactive `(,current-prefix-arg
                 ,(read-string "Model name: ")
                 ,(zf-get-module-name)))
  (if (nil-or-blank name)
      (message "You must give the model a name!")
    (apply 'php-insert-class
           (apply 'php-get-insert-class-arguments
                  (append `(,(concat (upcase-initials (if (stringp module)
                                                          module
                                                        (zf-global-namespace)))
                                     "_Model_"
                                     (upcase-initials name)))
                          (unless (consp arg)
                            '(:implements nil :finalp nil :abstractp nil)))))))

(defun* zf-insert-dbtable-model (arg name &optional
                                     (module (zf-default-module-name) module-p))
  "Inserts a new DbTable model named NAME into the buffer.  If
  MODULE is given, the model name reflects the module given.
  Defaults to extending Zend_Db_Table_Abstract.  Unless prefix
  argument is given, assume that the DbTable model doesn't
  implement any interfaces and is neither abstract nor final."
  (interactive `(,current-prefix-arg
                 ,(read-string "DbTable Model name: ")
                 ,(zf-get-module-name)))
  (if (nil-or-blank name)
      (message "You must give the DbTable model a name!")
    (apply 'php-insert-class
           (apply 'php-get-insert-class-arguments
                  (append `(,(concat (upcase-initials (if (stringp module)
                                                          module
                                                        (zf-global-namespace)))
                                     "_Model_DbTable_"
                                     (upcase-initials name))
                            :extends-default "Zend_Db_Table_Abstract")
                          (unless (consp arg)
                            '(:implements nil :finalp nil :abstractp nil)))))))

(defun* zf-insert-form (arg name &optional
                            (module (zf-default-module-name) module-p))
  "Inserts a new form named NAME into the buffer.  If MODULE
  is given, the model name reflects the module given.  Defaults
  to extending Zend_Form.  Unless prefix argument is given,
  assume that the form doesn't implement any interfaces and is
  neither abstract nor final."
  (interactive `(,current-prefix-arg
                 ,(read-string "Form name: ")
                 ,(zf-get-module-name)))
  (if (nil-or-blank name)
      (message "You must give the form a name!")
    (apply 'php-insert-class
           (apply 'php-get-insert-class-arguments
                  (append `(,(concat (upcase-initials (if (stringp module)
                                                          module
                                                        (zf-global-namespace)))
                                     "_Form_"
                                     (upcase-initials name))
                            :extends-default "Zend_Form")
                          (unless (consp arg)
                            '(:implements nil :finalp nil :abstractp nil)))))))

(defun zf-insert-dump (&optional arg)
  "Inserts a Zend_Debug::dump() statement."
  (interactive "P")
  (insert "Zend_Debug::dump(")
  (save-excursion
    (insert ");")
    (when (consp arg)
      (newline-and-indent)
      (insert "die;"))))

(defun zf-insert-dump-and-die ()
  "Inserts a Zend_Debug::dump() statement with a die afterwards."
  (interactive)
  (zf-insert-dump t))

;;; ***********************
;;; File/directory creation
;;; ***********************
(defun zf-find-file (filename directory)
  (let ((file (convert-standard-filename (concat directory filename))))
    (when (file-exists-p file)
      (find-file file)
      t)))

(defun zf-name->autoload-spec (name &optional type)
  "Takes a PHP class/interface name, applies the autoloader
rules, and returns a cons of (directory . filename).  Optional
argument TYPE is not currently used, but is there in case the
autoloader ever treats classes and interfaces differently."
  (let* ((type (or type 'class))
         (type-string (symbol-name type))
         (parts (split-string name "_"))
         (name (first (last parts)))
         (filename (zf-build-filename type-string name))
         (directory (file-name-as-directory
                     (mapconcat 'identity (butlast parts) "/"))))
    `(,directory . ,filename)))

(defun* zf-get-class/interface-arguments (type &optional name directory &key
                                               (filename nil filename-p)
                                               filename-default)
  "Gather arguments for zf-{class,interface}."
  (let* ((type-string (symbol-name type))
         (name (or name (php-completion-read-class/interface type)))
         (check (if (nil-or-blank name)
                    (error (concat "Must give " type-string " a name!"))))
         (directory
          (or directory (read-directory-name "Directory: "
                                             (php-project-directory))))
         (normal-filename (or filename-default
                              (zf-build-filename type-string name)))
         (filename (if filename-p
                       filename
                     (read-string "File name: " normal-filename))))
    `(,name ,directory ,filename)))

(defun zf-class/interface (arg type &optional name directory filename)
  "Creates a new TYPE (either 'class or 'interface) named NAME in
the given DIRECTORY.  If ARG, ask for filename rather than
assuming the normal generation from TYPE name.  Passes ARG along
to `zf-insert-TYPE.'"
  (let* ((type-string (symbol-name type))
         (insert-func
          (symbol-function (intern (concat "zf-insert-" type-string)))))
    (when (functionp insert-func)
      (let* ((insert-args
              (apply 'zf-get-class/interface-arguments
                     (append `(,type ,name ,directory)
                             (if (consp arg)
                                 `(:filename-default ,filename)
                               `(:filename ,filename)))))
             (name (first insert-args))
             (directory (second insert-args))
             (filename (third insert-args)))
        (unless (zf-find-file filename directory)
          (when (php-create-new-file filename directory)
            (funcall insert-func arg (upcase-initials name))
            (write-file filename)))))))

(defun zf-interface (arg &optional name directory filename)
  "Creates a new interface file named NAME in the given DIRECTORY.
If ARG, ask for filename rather than assuming the normal
generation from interface name.  Passes ARG along to
`zf-insert-interface.'"
  (interactive "P")
  (zf-class/interface arg 'interface name directory filename))

(defun zf-class (arg &optional name directory filename)
  "Creates a new class file named NAME in the given DIRECTORY.
If ARG, ask for filename rather than assuming the normal
generation from class name.  Passes ARG along to
`zf-insert-class.'"
  (interactive "P")
  (zf-class/interface arg 'class name directory filename))

(defun zf-library-class/interface (arg type &optional name directory)
  "Creates a new TYPE file named NAME in the project library
directory.  It will derive the final directory using normal
autoloader rules.  If ARG, prompt for this directory, and pass
ARG on to zf-TYPE."
  (let* ((type-string (symbol-name type))
         (create-func
          (symbol-function (intern (concat "zf-" type-string)))))
    (when (functionp create-func)
      (let* ((name (or name (php-completion-read-class/interface type)))
             (autoload-spec (zf-name->autoload-spec name type))
             (filename (rest autoload-spec))
             (computed-directory
              (concat (php-project-library-directory) (first autoload-spec)))
             (create-args
              `(,arg ,name
                     ,(if (consp arg)
                          (read-directory-name "Directory: " computed-directory)
                        computed-directory)
                     ,filename)))
        (apply create-func create-args)))))

(defun zf-library-interface (arg &optional name directory)
  "Creates a new interface file named NAME in the project library
directory.  It will derive the final directory using normal
autoloader rules.  If ARG, prompt for this directory, and pass
ARG on to zf-interface."
  (interactive "P")
  (zf-library-class/interface arg 'interface  name directory))

(defun zf-library-class (arg &optional name directory)
  "Creates a new class file named NAME in the project library
directory.  It will derive the final directory using normal
autoloader rules.  If ARG, prompt for this directory, and pass
ARG on to zf-class."
  (interactive "P")
  (zf-library-class/interface arg 'class  name directory))

(defun* zf-controller (arg name &optional
                           (module (zf-default-module-name) module-p)
                           project)
  "Creates a new controller file named NAME inside MODULE (if
  given). Defaults to extending Zend_Controller_Action.  Unless
  prefix argument is given, assume that the model doesn't
  implement any interfaces and is neither abstract nor final."
  (interactive (let* ((module-cons (zf-get-module-name))
                      (module (car module-cons))
                      (project (cdr module-cons))
                      (name (zf-get-controller-name module project)))
                 `(,current-prefix-arg ,name ,module ,project)))
  (let ((project (or project (php-project-buffer-project))))
    (if (nil-or-blank name)
        (message "Must give the controller a name!"))
    (setq directory (zf-get-directory "controller" module nil nil project))
    (setq filename (zf-build-filename "controller" name))
    (unless (zf-find-file filename directory)
      (when (php-create-new-file filename directory)
        (zf-insert-controller arg (upcase-initials name) module)
        (write-file filename)))))

(defun* zf-model (arg name &optional (module (zf-default-module-name) module-p)
                      project)
  "Creates a new model file named NAME inside MODULE (if given).
Unless prefix argument is given, assume that the model doesn't
implement any interfaces and is neither abstract nor final."
  (interactive (let* ((module-cons (zf-get-module-name))
                      (module (car module-cons))
                      (project (cdr module-cons))
                      (name (zf-get-model-name module project)))
                 `(,current-prefix-arg ,name ,module ,project)))
  (let ((project (or project (php-project-buffer-project))))
    (if (nil-or-blank name)
        (message "Must give the model a name!")
      (setq directory (zf-get-directory "model" module nil nil project))
      (setq filename (zf-build-filename "model" name))
      (unless (zf-find-file filename directory)
        (when (php-create-new-file filename directory)
          (zf-insert-model arg (upcase-initials name) module)
          (write-file filename))))))

(defun* zf-dbtable-model (arg name &optional
                              (module (zf-default-module-name) module-p)
                              project)
  "Creates a new DbTable model file named NAME inside MODULE (if
  given). Defaults to extending Zend_Db_Table_Abstract.  Unless
  prefix argument is given, assume that the model doesn't
  implement any interfaces and is neither abstract nor final."
  (interactive (let* ((module-cons (zf-get-module-name))
                      (module (car module-cons))
                      (project (cdr module-cons))
                      (name (zf-get-dbtable-model-name module project)))
                 `(,current-prefix-arg ,name ,module ,project)))
  (let ((project (or project (php-project-buffer-project))))
    (if (nil-or-blank name)
        (message "Must give DbTable model a name!")
      (setq directory (zf-get-directory "dbtable" module nil nil project))
      (setq filename (zf-build-filename "model" name))
      (unless (zf-find-file filename directory)
        (when (php-create-new-file filename directory)
          (zf-insert-dbtable-model arg (upcase-initials name) module)
          (write-file filename))))))

(defun* zf-form (arg name &optional
                     (module (zf-default-module-name) module-p) project)
  "Creates a new Zend form file named NAME inside MODULE (if
  given). Defaults to extending Zend_Form.  Unless prefix
  argument is given, assume that the model doesn't implement any
  interfaces and is neither abstract nor final."
  (interactive (let* ((module-cons (zf-get-module-name))
                      (module (car module-cons))
                      (project (cdr module-cons))
                      (name (zf-get-form-name module project)))
                 `(,current-prefix-arg ,name ,module ,project)))
  (let ((project (or project (php-project-buffer-project))))
    (if (nil-or-blank name)
        (message "Must give form a name!")
      (setq directory (zf-get-directory "form" module nil nil project))
      (setq filename (zf-build-filename "form" name))
      (unless (zf-find-file filename directory)
        (when (php-create-new-file filename directory)
          (zf-insert-form arg (upcase-initials name) module)
          (write-file filename))))))

(defun* zf-bootstrap (arg &optional
                          (name "Bootstrap")
                          (module (zf-default-module-name) module-p))
  "Creates a new bootstrap file named NAME (if given) inside
  MODULE (if given). NAME defaults to Bootstrap.  Defaults to
  extending Zend_Application_Bootstrap_Bootstrap (or if MODULE
  given, Zend_Application_Module_Bootstrap).  Unless prefix
  argument is given, assume the name is Bootstrap and assume that
  the model doesn't implement any interfaces and is neither
  abstract nor final."
  (interactive `(,current-prefix-arg
                 ,(if current-prefix-arg
                      (read-string "Bootstrap name: ")
                    "Bootstrap")
                 ,(zf-get-module-name)))
  (let ((project (when (consp module) (cdr module)))
        (module (if (consp module) (car module) module)))
    (setq directory (zf-get-directory "bootstrap" module nil nil project))
    (setq filename (zf-build-filename "bootstrap" name))
    (unless (zf-find-file filename directory)
      (when (php-create-new-file filename directory)
        (zf-insert-bootstrap arg (upcase-initials name) module)
        (write-file filename)))))

(defun zf-view-script (name module controller url &optional project)
  "Creates a new view script file named NAME for CONTROLLER
inside NODULE.  Completion for viewscript name will be provided
for travelling to existing viewscripts.  Optionally lock down to
URL."
  (interactive (let* ((module-cons (zf-get-module-name))
                      (module (car module-cons))
                      (project (cdr module-cons))
                      (controller (zf-get-controller-name module project))
                      (name (zf-get-viewscript-name controller module nil
                                                    project))
                      (name-parts (split-string name "/"))
                      (name (if (> (length name-parts) 1)
                                (second name-parts)
                              (first name-parts)))
                      (url (when (> (length name-parts) 1) (first name-parts))))
                 `(,name ,module ,controller ,url ,project)))
  (let ((project (or project (php-project-buffer-project))))
    (if (nil-or-blank name)
        (message "Must give the view script a name!")
      (if (nil-or-blank controller)
          (message "Must give a controller!")
        (setq directory (zf-get-directory "viewscript" module controller url
                                          project))
        (setq filename (zf-build-filename "viewscript" name))
        (unless (zf-find-file filename directory)
          (when (php-create-new-file filename directory)
            (delete-char -1)
            (write-file filename)))))))

(defun zf-view-script-via-controller-action (arg)
  "Opens the file for the view script where the cursor is located."
  (interactive "P")
  (when (setq action-info (zf-get-view-script-info))
    (let* ((name (first action-info))
           (module (second action-info))
           (controller (third action-info))
           (url (when (consp arg) (zf-get-viewscript-url module controller))))
      (zf-view-script name module controller url))))

(defun* zf-config (name type &optional
                        (module (zf-default-module-name) module-p)
                        project)
  "Creates a new config file named name, in 'module' if
given. Type is either XML or INI."
  (interactive (let* ((module-cons (zf-get-module-name))
                      (module (car module-cons))
                      (project (cdr module-cons))
                      (name (zf-get-config-name module project))
                      (type (completing-read "XML or INI (x/i): " '("i" "x")
                                             nil t)))
                 `(,name ,type ,module ,project)))
  (let ((project (or project (php-project-buffer-project))))
    (if (nil-or-blank name)
        (message "Must give the config file a name!")
      (if (or (nil-or-blank type)
              (and (not (string= type "X"))
                   (not (string= type "x"))
                   (not (string= type "I"))
                   (not (string= type "i"))))
          (message "Must select either 'x' or 'i' for the type!")
        (setq directory (zf-get-directory "config" module nil nil project))
        (setq filename (zf-build-filename "config" name module type))
        (unless (zf-find-file filename directory)
          (when (php-create-new-file filename directory t)
            (write-file filename)))))))

;;; *****************
;;; Utility functions
;;; *****************
(defun zf-get-view-script-info ()
  "Returns the action, controller, and modules names for the view
script where the point is located."
  (when (and buffer-file-name
         (string= (substring buffer-file-name -14 nil) "Controller.php"))
    (let ((module-name (zf-get-current-module-name))
          (controller-name
           (substring (car (last (split-string buffer-file-name "/"))) 0 -14))
          (parse (php-parse-current 'method)))
      (when (php-parse-p parse)
        (let ((name (rest (assoc 'name parse))))
          (when (string= "Action" (substring name -6))
            (let ((action-name (substring name 0 -6)))
              `(,action-name ,module-name ,controller-name))))))))

(defun* zf-get-directory (subject &optional
                                  (module (zf-default-module-name) module-p)
                                  controller url project)
  "Creates and returns the appropriate directory for a given Zend
Framework element (controller, model, etc). The third and fourth
arguments are used when getting the directory for a view script.
If the fourth is omitted the default conversion specified in the
defcustom `zf-use-hyphens-in-viewscript-urls' will be used."
  (if (nil-or-blank subject)
      (message "Must give a subject!")
    (let ((dirappend (if (string= "library" subject)
                         "/library"
                       (concat "/application"
                               (when (stringp module)
                                 (concat "/modules/" module)))))
          (dirtype (cond
                    ((string= subject "controller") "controllers")
                    ((string= subject "model") "models")
                    ((string= subject "dbtable") "DbTable")
                    ((string= subject "form") "forms")
                    ((string= subject "viewscript")
                     (let ((controller
                            (if (stringp url)
                                url
                              (zf-get-default-viewscript-url controller))))
                       (concat "views/scripts/" controller)))
                    ((string= subject "config") "configs"))))
      (concat (php-project-directory project) dirappend "/"
              (when (stringp dirtype)
                (concat dirtype "/"))))))

(defun zf-build-filename (subject &optional name module type)
  "Creates and returns the appropriate file name for a given Zend
Framework element (controller, model, etc."
  (if (nil-or-blank subject)
      (message "Must profile a subject!")
    (if (and (nil-or-blank name)
             (not (string= subject "bootstrap")))
        (message "Must provide a name for the file!")
      (cond
       ((or (string= subject "interface")
            (string= subject "class")
            (string= subject "model")
            (string= subject "form"))
        (concat (upcase-initials name) ".php"))
       ((string= subject "controller")
        (concat (upcase-initials name) "Controller.php"))
       ((string= subject "bootstrap")
        (concat (if (nil-or-blank name)
                    "Bootstrap"
                  (upcase-initials name)) ".php"))
       ((string= subject "viewscript")
        (concat name ".phtml"))
       ((string= subject "config")
        (if (or (string= type "X")
                (string= type "x"))
            (concat name ".xml")
          (when (or (string= type "I")
                    (string= type "i"))
            (concat name ".ini"))))))))

(defun zf-get-application-config ()
  "Opens the application config file for the project."
  (convert-standard-filename
   (concat (php-project-directory) "/application/configs/application.ini")))

(defun zf-open-application-config ()
  "Opens the phpunit application file for the project."
  (interactive)
  (find-file (zf-get-application-config)))

(defun zf-global-namespace ()
  "Returns the value of the global namespace for this project."
  (let ((ns (php-project-zend-global-namespace)))
    (if (nil-or-blank ns) "Default" ns)))

(defun zf-default-module-name ()
  "Returns the default module name as set in php-project-list,
defaulting to nil."
  (let ((dm (php-project-zend-default-module-name)))
    (if (nil-or-blank dm) nil dm)))

(defun zf-module-list ()
  "Returns a list of modules for the current ZF project."
  (let* ((project (or (php-project-buffer-project)
                      (php-project-ask-for-project)))
         (modules-directory
          (expand-file-name (concat (php-project-directory project)
                                    "/application/modules"))))
    (cons (append `("*none*")
                  (when (file-exists-p modules-directory)
                    (remove-if (lambda (x)
                                 (string= "." (substring x 0 1)))
                               (directory-files modules-directory))))
          project)))

(defun zf-get-current-module-name ()
  "Get the module name of the current file."
  (let ((fn (buffer-file-name)))
    (when (stringp fn)
      (let ((parts (split-string fn "/")))
        (catch 'found
          (dotimes (i (length parts))
            (when (string= (elt parts i) "modules")
              (throw 'found (elt parts (1+ i))))))))))

(defun zf-get-module-name ()
  "Read in a ZF module name."
  (let* ((module-cons (zf-module-list))
         (module-list (car module-cons))
         (project (cdr module-cons))
         (default-module (zf-get-current-module-name))
         (default-module (or default-module "*none*"))
         (name (completing-read (concat "Module (" default-module "): ")
                                  module-list nil t nil nil default-module))
         (name (unless (string= name "*none*") name)))
    (cons name project)))

(defun zf-model-list (&optional module project)
  "Returns a list of models for the current ZF project.  MODULE
defaults to the global module."
  (let ((models-directory
         (expand-file-name (zf-get-directory "model" module nil nil project))))
    (when (file-exists-p models-directory)
      (mapcar (lambda (x)
                (substring x 0 -4))
              (remove-if-not
               (lambda (x)
                 (string= ".php"
                          (substring x -4)))
               (remove-if (lambda (x)
                            (string= "." (substring x 0 1)))
                          (directory-files models-directory)))))))

(defun zf-get-model-name (&optional module project)
  "Read in a ZF model name.  Autocompletion from MODULE is
available.  MODULE defaults to the global module."
  (let ((model-list (zf-model-list module project)))
    (completing-read "Model: " model-list)))

(defun zf-dbtable-model-list (&optional module project)
  "Returns a list of DbTable models for the current ZF project.  MODULE
defaults to the global module."
  (let ((dbtable-models-directory
         (expand-file-name (zf-get-directory "dbtable" module nil nil
                                             project))))
    (when (file-exists-p dbtable-models-directory)
      (mapcar (lambda (x)
                (substring x 0 -4))
              (remove-if-not
               (lambda (x)
                 (string= ".php"
                          (substring x -4)))
               (remove-if (lambda (x)
                            (string= "." (substring x 0 1)))
                          (directory-files dbtable-models-directory)))))))

(defun zf-get-dbtable-model-name (&optional module project)
  "Read in a ZF DbTable model name.  Autocompletion from MODULE is
available.  MODULE defaults to the global module."
  (let ((dbtable-model-list (zf-dbtable-model-list module project)))
    (completing-read "DbTable Model: " dbtable-model-list)))

(defun zf-form-list (&optional module project)
  "Returns a list of DbTable models for the current ZF project.
MODULE defaults to the global module."
  (let ((forms-directory
         (expand-file-name (zf-get-directory "form" module nil nil project))))
    (when (file-exists-p forms-directory)
      (mapcar (lambda (x)
                (substring x 0 -4))
              (remove-if-not
               (lambda (x)
                 (string= ".php"
                          (substring x -4)))
               (remove-if (lambda (x)
                            (string= "." (substring x 0 1)))
                          (directory-files forms-directory)))))))

(defun zf-get-form-name (&optional module project)
  "Read in a ZF DbTable model name.  Autocompletion from MODULE is
available.  MODULE defaults to the global module."
  (let ((form-list (zf-form-list module project)))
    (completing-read "Form: " form-list)))

(defun zf-config-list (&optional module project)
  "Returns a list of DbTable models for the current ZF project.
MODULE defaults to the global module."
  (let ((configs-directory
         (expand-file-name (zf-get-directory "config" module nil nil project))))
    (when (file-exists-p configs-directory)
      (mapcar (lambda (x)
                (substring x 0 -4))
              (remove-if-not
               (lambda (x)
                 (string-match "\\.\\(xml\\|ini\\)$" x))
               (remove-if (lambda (x)
                            (string= "." (substring x 0 1)))
                          (directory-files configs-directory)))))))

(defun zf-get-config-name (&optional module project)
  "Read in a ZF DbTable model name.  Autocompletion from MODULE is
available.  MODULE defaults to the global module."
  (let ((config-list (zf-config-list module project)))
    (completing-read "Config: " config-list)))

(defun zf-controller-list (&optional module project)
  "Returns a list of controllers for the current ZF project.
MODULE defaults to the global module."
  (let ((controllers-directory
         (expand-file-name (zf-get-directory "controller" module nil nil
                                             project))))
    (when (file-exists-p controllers-directory)
      (mapcar (lambda (x)
                (substring x 0 -14))
              (remove-if-not
               (lambda (x)
                 (string= "Controller.php"
                          (substring x -14)))
               (remove-if (lambda (x)
                            (string= "." (substring x 0 1)))
                          (directory-files controllers-directory)))))))

(defun zf-get-controller-name (&optional module project)
  "Read in a ZF controller name.  Autocompletion from MODULE is
available.  MODULE defaults to the global module."
  (let ((controller-list (zf-controller-list module project)))
    (completing-read "Controller: " controller-list)))

(defun zf-get-default-viewscript-url (controller)
  "Retrun the default viewscript url for a controller.  Respects
`zf-use-hyphens-in-viewscript-urls'."
  (if zf-use-hyphens-in-viewscript-urls
      (camelcase->hyphenated controller)
    (downcase controller)))

(defun zf-get-viewscripts-dir (module controller &optional project)
  "Return the top-level viewscripts director for MODULE and
CONTROLLER."
  (let ((dir (zf-get-directory "viewscript" module controller nil project)))
    (when (file-exists-p dir)
      (file-name-directory (substring (expand-file-name dir) 0 -1)))))

(defun zf-get-viewscript-dirs (module controller &optional url)
  "Get a list of viewscript directories that can be mapped from
CONTROLLER of MODULE.  Optionally lock return down to a certain
url."
  (when (stringp controller)
    (let* ((viewscripts-directory (zf-get-viewscripts-dir module controller)))
      (when viewscripts-directory
        (remove-if (lambda (x)
                     (or (string= "." (substring x 0 1))
                         (and (stringp url) (not (string= x url)))
                         (not (string=
                               (downcase controller)
                               (replace-regexp-in-string "-" ""
                                                         (downcase x))))))
                   (directory-files viewscripts-directory))))))

(defun zf-get-viewscript-url (module controller)
  "Read in a ZF viewscript URL.  Autocompletion for CONTROLLER of
MODULE is provided."
  (let ((url-list (zf-get-viewscript-dirs module controller)))
    (completing-read "URL: " url-list)))

(defun zf-viewscript-list (controller &optional module url project)
  "Returns a list of viewscripts for CONTROLLER in the current ZF project.
MODULE defaults to the global module.  If multiple possible
viewscript directories exist the completion list will include
them all with directories prepended, as well as a non-directory
version for the default."
  (let* ((viewscripts-directory (zf-get-viewscripts-dir module controller
                                                        project))
         (default-url (zf-get-default-viewscript-url controller)))
    (when viewscripts-directory
      (remove-if-not
       'identity
       (apply
        'append
        (apply 'append
               (mapcar (lambda (x)
                         (mapcar (lambda (y)
                                   (let* ((name (substring y 0 -6))
                                          (full-path (concat x "/" name)))
                                     `(,(when (string= x default-url) name)
                                       ,full-path)))
                                 (remove-if
                                  (lambda (x)
                                    (or (string= "." (substring x 0 1))
                                        (not (string= ".phtml"
                                                      (substring x -6)))))
                                  (directory-files
                                   (concat viewscripts-directory x)))))
                       (zf-get-viewscript-dirs module controller))))))))

(defun zf-get-viewscript-name (controller &optional module url project)
  "Read in a ZF viewscript name.  Autocompletion from CONTROLLER
of MODULE is available.  MODULE defaults to the global
module. URL may be specified if it differs from the normal
mapping (controlled by
`zf-use-hyphens-in-viewscript-urls')."
  (let ((viewscript-list (zf-viewscript-list controller module url project)))
    (completing-read "Viewscript: " viewscript-list)))

(provide 'php+-zf)

;;; php+-zf.el ends here
