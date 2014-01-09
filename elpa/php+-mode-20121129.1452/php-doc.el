;;; php-doc.el --- Makes phpdoc blocks more automated and easier.

;; Version: 2.0
;; Created: 10-26-2010
;; Copyright © 2010 Brian Zwahr
;; Author(s): 
;; Brian Zwahr <echosa@gmail.com>

;;; *****
;;; About
;;; *****

;; php-doc.el is a part of the php+-mode suite and contains convenience 
;; functions for phpdoc blocks. It will automatically insert phpdoc elements
;; such as author, description, and parameters.

;; *************************************************************************

;; ************
;; REQUIREMENTS
;; ************
(require 'php-completion)
(require 'php-funcs)
(require 'php-project)
(require 'string-utils)

;; ******
;; CUSTOM
;; ******
(defgroup php-doc nil
  "Customizations for php-doc."
  :group 'php)

(defcustom php-doc-default-author (cons "" "")
  "Default author to insert in doc blocks."
  :group 'php-doc
  :type '(cons (string :tag "Name") (string :tag "Email Address")))

(defcustom php-doc-default-copyright nil
  "Default copyright to insert in doc blocks."
  :group 'php-doc
  :type '(string :tag "Copyright"))

(defcustom php-doc-default-license nil
  "Default license to insert in doc blocks."
  :group 'php-doc
  :type '(string :tag "License"))

(defcustom php-doc-default-version nil
  "Default version string to insert in doc blocks."
  :group 'php-doc
  :type '(string :tag "Version"))

(defcustom php-doc-default-php-version nil
  "Default PHP version to insert in doc blocks."
  :group 'php-doc
  :type '(string :tag "PHP Version"))

(defcustom php-doc-default-link nil
  "Default link required by PEAR standard."
  :group 'php-doc
  :type '(string :tag "Link"))

(defcustom php-doc-default-category ""
  "Default category required by the PEAR standard.  May be a
function name."
  :group 'php-doc
  :type '(string :tag "Category"))

(defcustom php-doc-default-package ""
  "Default package required by the PEAR standard.  May be a
function name."
  :group 'php-doc
  :type '(string :tag "Package"))

(defcustom php-doc-default-subpackage ""
  "Default subpackage required by the PEAR standard.  May be a
function name."
  :group 'php-doc
  :type '(string :tag "Subpackage"))

;; *********
;; FUNCTIONS
;; *********
(defun php-doc-get-field-from-buffer (field)
  (let (category)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (concat "@" field "\\s +") nil t)
        (buffer-substring-no-properties (point) (line-end-position))))))

(defun php-doc-get-* (x)
  "Returns a PHP doc default X, looking first in the current
buffer, then the PHP Project PHP DOC list and then tries the
php-doc-default-X defcustom."
  (or (php-doc-get-field-from-buffer x)
      (let ((var-name (or (let ((doc-val 
                                 (elt (php-project-php-doc) 
                                      (cond ((string= x "author") 0)
                                            ((string= x "copyright") 1)
                                            ((string= x "license") 2)
                                            ((string= x "version") 3)
                                            ((string= x "php-version") 4)
                                            ((string= x "link") 5)
                                            ((string= x "category") 6)
                                            ((string= x "package") 7)
                                            ((string= x "subpackage") 8)))))
                            (when (not (and (stringp doc-val)
                                            (zerop (length doc-val))))
                              doc-val))
                          (symbol-value (intern (concat "php-doc-default-" 
                                                        x))))))
        (if (not (stringp var-name))
            var-name
          (let ((sym (intern var-name)))
            (if (functionp sym) (funcall sym) var-name))))))

(defun php-doc-get-author ()
  "Returns a default author, respecting the
php-doc-default-author defcustom."
  (php-doc-get-* "author"))

(defun php-doc-get-copyright ()
  "Returns a default copyright, respecting the
php-doc-default-copyright defcustom."
  (php-doc-get-* "copyright"))

(defun php-doc-get-license ()
  "Returns a default license, respecting the
php-doc-default-license defcustom."
  (php-doc-get-* "license"))

(defun php-doc-get-version ()
  "Returns a default version, respecting the
php-doc-default-version defcustom."
  (php-doc-get-* "version"))

(defun php-doc-get-php-version ()
  "Returns a default php-version, respecting the
php-doc-default-php-version defcustom."
  (php-doc-get-* "php-version"))

(defun php-doc-get-link ()
  "Returns a default link, respecting the
php-doc-default-link defcustom."
  (php-doc-get-* "link"))

(defun php-doc-get-category ()
  "Returns a default category, respecting the
php-doc-default-category defcustom."
  (php-doc-get-* "category"))

(defun php-doc-get-package ()
  "Returns a default package, respecting the
php-doc-default-package defcustom."
  (php-doc-get-* "package"))

(defun php-doc-get-subpackage ()
  "Returns a default subpackage, respecting the
php-doc-default-subpackage defcustom."
  (php-doc-get-* "subpackage"))

(defun php-doc-file/class/iface-get-arguments (type &optional short-desc 
                                                    long-desc category package 
                                                    subpackage author license 
                                                    link)
  "This function collects the arguments common to files, classes
and interfaces."
  (let ((type-str (symbol-name type)))
    `(,(or short-desc 
           (upcase-first (read-string (concat "Short description of " 
                                              type-str ": ")
                                      (php-doc-get-file-description))))
      ,(or (upcase-first (read-string (concat "Long description of " 
                                              type-str ": ")
                                      (php-doc-get-file-description t))))
      ,(or (upcase-first (read-string (concat "Category of " type-str ": ") 
                                      (php-doc-get-category))))
      ,(or (upcase-first (read-string (concat "Package of " type-str ": ") 
                                      (php-doc-get-package))))
      ,(or (upcase-first (read-string (concat "Subpackage of " type-str ": ") 
                                      (php-doc-get-subpackage))))
      ,(or (read-string (concat "Author of " type-str ": ") 
                        (if (listp (php-doc-get-author))
                            (concat (car (php-doc-get-author)) 
                                    " <" (cdr (php-doc-get-author)) ">")
                          (php-doc-get-author))))
      ,(or (read-string (concat "License of " type-str ": ") 
                        (php-doc-get-license)))
      ,(or (read-string "Link: " (php-doc-get-link))))))

(defun php-doc-file-insert (short-desc long-desc category package subpackage 
                                       author license link php-version copyright
                                       version)
  "Inserts a phpdoc block for files."
  (interactive
   (append (php-doc-file/class/iface-get-arguments 'file)
           `(,(read-string "PHP Version of file: " (php-doc-get-php-version))
             ,(read-string "Copyright of file: " (php-doc-get-copyright))
             ,(read-string "Version string of file: " 
                           (php-doc-get-version)))))
  (let ((column (php-doc-start)))
    (when (php-doc-insert-desc short-desc column)
      (php-doc-newline column))
    (when (php-doc-insert-desc long-desc column)
      (php-doc-newline column))
    (when (php-doc-insert-php-version php-version column)
      (php-doc-newline column))
    (php-doc-insert-cat-pack-sub category package subpackage column)
    (php-doc-insert-author author column t)
    (php-doc-insert-copyright copyright column)
    (php-doc-insert-license license column)
    (php-doc-insert-version version column)
    (php-doc-insert-link link column)
    (php-doc-end column)))

(defun php-doc-interface-insert (short-desc long-desc category package 
                                            subpackage author license link)
  "Inserts a PHPDoc block for a PHP interface."
  (interactive (php-doc-file/class/iface-get-arguments 'interface))
  (apply 'php-doc-class/iface-insert 
         `(,short-desc ,long-desc ,category ,package ,subpackage ,author 
                       ,license ,link)))

(defun php-doc-class-insert (short-desc long-desc category package subpackage
                                        author license link)
  "Inserts a PHPDoc block for a PHP class."
  (interactive (php-doc-file/class/iface-get-arguments 'class))
  (apply 'php-doc-class/iface-insert 
         `(,short-desc ,long-desc ,category ,package ,subpackage ,author 
                       ,license ,link)))
  
(defun php-doc-class/iface-insert (short-desc long-desc category package 
                                              subpackage author license link)
  "Inserts a PHPDoc block for classes or interfaces."
  (let ((column (php-doc-start)))
    (when (php-doc-insert-desc short-desc column)
      (php-doc-newline column))
    (when (php-doc-insert-desc long-desc column)
      (php-doc-newline column))
    (php-doc-insert-cat-pack-sub category package subpackage column)
    (php-doc-insert-author author column t)
    (php-doc-insert-license license column)
    (php-doc-insert-link link column)
    (php-doc-end column)))

(defun php-doc-constant/property-get-arguments (&optional desc type)
  "Gathers arguments for a PHP constant or property PHPDoc block."
  `(,(or desc (read-string "Description: "))
    ,(or (completing-read "Variable type: "
                          (append (php-completion-get-etags)
                                  (php-completion-candidates 
                                   "" '("language.types" "class"))
                                  '("mixed"))))))

(defun php-doc-constant/property-insert (desc type)
  (let ((column (php-doc-start)))
    (when (php-doc-insert-desc desc column)
      (php-doc-newline column))
    (php-doc-insert-var type column)
    (php-doc-end column)))

(defun php-doc-property-insert (desc type)
  "This function inserts a doc block for a property."
  (interactive (php-doc-constant/property-get-arguments))
    (php-doc-constant/property-insert desc type)
    `(,desc ,type))

(defalias 'php-doc-constant-insert 'php-doc-property-insert)

(defun* php-doc-method-insert (short-desc long-desc author return-type 
                                          &key (var-list nil var-list-p))
  "This function inserts a doc block for a function or method.
The VAR-LIST may be passed in, in which case there will be no
prompting for them."
  (interactive 
   (append (let* ((short-desc (read-string "Short description: "))
                  (long-desc (if (> (length short-desc) 0)
                                 (read-string "Long description: ") "")))
             `(,short-desc ,long-desc))
           `(,(read-string (concat "Author: ") 
                           (if (listp (php-doc-get-author))
                               (concat (car (php-doc-get-author)) 
                                       " <" (cdr (php-doc-get-author)) ">")
                             (php-doc-get-author)))
             ,(completing-read 
               "Return type (void): " 
               (php-completion-get-type-list t)
               nil nil nil nil "void"))))
  (indent-according-to-mode)
  (let ((column (1+ (current-column))))
    (insert (substring
             (with-temp-buffer
               (php-doc-start)
               (when (php-doc-insert-desc short-desc column)
                 (php-doc-newline column))
               (when (php-doc-insert-desc long-desc column)
                 (php-doc-newline column))
               (when (setq var-list 
                           (apply 'php-doc-method-add-params 
                                  (append `(,column) 
                                          (when var-list-p 
                                            `(:var-list ,var-list)))))
                 (php-doc-newline column))
               (when (php-doc-insert-author author column)
                 (php-doc-newline column))
               (php-doc-insert-return return-type column)
               (php-doc-end column)
               (buffer-string)) 0 -1)))
  var-list)

(defun* php-doc-method-add-params (column &key (var-list nil var-list-p))
  "This function inserts parameters into PHP doc blocks.  If
VAR-LIST is passed in, just print what's in it and don't ask for
arguments."
  (let ((max-type-length 0)
        (max-name-length 0))
    (unless var-list-p
      (while (y-or-n-p "Add argument? ")
        (let* ((name (read-string 
                      "Variable name (without leading characters): "))
               (name (if (string= "&" (substring name 0 1))
                         (concat "&$" (substring name 1))
                       (concat "$" name)))
               (type (completing-read 
                      "Variable type: " 
                      (append (php-completion-get-etags)
                              (php-completion-candidates 
                               "" '("language.types" "class"))
                              '("callback" "mixed"))))
               (value (read-string "Default value: "))
               (value (when (not (string= value "")) value))
               (desc (read-string "Variable description: ")))
          (add-to-list 'var-list 
                       `((name . ,name) (type . ,type) (value . ,value) 
                         (desc . ,desc)) t))))
    (setf var-list (mapcar (lambda (var)
                             (if (rest (assq 'type var))
                                 var
                               (assq-delete-all 'type var)
                               (setf var (append var `((type . "mixed"))))))
                           var-list))
    (dolist (var var-list)
      (setf max-type-length 
            (max max-type-length (length (rest (assq 'type var)))))
      (setf max-name-length 
            (max max-name-length (length (rest (assq 'name var))))))
    (dolist (var (php-sort-method-args var-list))
      (php-doc-insert-param var column `(,max-type-length ,max-name-length)))
    var-list))

(defun php-doc-start ()
  "Starts a phpdoc block."
  (let ((column (+ (current-column) 1)))
    (insert "/")
    (php-doc-insert "**" column t)
    column))

(defun php-doc-end (column)
  "Ends a phpdoc block."
  (php-doc-insert "*/" column t)
  (indent-according-to-mode))

(defun php-doc-newline (column)
  "Inserts an empty doc line."
  (php-doc-insert "" column))

(defun php-doc-insert (text column &optional no-leading-asterisk max-column)
  "This function is a wrapper for the regular (insert) function."
  (let ((max-column (or max-column 75))
        (insert-string ""))
    (when column
      (let ((spaces (- column (current-column))))
        (when (wholenump spaces)
          (setf insert-string 
                (concat insert-string (make-string spaces ?\ ))))))
    (unless no-leading-asterisk
      (setf insert-string (concat insert-string "*"))
      (unless (zerop (length text))
        (setf insert-string (concat insert-string " "))))
    (setf insert-string (concat insert-string (chomp text)))
    (insert (concat (break-string insert-string "\\* " max-column) "\n"))
    t))

(defun php-doc-insert-author (author column &optional add-spacing)
  "Inserts an author line."
  (unless (nil-or-blank author)
    (let ((author-string (concat "@author " (when add-spacing "    ") author)))
      (php-doc-insert author-string column))))

(defun php-doc-insert-desc (desc column)
  "Inserts a description line."
  (unless (nil-or-blank desc)
    (php-doc-insert desc column)))

(defun php-doc-insert-return (text column)
  "Inserts a return line."
  (unless (nil-or-blank text)
    (php-doc-insert (concat "@return " text) column)))

(defun php-doc-insert-php-version (version column)
  "Inserts a php version line."
  (unless (nil-or-blank version)
    (php-doc-insert (concat "PHP Version " version) column)))

(defun php-doc-insert-category (category column)
  "Inserts a category line."
  (unless (nil-or-blank category)
    (php-doc-insert (concat "@category   " category) column)))
  
(defun php-doc-insert-package (package column)
  "Inserts a package line."
  (unless (nil-or-blank package)
    (php-doc-insert (concat "@package    " package) column)))
  
(defun php-doc-insert-subpackage (subpackage column)
  "Inserts a subpackge line."
  (unless (nil-or-blank subpackage)
    (php-doc-insert (concat "@subpackage " subpackage) column)))

(defun php-doc-insert-cat-pack-sub (&optional category package subpackage column)
  "Inserts category, package, and subpackage if given."
  (let (inserted)
    (when (php-doc-insert-category category column)
      (setq inserted t))
    (when (php-doc-insert-package package column)
      (setq inserted t))
    (when (php-doc-insert-subpackage subpackage column)
      (setq inserted t))
    (when inserted
      (php-doc-newline column))))

(defun php-doc-insert-copyright (copyright column)
  "Inserts a copyright line."
  (unless (nil-or-blank copyright)
    (php-doc-insert (concat "@copyright  " copyright) column)))

(defun php-doc-insert-license (license column)
  "Inserts a license line."
  (unless (nil-or-blank license)
    (php-doc-insert (concat "@license    " license) column)))

(defun php-doc-insert-version (version column)
  "Inserts a version line."
  (unless (nil-or-blank version)
    (php-doc-insert (concat "@version    " version) column)))

(defun php-doc-insert-link (link column)
  "Inserts a link line."
  (unless (nil-or-blank link)
    (php-doc-insert (concat "@link       " link) column)))

(defun php-doc-insert-var (type column)
  "Inserts a var line."
  (unless (nil-or-blank type)
    (php-doc-insert (concat "@var " type) column)))

(defun php-doc-insert-param (var column &optional max-lengths)
  "Inserts a param line."
  (let* ((name (rest (assoc 'name var)))
         (type (rest (assoc 'type var)))
         (type-spaces (make-string (or (and (sequencep max-lengths) 
                                            (> (length max-lengths) 0) 
                                            (integerp (elt max-lengths 0)) 
                                            (1+ (- (elt max-lengths 0) 
                                                   (length type)))) 1) ?\ ))
         (desc (rest (assoc 'desc var)))
         (name-spaces (make-string (or (and (sequencep max-lengths) 
                                            (> (length max-lengths) 1) 
                                            (integerp (elt max-lengths 1)) 
                                            (1+ (- (elt max-lengths 1) 
                                                   (length name)))) 1) ?\ )))
    (unless (or (nil-or-blank name)
                (nil-or-blank type))
      (php-doc-insert (concat "@param " type type-spaces name name-spaces desc) 
                      column))))

(defun php-doc-get-file-description (&optional long)
  "Grab the PHP doc file description.  If LONG then grab the long
description."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (concat "\\*[[:space:]]*" ws-re
                   "*[[:space:]]*\\*[[:space:]]*[[:word:]]")
           nil t (when long 2))
      (backward-char)
      (let ((begin (point)))
        (re-search-forward "^[[:space:]]*\\*[[:space:]]*$")
        (forward-line -1)
        (end-of-line)
        (let ((description (buffer-substring-no-properties begin (point))))
          (when (or (<= (length description) 11)
                    (not (string= "php version" 
                                  (downcase (substring description 0 11)))))
            (php-doc-merge-description-lines description)))))))

(defun php-doc-merge-description-lines (string)
  "This function takes a doc description and merges the lines into one line, 
removing the beginning ' * '."
  (replace-regexp-in-string (concat ws-re "*\\*" ws-re "*") " " string))

(provide 'php-doc)
