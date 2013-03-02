;;; php-completion.el --- PHP completion engine

;; Vrsion: 1.0
;; Created: 7-5-2011
;; Copyright © 2009 Brian Zwahr
;; Author(s): 
;; Michael Dwyer <mdwyer@ehtech.in>

;;; *****
;;; About
;;; *****

;; php-completion.el is a part of the php+-mode suite and is used to
;; provide completion functionality.

;;; *****
;;; Usage
;;; *****

;; Use customize to set php-manual-path and/or php-manual-url.
;;
;; These functions will default to using local resources if they are
;; available (unless php-completion-prefer-remote is set to t).  If a
;; local copy of the PHP many HTML manual files
;; (http://www.php.net/get/php_manual_en.tar.gz/from/a/mirror) is not
;; available, the functions may try to pull information from the
;; php-manual-url.  Some capabilities may not be available using this
;; method.

;; *************************************************************************

;; ******
;; CUSTOM
;; ******
(defgroup php-completion nil
  "Customizations for php-completion."
  :group 'php)

(defcustom php-completion-prefer-remote nil
  "Prefer remote methods over local."
  :group 'php-completion
  :type 'boolean)

;; *********
;; VARIABLES
;; ********

(defvar php-completion-index nil
  "Full cache of all available keywords.")

(defvar php-completion-hash-remote nil 
  "Cache for remote-retrieved completion candidates.")

(defvar php-completion-hash-local nil
  "Cache for local-retrieved completion candidates.")

;; defvar these to hush the compiler
(defvar php-manual-path)
(defvar php-manual-url)

;; *********
;; FUNCTIONS
;; *********

(defun php-completion-read-class/interface (type)
  (let ((type-string (symbol-name type)))
    (completing-read (concat 
                      (upcase-initials type-string) " name: ")
                     (append (php-completion-get-etags)
                             (php-completion-candidates "" '("class"))))))

(defun php-completion-read-class ()
  (php-completion-read-class/interface 'class))

(defun php-completion-read-interface ()
  (php-completion-read-class/interface 'interface))

(defun php-completion-candidates (arg hash-list)
  (all-completions (or arg "") (php--completion-candidates hash-list)))

(defun php-completion-build-index ()
  "Builds the master hash table indexing the PHP documentation."
  (let ((hash (make-hash-table :test 'equal))
        (dir-list (when (file-exists-p php-manual-path)
                    (cddr (directory-files php-manual-path)))))
    (dolist (entry dir-list)
      (let* ((parts (butlast (split-string entry "\\.")))
             (current-hash hash)
             (in-class (string= (first parts) "class")))
        (dotimes (i (length parts))
          (let* ((current-part (nth i parts))
                 (new-current-hash (gethash current-part current-hash)))
            (unless new-current-hash
              (let ((new-entry (make-hash-table :test 'equal)))
                (puthash (replace-regexp-in-string "-" "_" current-part) 
                         new-entry current-hash)
                (setq new-current-hash new-entry)))
            (setq current-hash new-current-hash)))
        (when in-class
          (let* ((file-url (concat "file://" (expand-file-name php-manual-path) 
                                   "/" entry))
                 (proper-name (php-completion-extract-class-name file-url)))
            (puthash 'proper-name proper-name current-hash)))))
    (setq php-completion-index hash)))

(defun php-completion-extract-class-name (url)
  (let ((buf (url-retrieve-synchronously url)))
    (when buffer-display-count
      (with-current-buffer buf
        (when (re-search-forward "class=\\\"classname\\\">\\([^<]*\\)<" nil t)
          (match-string-no-properties 1))))))
        
(defun php--completion-candidates (&optional hash-list)
  "Returns a list of all PHP completion candidate symbols."
  (if (and php-manual-path (file-readable-p php-manual-path))
      (php-completion-candidates-local hash-list)
    (php-completion-candidates-remote)))

(defun php-completion-candidates-local (&optional hash-list)
  "Returns a list of all PHP completion candidate symbols, using
the locally installed PHP manual.  Example
hash-list: `('function' 'class' 'language.types')"
  (unless (and (boundp 'php-completion-index) php-completion-index)
    (php-completion-build-index))
  (let ((cand-list '())
        (hash-list (or hash-list '("function"))))
    (dolist (hash hash-list)
      (let ((current-hash php-completion-index)
            (hash-address (split-string hash "\\.")))
        (dolist (next-hash (butlast hash-address))
          (setq current-hash (gethash next-hash current-hash)))
        (let ((subhash (gethash (first (last hash-address)) current-hash)))
          (when (hash-table-p subhash)
            (maphash (lambda (key val) 
                       (add-to-list 'cand-list 
                                    (or (and (hash-table-p val)
                                             (gethash 'proper-name val))
                                        key)))
                     subhash)))))
    (sort cand-list 'string<)))

(defun php-completion-candidates-remote ()
  "Returns a list of all PHP completion candidate symbols, using the PHP website."
  (unless (and (boundp 'php-completion-hash-remote) php-completion-hash-remote)
    (with-current-buffer
        (url-retrieve-synchronously "http://php.net/quickref.php")
      (goto-char (point-min))
      (if (re-search-forward "<!-- result list start -->" nil t)
          (let ((end (save-excursion
                       (if (re-search-forward "<!-- result list end -->" 
                                              nil t)
                           (point)))))
            (if end
                (let ((hash (make-hash-table)))
                  (while (re-search-forward ">\\([^<]+\\)</a>" end t)
                    (puthash (match-string 1) t hash))
                  (setq php-completion-hash-remote hash)))))))
  php-completion-hash-remote)

(defun php-completion-lookup (keyword)
  "Lookup meta-info for a PHP keyword."
  (interactive "sKeyword: ")
  (let* ((keyword (replace-regexp-in-string "_" "-" keyword))
         (help-url (if (and php-manual-path (file-readable-p php-manual-path))
                       (php-completion-lookup-url-local keyword)
                     (php-completion-lookup-url-remote keyword)))
         (help-buffer (url-retrieve-synchronously help-url)))
    (if help-buffer
        (php-completion-lookup-buffer help-buffer))))

(defun php-completion-lookup-url-local (keyword)
  "Build a url for looking up PHP keyword meta-info from a local copy."
  (interactive "sKeyword: ")
  (let* ((local-file (first (file-expand-wildcards 
                             (concat (expand-file-name php-manual-path) 
                                     "/function." keyword ".html")))))
    (when local-file
      (concat "file://" local-file))))

(defun php-completion-lookup-url-remote (keyword)
  "Build a url for looking up PHP keyword meta-info from the website."
  (interactive "sKeyword: ")
  (concat php-manual-url (concat "/function." keyword ".php")))

(defun php-completion-lookup-buffer (buf)
  "Retrieve PHP symbol meta-info from a buffer."
  (with-current-buffer buf
    (goto-char (point-min))
    (let (desc)
      (when (re-search-forward 
             "<div class=\"methodsynopsis dc-description\">\\(\\(.\\|\n\\)*?\\)</div>" 
             nil t)
        (setq desc
              (replace-regexp-in-string
               " +" " "
               (replace-regexp-in-string
                "\n" ""
                (replace-regexp-in-string "<.*?>" "" 
                                          (match-string-no-properties 1)))))
        
        (when (re-search-forward 
               "<p class=\"para rdfs-comment\">\\(\\(.\\|\n\\)*?\\)</p>" nil t)
          (setq desc
                (concat desc "\n\n"
                        (replace-regexp-in-string
                         " +" " "
                         (replace-regexp-in-string
                          "\n" ""
                          (replace-regexp-in-string 
                           "<.*?>" "" (match-string-no-properties 1)))))))))))

(defun php-completion-lookup-at-point ()
  "Lookup meta-info for the PHP symbol at point."
  (interactive)
  (let* ((function (symbol-name (or (symbol-at-point)
                                    (error "No function at point.")))))
    (php-completion-lookup function)))

(defun php-completion-lookup-at-point->message ()
  "Lookup meta-info for the PHP symbol at point and message so it
shows up on the minibuffer."
  (interactive)
  (message (php-completion-lookup-at-point)))

(defun php-completion-get-type-list (&optional include-void)
  (append (php-completion-get-etags)
          (php-completion-candidates "" '("language.types" "class"))
          `("mixed" ,(when include-void "void"))))

(defun php-completion-get-etags ()
  (save-excursion
    (visit-tags-table-buffer)
    (mapcar 'symbol-name 
            (remove-if (lambda (x) (eq x 0)) (etags-tags-completion-table)))))

(defun php-completion-customize ()
  "This functions opens the customize buffer for php-completion."
  (interactive)
  (customize-group "php-completion"))

(provide 'php-completion)
