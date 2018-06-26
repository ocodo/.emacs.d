;;; php-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "php-mode" "php-mode.el" (23344 44804 734850
;;;;;;  111000))
;;; Generated autoloads from php-mode.el

(let ((loads (get 'php 'custom-loads))) (if (member '"php-mode" loads) nil (put 'php 'custom-loads (cons '"php-mode" loads))))

(defvar php-extra-constants 'nil "\
A list of additional strings to treat as PHP constants.")

(custom-autoload 'php-extra-constants "php-mode" nil)

(if (version< emacs-version "24.4") (dolist (i '("php" "php5" "php7")) (add-to-list 'interpreter-mode-alist (cons i 'php-mode))) (add-to-list 'interpreter-mode-alist (cons "php\\(?:-?[3457]\\(?:\\.[0-9]+\\)*\\)?" 'php-mode)))

(define-obsolete-variable-alias 'php-available-project-root-files 'php-project-available-root-files "1.19.0")

(let ((loads (get 'php-faces 'custom-loads))) (if (member '"php-mode" loads) nil (put 'php-faces 'custom-loads (cons '"php-mode" loads))))

(autoload 'php-mode "php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

(autoload 'php-current-class "php-mode" "\
Insert current class name if cursor in class context.

\(fn)" t nil)

(autoload 'php-current-namespace "php-mode" "\
Insert current namespace if cursor in namespace context.

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons (eval-when-compile (rx (or (: "." (or (: "php" (32 (in "s345t"))) "amk" "phtml")) (: "/" (or "Amkfile" ".php_cs" ".php_cs.dist"))) string-end)) 'php-mode) t)

;;;***

;;;### (autoloads nil "php-project" "php-project.el" (23344 44804
;;;;;;  722849 829000))
;;; Generated autoloads from php-project.el

(defvar-local php-project-root 'auto "\
Method of searching for the top level directory.

`auto' (default)
      Try to search file in order of `php-project-available-root-files'.

SYMBOL
      Key of `php-project-available-root-files'.

STRING
      A file/directory name of top level marker.
      If the string is an actual directory path, it is set as the absolute path
      of the root directory, not the marker.")

(put 'php-project-root 'safe-local-variable #'(lambda (v) (or (stringp v) (assq v php-project-available-root-files))))

(defvar-local php-project-bootstrap-scripts nil "\
List of path to bootstrap php script file.

The ideal bootstrap file is silent, it only includes dependent files,
defines constants, and sets the class loaders.")

(put 'php-project-bootstrap-scripts 'safe-local-variable #'php-project--eval-bootstrap-scripts)

(defvar-local php-project-php-executable nil "\
Path to php executable file.")

(put 'php-project-php-executable 'safe-local-variable #'(lambda (v) (and (stringp v) (file-executable-p v))))

(defvar-local php-project-phan-executable nil "\
Path to phan executable file.")

(put 'php-project-phan-executable 'safe-local-variable #'php-project--eval-bootstrap-scripts)

(defvar-local php-project-coding-style nil "\
Symbol value of the coding style of the project that PHP major mode refers to.

Typically it is `pear', `drupal', `wordpress', `symfony2' and `psr2'.")

(put 'php-project-coding-style 'safe-local-variable #'symbolp)

(autoload 'php-project-get-bootstrap-scripts "php-project" "\
Return list of bootstrap script.

\(fn)" nil nil)

(autoload 'php-project-get-root-dir "php-project" "\
Return path to current PHP project.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("php-array.el" "php-classobj.el" "php-control-structures.el"
;;;;;;  "php-crack.el" "php-dio.el" "php-dom.el" "php-exceptions.el"
;;;;;;  "php-exif.el" "php-ext.el" "php-filesystem.el" "php-gd.el"
;;;;;;  "php-math.el" "php-mode-pkg.el" "php-pcre.el" "php-regex.el"
;;;;;;  "php-simplexml.el" "php-strings.el" "php-var.el" "php-xmlparser.el"
;;;;;;  "php-xmlreader.el") (23344 44804 746850 393000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; php-mode-autoloads.el ends here
