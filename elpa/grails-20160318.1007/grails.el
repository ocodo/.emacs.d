;;; grails.el --- Minor mode for Grails projects
;;
;; Copyright (c) 2016 Alessandro Miliucci
;;
;; Authors: Alessandro Miliucci <lifeisfoo@gmail.com>
;; Version: 0.2.2
;; Package-Version: 20160318.1007
;; URL: https://github.com/lifeisfoo/emacs-grails
;; Package-Requires: ((emacs "24"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Description:

;; Grails.el is a minor mode that allows an easy
;; navigation of Gails projects.  It allows jump to a model, to a view,
;; to a controller or to a service.
;;
;; For more details, see the project page at
;; https://github.com/lifeisfoo/emacs-grails
;;
;; Installation:
;;
;; Copy this file to to some location in your Emacs load path.  Then add
;; "(require 'grails)" to your Emacs initialization (.emacs,
;; init.el, or something).
;;
;; Example config:
;;
;;   (require 'grails)

;; Then, to auto enable grails mode, create a .dir-locals.el file
;; in the root of the grails project with this configuration:

;; ((groovy-mode (grails . 1))
;;  (html-mode (grails . 1))
;;  (java-mode (grails . 1)))

;; In this way, the grails mode will be auto enabled when any of
;; these major modes are loaded (only in this directory tree - the project tree)
;; (you can attach it to other modes if you want).

;; The first time that this code is executed, Emacs will show a security
;; prompt: answer "!" to mark code secure and save your decision.
;; (a configuration line is automatically added to your .emacs file)

;; In order to have grails minor mode always enabled inside your project tree,
;; place inside your `.dir-locals.el`:

;;   ((nil . ((grails . 1))))
;;

;;; Code:

(defvar grails-dir-name-by-type
  '((controller "controllers")
    (domain "domain")
    (service "services")
    (view "views")))

(defvar grails-postfix-by-type
  '((controller "Controller.groovy")
    (domain ".groovy")
    (service "Service.groovy")
    (view ".gsp")))

(defvar grails-properties-by-version
  '((2 "application.properties" "^app.grails.version=")
    (3 "gradle.properties" "^grailsVersion=")))

;;
;;
;; Utils functions
;;
;;

(defun util-string-from-file (file-path)
  "Return a string with file-path contents."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

;;
;;
;; Internal functions (non interactive)
;;
;;

(defun grails-grep-version (string)
  "Try all regex on the string to detect major version.

  E.g. string=grailsVersion=3.0.1 will return 3
  "
  (let (version)
    (dolist (elt grails-properties-by-version version)
      (if (string-match (car (cddr elt)) string)
          (setq version (car elt))))))

(defun grails-version-detect (file-path)
  "Try to detect the Grails version of the project from file-path.

   Grails 2 projects have an application.properties file.
   Grails 3 projects have a gradle.properties file.
  "
  (let (version)
    (dolist (elt grails-properties-by-version version)
      (let ((inside-path
             (concat (grails-app-base file-path) "../" (car (cdr elt))))
            (outside-path
             (concat (file-name-directory file-path) "/" (car (cdr elt)))))
        (cond ((file-readable-p inside-path)
               (setq version (grails-grep-version
                (util-string-from-file inside-path))))
              ((file-readable-p outside-path)
               (setq version (grails-grep-version
                (util-string-from-file outside-path)))))))))

(defun grails-dir-by-type-and-name (type class-name base-path)
  "Return the file path (string) for the type and the class-name.
  
   E.g. type='domain, class-name=User and base-path=/prj/grails-app/
        will output /prj/grails-app/domain/User.groovy
  "
  (concat
   base-path
   (car (cdr (assoc type grails-dir-name-by-type)))
   "/"
   class-name
   (car (cdr (assoc type grails-postfix-by-type)))))

(defun grails-extract-name (file-path grails-type)
  "Transform MyClassController.groovy to MyClass, 
   or my/package/MyClassController.groovy to my/package/MyClass.
  "
  (cond ((eq grails-type 'controller)
         (let ((end (string-match "Controller\.groovy" file-path)))
           (substring file-path 0 end)))
        ((eq grails-type 'domain)
         (let ((end (string-match "\.groovy" file-path)))
           (substring file-path 0 end)))
        ((eq grails-type 'view)
         (error "Jumping from views isn't supported"))
        ((eq grails-type 'service)
         (let ((end (string-match "Service\.groovy" file-path)))
           (substring file-path 0 end)))
        (t (error "Grails type not recognized"))))

(defun grails-clean-name (file)
  "Detect current file type and extract it's clean class-name.

  E.g. ~/prj/grails-app/controllers/UserController.groovy
  will results in User

  Or ~/grails-app/domain/pkg/User.groovy
  will results in pkg/User
  "
  (string-match "\\(^.*/grails-app/\\)\\([a-zA-Z]+\\)/\\(.*\\)" file)
  (let ((base-path (match-string 1 file))
        (dir-type (match-string 2 file))
        (file-path (match-string 3 file)))
    (let ((grails-type (car (rassoc (cons dir-type '()) grails-dir-name-by-type))))
      (if grails-type
          (grails-extract-name file-path grails-type)
        (error "Type not recognized")))))

(defun grails-app-base (path)
  "Get the current grails app base path /my/abs/path/grails-app/ if exist, else nil"
  (let ((start (string-match "^.*/grails-app/" path)))
    (if start
	(substring path 0 (match-end 0))
      () ;; if this is not a grails app return nil
      )))

(defun grails-find-file-auto (grails-type current-file)
  "Generate the corresponding file path for the current-file and grails-type.

   grails-type is a symbol (e.g. 'domain, 'controller, 'service)
   current-file is a file path
      
   E.g. (grails-find-file-auto 
          'domain'
          '~/prj/grails-app/controllers/UserController.groovy')
   Will output: '~/prj/grails-app/domain/User.groovy'

  "
  (let ((base-path (grails-app-base current-file))
	(class-name (grails-clean-name current-file)))
    (grails-dir-by-type-and-name grails-type class-name base-path)))

;;
;;
;; INTERACTIVE FUNCTIONS
;;
;;

(defun grails-version ()
  "Show Grails version for the project in the minibuffer"
  (interactive)
  (let ((version (grails-version-detect (buffer-file-name))))
    (if version
        (message (concat "Grails " (number-to-string version)))
      (error "Grails version not found"))))

(defmacro grails-fun-gen-from-file (grails-type)
  (let ((funsymbol (intern (concat "grails-" (symbol-name grails-type) "-from-file"))))
    `(defun ,funsymbol () (interactive) (switch-to-buffer
					 (find-file-noselect
					  (grails-find-file-auto
					   ',grails-type (buffer-file-name)))))))

(defmacro grails-fun-gen-from-name (grails-type)
  (let ((funsymbol (intern (concat "grails-" (symbol-name grails-type) "-from-name"))))
    `(defun ,funsymbol () (interactive)
	    (let ((x
		   (read-file-name
		    "Enter file name:"
		    (concat
		     (grails-app-base (buffer-file-name))
		     ,(concat (car (cdr (assoc grails-type grails-dir-name-by-type)))  "/")))))
	      (switch-to-buffer
	       (find-file-noselect x))))))

(defun grails-key-map ()
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c - d") (grails-fun-gen-from-file domain))
    (define-key keymap (kbd "C-c - c") (grails-fun-gen-from-file controller))
    (define-key keymap (kbd "C-c - s") (grails-fun-gen-from-file service))
    (define-key keymap (kbd "C-c - n d") (grails-fun-gen-from-name domain))
    (define-key keymap (kbd "C-c - n c") (grails-fun-gen-from-name controller))
    (define-key keymap (kbd "C-c - n s") (grails-fun-gen-from-name service))
    (define-key keymap (kbd "C-c - n v") (grails-fun-gen-from-name view))
    keymap))

;;;###autoload
(define-minor-mode grails
  "Grails minor mode.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode.
     When Grails minor mode is enabled you have some
     shortcut to fast navigate a Grails project."
  :init-value nil
  :lighter " Grails"
  :keymap (grails-key-map)
  :group 'grails)

(provide 'grails)

;;; grails.el ends here
