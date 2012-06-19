;; Dropins
;;
;; This extension provides a simple way to load a set of libraries
;; by placing them in a named folder.  This is similar to the site-lisp
;; folder in the Emacs directory, however, Dropins defaults to a folder 
;; within the user HONE.
;;
;; ~/.emacs.d/dropins/
;;
;; is the default Library dropin folder, place your 
;; dropin .el libraries in here, also place .elc's in this 
;; folder but a corresponding .el must be available.
;; Otherwise it won't load.
;;
;; note that ELPA http://tromey.com/elpa/install.html would 
;; be the best way to install libraries. But they must be
;; packaged properly.
;; 
;; Dropins is really intended for small extensions, for example
;; color-themes or binding setups.
;;

(require 'cl)

(defgroup dropins nil
  "Library dropin folder"
  :group 'convenience
)

(defcustom dropin-folder "~/.emacs.d/dropins" 
  "Library dropin folder, place your dropin .el libraries 
   in this folder. You can also place .elc's in this folder
   but if there is not corresponding .el it will not load.
   
  note that ELPA http://tromey.com/elpa/install.html would 
  be the best way to install libraries. But they must be 
  packaged properly."

  :group 'dropins
  :type 'string
)

(defvar dropin-folder 'dropin-folder
  "Library dropin folder, place your dropin .el libraries 
   in this folder. You can also place .elc's in this folder
   but if there is not corresponding .el it will not load.

   note that ELPA http://tromey.com/elpa/install.html would 
   be the best way to install libraries. But they must be
   packaged properly.")

(defun load-dropins () ""
  (interactive)
    (loop for entry in (directory-files dropin-folder t)
	  when (equal ".el" (substring entry -3))
	  ;; do regexp match and grab result substring
	  do (load-library (car (split-string (car (reverse (split-string entry "/" ))) "\\.")))
	  )
    )

(provide 'dropins)

