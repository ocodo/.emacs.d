;; Dropins
;;
;; This extension provides a simple way to load a set of libraries
;; by placing them in a named folder.  This is similar to the site-lisp
;; folder in the Emacs directory, however, Dropins defaults to a folder 
;; within the user HONE.
;; 
;; Just place your dropin libraries in a folder and use
;; M-x customize-group RET dropins to point to that folder.
;;  
;; NOTE that ELPA http://tromey.com/elpa/install.html would 
;; be the best way to install libraries. But libraries must be 
;; packaged specifically for ELPA to work.
;; 
;; Dropins is really intended for small extensions, for example
;; color-themes or binding setups.
;;

(defgroup dropins nil
  "Library dropin folder"
  :group 'convenience
)

(defcustom dropin-folder "~/.emacs.d/dropins" 
  "Library dropin folder, place your dropin .el libraries in this folder. 
   
  note that ELPA http://tromey.com/elpa/install.html would 
  be the best way to install libraries. But they must be 
  packaged properly."

  :group 'dropins
  :type 'string
)

(defvar dropin-folder 'dropin-folder
  "Library dropin folder, place your dropin .el libraries in this folder. note that ELPA http://tromey.com/elpa/install.html would be the best way to install libraries. But they must be packaged properly.")

(defun load-dropins () ""
  (interactive)
    (loop for entry in (directory-files dropin-folder t)
	  when (equal ".el" (substring entry -3))
	  do (load-library entry)
	  )
    )

(load-dropins)
(provide 'dropins)
