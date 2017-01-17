;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'gnu-apl-util)
(require 'gnu-apl-network)

(cl-defmacro gnu-apl--with-temp-files (files &body body)
  (declare (indent 1))
  (if files
      (let ((f (car files))
            (temp-file-name (gensym)))
        `(let ((,temp-file-name (make-temp-file ,(cadr f))))
           (let ((,(car f) ,temp-file-name))
             (unwind-protect
                 (gnu-apl--with-temp-files ,(cdr files)
                   ,@body)
               (delete-file ,temp-file-name)))))
    `(progn ,@body)))

(cl-defmacro gnu-apl--define-variable-reading-function ((fun-name varname) &body body)
  (declare (indent 1))
  (let ((result-sym (gensym "result-")))
    `(defun ,fun-name (,varname)
       (interactive (list (gnu-apl--choose-variable "Variable" :variable (gnu-apl--name-at-point))))
       (gnu-apl--send-network-command (concat "getvar:" ,varname))
       (let ((,result-sym (gnu-apl--read-network-reply-block)))
         (unless (string= (car ,result-sym) "content")
           (error "Unable to read variable. Response: %s" (car result)))
         (let ((,varname (car (read-from-string (apply #'concat (cdr ,result-sym))))))
           ,@body)))))

(defun gnu-apl--single-dimension-p (value)
  "Returns non-nil if VALUE is a single-dimension array returned
from the APL runtime"
  (and (listp value)
       (or (numberp (car value))
           (listp (car value)))))

(defun gnu-apl--plot-insert-cell (entry)
  (etypecase entry
    (integer (insert (format "%d" entry)))
    (number (insert (format "%f" entry)))
    (string (insert (format "\"%s\"" entry)))
    (list (cond ((listp (car entry))
                 (error "Cell contains array"))
                ((eq (car entry) :unicode)
                 (insert (char-to-string (cadr entry))))
                (t
                 (error "Unknown cell content: %S" entry))))))

(defun gnu-apl--cell-value-as-string (value)
  (cond ((stringp value)
         (format "\"%s\"" value))
        ((integerp value)
         (format "%d" value))
        ((numberp value)
         (format "%f" value))
        ((and (listp value) (eq (car value) :unicode))
         (format "\"%s\"" (char-to-string (cadr value))))
        (t
         (error "Unable to convert value of type %s to string" (symbol-name (car value))))))

(defun gnu-apl--write-array-content-to-csv (content)
  (cond ((gnu-apl--single-dimension-p content)
         (loop for value in content
               do (progn
                    (insert (gnu-apl--cell-value-as-string value))
                    (insert "\n")))
         (list (length content) 1))
        ((and (listp content) (eq (car content) :vector))
         (let ((size (cadr content)))
           (unless (= (length size) 2)
             (error "Unexpected dimensions: %d" (length size)))
           (loop for row-value in (caddr content)
                 do (loop for col-content in row-value
                          for first = t then nil
                          when (not first)
                          do (insert " ")
                          do (insert (gnu-apl--cell-value-as-string col-content)))
                 do (insert "\n"))
           size))
        (t
         (error "Unable to write variable of this type"))))

(defun gnu-apl-dump-variable-csv (varname filename)
  "Exports the array stored in the APL variable named by VARNAME
to CSV format and save it to the file name FILENAME. Returns the
dimension of the exported data as a list of the form (ROWS COLS)"
  (interactive (list (gnu-apl--choose-variable "Variable" :variable (gnu-apl--name-at-point))
                     (read-file-name "Output filename: ")))
  (gnu-apl--send-network-command (concat "getvar:" varname))
  (let ((result (gnu-apl--read-network-reply-block)))
    (unless (string= (car result) "content")
      (error "Error from runtime"))
    (let ((value (car (read-from-string (apply #'concat (cdr result))))))
      (with-temp-buffer
        (prog1
            (gnu-apl--write-array-content-to-csv value)
          (write-file filename))))))

(gnu-apl--define-variable-reading-function (gnu-apl-plot-line result)
  (gnu-apl--with-temp-files ((script-file "script")
                             (data-file "data"))
    (let ((size (with-temp-buffer
                  (prog1
                      (gnu-apl--write-array-content-to-csv result)
                    (write-file data-file)))))
      (with-temp-buffer
        (insert "plot ")
        (let ((numcols (cadr size)))
          (dotimes (n numcols)
            (insert (format "\"%s\" using %d title \"Col %d\" with lines" data-file (1+ n) (1+ n)))
            (when (< n (1- numcols))
              (insert ",\\"))
            (insert "\n")))
        (write-file script-file)))
    (shell-command (format "%s -p %s" gnu-apl-gnuplot-program script-file))))

(provide 'gnu-apl-plot)
