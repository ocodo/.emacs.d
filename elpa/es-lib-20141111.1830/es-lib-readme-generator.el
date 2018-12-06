;;; es-lib-readme-generator.el --- For internal use, and to satisfy curiosity. -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'loadhist)
(require 'apropos)

(defun es-analyze-feature-loadhist (feature)
  (let* (( all-syms (feature-symbols feature))
         ( funs-raw
           (mapcar 'cdr
                   (cl-remove-if-not
                    (lambda (thing)
                      (and (consp thing)
                           (eq (car thing)
                               'defun)))
                    all-syms)))
         ( func-aliases
           (prog1 (cl-remove-if-not
                   'symbolp
                   funs-raw
                   :key 'symbol-function)
             (setq funs-raw
                   (cl-remove-if-not
                    'consp
                    funs-raw
                    :key 'symbol-function))))
         ( funs (cl-remove-if 'apropos-macrop funs-raw))
         ( commands (prog1 (cl-remove-if-not 'commandp funs)
                      (setq funs (cl-remove-if 'commandp funs))))
         ( macros (cl-remove-if-not 'apropos-macrop funs-raw))
         ( vars (cl-remove-if-not 'symbolp all-syms)))
    (list :defuns-ni funs
          :commands commands
          :macros macros
          :defvars vars
          :func-aliases func-aliases)))

(defun es--type-name (type)
  (cl-case type
    (:defuns-ni "Non-interactive")
    (:commands "Commands")
    (:defvars "Defvars")
    (:macros "Macros")
    (:func-aliases "Aliases")))

(defun es-lib-features ()
  (let ((libs (mapcar (es-comp 'intern 'file-name-base)
                      (directory-files
                       (es-emacs-path "site-lisp/my-scripts/es-lib")
                       nil "^es-"))))
    (cl-remove-if (lambda (featch)
                 (memq featch
                       '(es-lib-readme-generator
                         es-lib)))
               libs)))

(defun es-toc (features)
  (with-temp-buffer
    (insert "\n")
    (cl-dolist (feature features)
      (insert
       (format "* [%s](#%s)\n"
               feature feature)))
    (insert "\n")
    (buffer-string)))

(let ((total-items 0))
  (defun es-feature-report (feature)
    (es-reset-feature feature)
    (let (( analyzed
            (mapcar (lambda (thing)
                      (if (consp thing)
                          (cl-remove-if
                           (lambda (sym-nam)
                             (or (equal "aai-mode" sym-nam)
                                 (cl-search "--" sym-nam)))
                           thing
                           :key 'symbol-name)
                          thing))
                    (es-analyze-feature-loadhist feature))))
      (with-temp-buffer
        (insert "\n## " (symbol-name feature) "\n\n")
        (cl-dolist (type '(:defvars :macros :commands :defuns-ni))
          (unless (zerop (length (cl-getf analyzed type)))
            (insert "\n#### " (es--type-name type) ":\n\n")
            (cl-dolist (item (cl-sort (cl-getf analyzed type) 'string< :key 'symbol-name))
              (insert "* " (symbol-name item) "\n")
              (cond ( (eq type :defvars)
                      (when (es-var-documentation item)
                        (insert "\n```\n"
                                (es-var-documentation item)
                                "\n```\n\n")))
                    ( (documentation item)
                      (insert "\n```\n"
                              (documentation item)
                              "\n```\n\n")))
              (cl-incf total-items))))
        (buffer-string))))

  (defun es-lib-report ()
    (let* (( libs (es-lib-features)))
      (setq total-items 0)
      (with-temp-buffer
        (cl-dolist (feature libs)
          (insert (es-feature-report feature)))
        (buffer-string))))

  (defun es-lib-make-readme ()
    (interactive)
    (let ((index (es-lib-report)))
      (save-excursion
        (save-window-excursion
          (find-file (es-emacs-path "site-lisp/my-scripts/es-lib/README.md"))
          (erase-buffer)
          (insert (format "#es-lib
A collecton of emacs utilities, and basis for several of my packages. Here are some highlights:

#### Packages:

* **es-lib-duplicate:**
  Functions duplicating the current region
* **es-lib-total-line:**
  Functions for comfortably moving with folded lines
* **es-lib-number-at-point:**
  Functions for manipulating the number at point.

#### Functions:

* **es-ack-replace-symbol:**
  A refactoring tool, with help of which this library was assembled
* **es-ido-like-helm:**
  Choose from a concatenated list of buffers and recent files. I have it bound to `<menu>`.

# Index:

_Auto-generated before each commit. Total items in the library: %s_

#### Table of contents:
%s
%s"
                          total-items
                          (es-toc (es-lib-features))
                          index))
          (save-buffer))))))

(provide 'es-lib-readme-generator)
;; es-lib-readme-generator.el ends here
