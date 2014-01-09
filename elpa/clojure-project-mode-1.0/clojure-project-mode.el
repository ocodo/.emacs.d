;;; clojure-project-mode.el --- Extends project-mode for Clojure projects
;;
;; Copyright 2011-2012 Benjamin Cluff
;;
;; Author: Benjamin Cluff <psyllo@gmail.com>
;; URL: https://github.com/psyllo/emacsenations
;; Created: 03-Nov-2011
;; Version: 1.0
;; Package-Requires: ((project-mode "1.0"))
;;
;; Synopsis: Extends project-mode for Clojure specific needs.
;;
;; Installation:
;; (require 'clojure-project-mode)
;;

(require 'project-mode)

(defgroup clojure-project-mode nil
  "Clojure project mode helps when working with clojure code projects."
  :prefix "clojure-project-"
  :group 'programming)

(define-minor-mode clojure-project-mode
  "Toggle project-project mode.
   With no argument, this command toggles the mode.
   Non-null prefix argument turns on the mode.
   Null prefix argument turns off the mode."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " CljPrj"
  ;; This mode is best as a global minor mode
  :global t
  ;; The minor mode bindings.
  :keymap
  '(([C-f10] . clojure-project-find-test-file-for-current))
  :group 'clojure-project-mode)

;;; Hooks
(add-hook 'project-mode-hook 'clojure-project-mode-toggle)

(defcustom project-tags-form-clojure
  '(".clj$"
    ('elisp ("(def[a-z0-9$?<>+*!_-]*[ \r\n\t]+\\([a-z0-9$?<>+*!_-]+\\)" 0)))
  "Gets added to `PROJECT-TAGS-FORM-DEFAULT' used for tags generation."
  :group 'clojure-project-mode)

(defun clojure-project-core-name-for-file (file-name)
  (project-file-basename
   (substring file-name 0
              (string-match "\\([_-]test\\.clj\\|\\.clj\\)"
                            file-name))))

(defun clojure-project-find-test-file-for (file-arg)
  (let ((core-name (clojure-project-core-name-for-file file-arg)))
    (dolist (file (project-path-cache-get (project-current)))
      (when (and (string-equal core-name (clojure-project-core-name-for-file file))
                 (project-dir-in-file-path-p file "test"))
        (message (concat "Found test file '" (project-file-basename file)
                         "' for '" (project-file-basename file-arg) "'"))
        (return file)))))

(defun clojure-project-find-test-file-for-current nil
  (interactive)
  (project-ensure-current)
  (let ((file (clojure-project-find-test-file-for (buffer-file-name))))
    (if file
        (find-file file)
      (message (concat "Could not find unit test for the current buffer.")))))

(defun clojure-project-mode-menu nil
  (interactive)
  (if (not clojure-project-mode)
      (global-unset-key [menu-bar cljprojmenu])
    (progn
      (define-key-after
        global-map
        [menu-bar cljprojmenu]
        (cons "CljPrj" (make-sparse-keymap))
        'tools)

      (define-key
        global-map
        [menu-bar cljprojmenu cljprjunit9z]
        '("Find Test File For Current Buffer" . clojure-project-find-test-file-for-current))))
  nil)

(defun clojure-project-mode-toggle nil
  ;; Note the condition. It's to sync enabled/disabled states between
  ;; the two modes in case clojure-project-mode is enabled/disabled
  ;; directly instead of through `PROJECT-MODE-HOOK'.
  (when (not (eq clojure-project-mode project-mode))
   (clojure-project-mode)
   (clojure-project-mode-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run once
(project-add-to-tags-form (project-current)
                          (first project-tags-form-clojure)
                          (second project-tags-form-clojure))
(project-add-directories-to-search-exclusion-regexes '("lib" "classes" ".cake"))
(project-add-to-search-exclusion-regexes '("\\.lein-failures"))

(provide 'clojure-project-mode)

;;; clojure-project-mode.el ends here
