;;; opl-coffee --- Coffee Script helpers for OpsManager Emacsen
;;; Author: Jason Milkins <jason@opsmanager.com>
;;; Commentary:
;;
;;  A collection of useful tools and helpers for working with OpsManager CoffeeSource
;;
;;; Code:

;;;###autoload
(defun opl-coffee-switch-to-related (regexp replace from to)
  "Using the current file, open a related file.

Uses REGEXP and REPLACE to generate the new filename.

FROM and TO are used for labelling the error messages.

For example FROM `view-model' TO `template'.

Throws an error if the buffer file name doesn't match with REGEXP or
if the target filename generated doesn't exist.

When prefixed with universal argument, the target file is created
if it doesn't exist."

  (let* ((filename (buffer-file-name))
         (target-filename (replace-regexp-in-string regexp replace filename)))
    (message "Switch from %S to %S" filename target-filename)
    (unless (string-match regexp filename)
      (error "The current file does not appear to be a %s" from))
    (unless (or (file-exists-p target-filename) current-prefix-arg)
      (error "The %s you are looking for is either missing or not conventionally named" to))
    (find-file target-filename)))

;;;###autoload
(defun opl-rxdotfix (s)
"Escape dots for regexp in S."
  (replace-regexp-in-string "\\." "\\\\." s))

;;;###autoload
(defun opl-switcher (src src-ext dest dest-ext from to)
  "Build an opl file switching defun.

Use SRC, SRC-EXT and DEST, DEST-EXT (folder names & file extensions).

FROM and TO are used to generate a named interactive function
and for error messages.  See `opl-coffee-switch-to-related`."

  (defalias (intern (concat "opl-coffee-switch-from-" from "-to-" to))
    `(lambda ()
      (interactive)
      (opl-coffee-switch-to-related
       (concat "\\(^.*\\)\\(" (opl-rxdotfix ,src)
               "\\)\\(.*\\)\\(" (opl-rxdotfix ,src-ext)
               "\\)")
       (concat "\\1" ,dest
               "\\3" ,dest-ext)
       ,from ,to))))

(dolist (switcher

         (list
;;        from folder     .ext              to folder     .ext              from name    to name
;;        ------------------------------------------------------------------------------------------
          '("templates"   ".hamlc"          "view_models" ".js.coffee"      "template"   "view-model")
          '("templates"   ".hamlc"          "views"       "_view.js.coffee" "template"   "view"      )
          '("view_models" ".js.coffee"      "templates"   ".hamlc"          "view-model" "template"  )
          '("view_models" ".js.coffee"      "views"       "_view.js.coffee" "view-model" "view"      )
          '("views"       "_view.js.coffee" "view_models" ".js.coffee"      "view"       "view-model")
          '("views"       "_view.js.coffee" "templates"   ".hamlc"          "view"       "template"  )))

  (apply 'opl-switcher switcher))

(provide 'opl-coffee)
;;; opl-coffee ends here
