;;; opl-coffee --- Coffee Script helpers for OpsManager Emacsen
;;; Author: Jason Milkins <jason@opsmanager.com>
;;; Commentary:
;;
;;  A collection of useful tools and helpers for working with OpsManager CoffeeSource
;;
;;; Code:

;;;###autoload
(defun opl-coffee-switch-to-related (regexp replace to from)
  "Use REGEXP and REPLACE to generate a filename to open.
TO and FROM are used for labelling the error messages, for example
switching TO `template' FROM `view-model'.

Error out If the buffer file name doesn't match with REGEXP or
if the filename generated doesn't exist."
  (let* ((filename (buffer-file-name))
         (template-filename (replace-regexp-in-string regexp replace filename)))
    (unless (string-match regexp filename)
      (error "The current file does not appear to be a %s" from))
    (unless (file-exists-p template-filename)
      (error "The %s you are looking for is either missing or not conventionally named" to))
    (find-file template-filename)))

;;;###autoload
(defun opl-rxdotfix (s)
"Escape dots for regexp in S."
  (replace-regexp-in-string "\\." "\\\\." s))

;;;###autoload
(defun opl-switcher (src src-ext dest dest-ext to from)
  "Build an opl file switching defun.

Use SRC folder, SRC-EXT (file extension) and DEST folder DEST-EXT
file extension.

TO and FROM are used to generate the interactive function name
and for error messages.  See `opl-coffee-switch-to-related`."

  (defalias (intern (concat "opl-coffee-switch-to-" to "-from-" from))
    (lambda ()
      (interactive)
      (opl-coffee-switch-to-related
       (concat "\\(^.*\\)\\(" (opl-rxdotfix src)
               "\\)\\(.*\\)\\(" (opl-rxdotfix src-ext)
               "\\)")
       (concat "\\1" dest
               "\\3" dest-ext)
       to from))))

;;             from folder   - ext          to folder      - ext         to name       from name
(opl-switcher  "view_models"  ".js.coffee"  "templates"    ".hamlc"      "template"    "view-model")
(opl-switcher  "templates"    ".hamlc"      "view_models"  ".js.coffee"  "view-model"  "template")
(opl-switcher  "views"        ".js.coffee"  "view_models"  ".js.coffee"  "view-model"  "view")
(opl-switcher  "view_models"  ".js.coffee"  "views"        ".js.coffee"  "view"        "view-model")

(provide 'opl-coffee)
;;; opl-coffee ends here
