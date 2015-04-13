;;; opl-coffee --- Coffee Script helpers for OpsManager Emacsen
;;; Author: Jason Milkins <jason@opsmanager.com>
;;; Commentary:
;;
;;  A collection of useful tools and helpers for working with OpsManager CoffeeSource
;;
;;  - Visit a template for a viewmodel
;;  - Visit a viewmodel for a template
;;
;;; Code:

;;;###autoload
(defun opl/coffee:switch-to-related (from-file-rx to-replace type-from type-to)
  "Generalised function to find a related file in the same structure."
  (let* ((filename (buffer-file-name))
         (template-filename (replace-regexp-in-string from-file-rx to-replace filename)))
    (unless (string-match from-file-rx filename)
      (error "The current file does not appear to be a %s" type-from))
    (unless (file-exists-p template-filename)
      (error "The %s you are looking for is either missing or not conventionally named" type-to))
    (find-file template-filename)))

;;;###autoload
(defun opl/coffee:get-template-for-viewmodel ()
  "Get the template for the viewmodel."
  (interactive)
  (opl/coffee:switch-to-related "\\(^.*\\)\\(view_models\\)\\(.*\\)\\(\\.js\\.coffee\\)" "\\1templates\\3.hamlc"
                                "view-model" "template"))

;;;###autoload
(defun opl/coffee:get-viewmodel-for-template ()
  "Get the viewmodel for the template."
  (interactive)
  (opl/coffee:switch-to-related "\\(^.*\\)\\(templates\\)\\(.*\\)\\(\\.hamlc\\)" "\\1view_models\\3.js.coffee"
                                "template" "view-model"))

(provide 'opl-coffee)

;;; opl-coffee ends here
