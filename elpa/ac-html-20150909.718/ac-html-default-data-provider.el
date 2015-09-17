(require 'ac-html-core)
(require 'f)

;;; web-completion-data helpers

(defconst web-completion-data-package-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The directory where `web-completion-data' package exists.")

(defconst web-completion-data-html-source-dir
  (expand-file-name "completion-data" web-completion-data-package-dir)
  "The directory where basic completion source of `web-completion-data'
 exists.")

(defconst web-completion-data-tag-list-file
  (f-expand "html-tag-list" web-completion-data-html-source-dir))

(defconst web-completion-data-tag-doc-dir
  (f-expand "html-tag-short-docs" web-completion-data-html-source-dir))

(defun web-completion-data-tag-doc-file (tag)
  (f-expand tag web-completion-data-tag-doc-dir))

(defconst web-completion-data-attr-list-dir
  (f-expand "html-attributes-list" web-completion-data-html-source-dir))

(defconst web-completion-data-attr-global-list-file
  (f-expand "global" web-completion-data-attr-list-dir))

(defun web-completion-data-attr-list-file (tag)
  (f-expand tag web-completion-data-attr-list-dir))

(defconst web-completion-data-attr-doc-dir
  (f-expand "html-attributes-short-docs" web-completion-data-html-source-dir))

(defun web-completion-data-attr-global-doc-file (attr)
  (f-expand (format "global-%s" attr) web-completion-data-attr-doc-dir))

(defun web-completion-data-attr-doc-file (tag attr)
  (f-expand (format "%s-%s" tag attr) web-completion-data-attr-doc-dir))

(defconst web-completion-data-attrv-list-dir
  (f-expand "html-attrv-list" web-completion-data-html-source-dir))

(defun web-completion-data-attrv-list-file (tag attr)
  (f-expand (format "%s-%s" tag attr) web-completion-data-attrv-list-dir))

(defun web-completion-data-attrv-global-list-file (attr)
  (f-expand (format "global-%s" attr) web-completion-data-attrv-list-dir))

(defconst web-completion-data-attrv-doc-dir
  (f-expand "html-attrv-docs" web-completion-data-html-source-dir))

(defun web-completion-data-attrv-global-doc-file (attr attrv)
  (f-expand (format "global-%s-%s" attr (url-hexify-string attrv))
            web-completion-data-attrv-doc-dir))

(defun web-completion-data-attrv-doc-file (tag attr attrv)
  (f-expand (format "%s-%s-%s" tag attr (url-hexify-string attrv))
            web-completion-data-attrv-doc-dir))

;;; cached data

(defvar ac-html--tags-list nil "The list of tags.")
(defvar ac-html--global-attributes nil "The list of global attrs.")
(defvar ac-html--cached-attributes-alist nil)

;;; helper functions

(defun ac-html--load-list-from-file (filepath)
  "Return a list separated by \\n from FILEPATH."
  (if (file-exists-p filepath)
      (with-current-buffer (find-file-noselect filepath)
        (unwind-protect
            (split-string
             (save-restriction
               (widen)
               (buffer-substring-no-properties (point-min) (point-max)))
             "\n" t)
          (kill-buffer)))
    nil))

(defun ac-html--read-file (file)
  "If file exist, return string of contents, otherwise return nil."
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string))
    nil))

;;; functions

(defun ac-html-default-tags ()
  (if ac-html--tags-list
      ac-html--tags-list
    (setq ac-html--tags-list
          (ac-html--load-list-from-file web-completion-data-tag-list-file))))

(defun ac-html-default-attrs (tag)
  (unless ac-html--global-attributes
    (setq ac-html--global-attributes
          (ac-html--load-list-from-file
           web-completion-data-attr-global-list-file)))
  (let (list attr-file)
    (setq attr-file (web-completion-data-attr-list-file tag))
    (if (file-exists-p attr-file)
        (setq list (ac-html--load-list-from-file
                    attr-file)))
    (append list ac-html--global-attributes)))

(defun ac-html-default-attrvs (tag attr)
  (append
   (ac-html--load-list-from-file
    (web-completion-data-attrv-list-file tag attr))
   (ac-html--load-list-from-file
    (web-completion-data-attrv-global-list-file attr))))

(defun ac-html-default-tag-doc (tag)
  (ac-html--read-file (web-completion-data-tag-doc-file tag)))

(defun ac-html-default-attr-doc (tag attr)
  (or (ac-html--read-file (web-completion-data-attr-doc-file tag attr))
      (ac-html--read-file (web-completion-data-attr-global-doc-file attr))))

(defun ac-html-default-attrv-doc (tag attr attrv)
  (or (ac-html--read-file (web-completion-data-attrv-doc-file tag attr attrv))
      (ac-html--read-file
       (web-completion-data-attrv-global-doc-file attr attrv))))

(ac-html-define-data-provider 'ac-html-default-data-provider
  :tag-func 'ac-html-default-tags
  :attr-func 'ac-html-default-attrs
  :attrv-func 'ac-html-default-attrvs
  :tag-doc-func 'ac-html-default-tag-doc
  :attr-doc-func 'ac-html-default-attr-doc
  :attrv-doc-func 'ac-html-default-attrv-doc)

(provide 'ac-html-default-data-provider)
;;; ac-html-default-data-provider.el ends here
