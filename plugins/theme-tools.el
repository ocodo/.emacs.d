;;; theme-tools -- A collection of tools to assist with the writing of themes
;;; Package-Requires: (s "1.10.0") (dash "2.13.0")
;;; Version: 0.1.1
;;; Author: Jason Milkins
;;; Commentary:
;;
;;  A collection of tools to assist writing themes

;;; code:
(require 's)
(require 'dash)

(defvar theme-tools-base-attributes '(:background :foreground :weight :underline :overline :box :family :height :inherit))

(defun theme-tools-face-themed-p (face)
  "Return t if the FACE is themed."
  (get face 'theme-face))

(defun theme-tools-theme-buffer-p (&optional buffer)
  "Return t if the BUFFER is a theme file.

Default to the current BUFFER."
  (unless buffer (setq buffer (current-buffer)))
  (and
   (s-ends-with? "theme" (file-name-base (buffer-file-name)))
   (s-contains? "(custom-theme-set-faces" (buffer-string))))

(defun theme-tools-faces-in-open-theme ()
  "List of the faces defined in the open theme."
  (save-mark-and-excursion
    (if (theme-tools-theme-buffer-p)
        (progn (goto-char (point-min))
               (when (re-search-forward "(custom-theme-set-faces")
                 (goto-char (match-beginning 0))
                 (mark-sexp)
                 (-flatten
                  (--map
                   (cdr it)
                   (s-match-strings-all
                    "^ *`(\\(.*?\\) "
                    (buffer-substring-no-properties
                     (region-beginning)
                     (region-end)))))))
      (error "Not in a theme file"))))

(defun theme-tools-face-in-theme-open-p (face)
  "Return t if the FACE is in the theme open for editing."
  (let ((found-face (--find (string= face it) (theme-tools-faces-in-open-theme))))
    found-face))

(defun theme-tools-formatted-face-attribute (face attribute)
  "Return a string with the FACE ATTRIBUTE value.
Formatted as \":attribute value\".
Return an empty string when the value is unspecified."
  (let ((value (face-attribute face attribute)))
    (if (not (string= value "unspecified"))
        (format "%s %S" attribute value)
      "" ;; else blan
      )))

(defun theme-tools-formatted-face-attributes (face attributes)
  "Given a FACE and ATTRIBUTES list.
Return formatted attribute name value pairs for all the attributes."
  (concat
   (s-trim
    (s-collapse-whitespace
     (s-join
      " "
      (mapcar
       (lambda (attribute)
         (theme-tools-formatted-face-attribute face attribute))
       attributes))))))

(defun theme-tools-theme-face-attributes (face attributes)
  "Return a FACE and ATTRIBUTES formatted for a theme definition.
This is useful for inserting the current values of a face into a theme for editing."
  (format "`(%s ((t (%s))))" face (theme-tools-formatted-face-attributes face attributes)))

(defun theme-tools-list-faces-with-prefix (face-prefix)
  "Return a list containing faces which have FACE-PREFIX."
  (--filter
   (s-starts-with?
    face-prefix (symbol-name it))
   (face-list)))

(defun theme-tools-list-unthemed-faces (face-prefix)
  "Return a list containing unthemed faces which have FACE-PREFIX."
  (--remove
   (theme-tools-face-themed-p it)
   (theme-tools-list-faces-with-prefix
    face-prefix)))

(defun theme-tools-list-faces-not-in-this-theme-file (face-prefix)
  "Return a list containing faces which have FACE-PREFIX not defined in this theme file."
  (let ((prefix-faces
         (--filter
          (s-starts-with? face-prefix (symbol-name it))
          (theme-tools-faces-in-open-theme))))
    (--remove
     (theme-tools-face-in-theme-open-p it)
     prefix-faces)))

(defun theme-tools-insert-theme-face-attributes (face)
  "Insert theme definition of a FACE.

Use `theme-tools-base-attributes' to customize the face attributes that are given."
  (interactive
   (list (completing-read "Select face: " (face-list))))
  (insert
   (theme-tools-theme-face-attributes
    (intern face)
    theme-tools-base-attributes)))

(defun helm-theme-tools-insert-faces-as-theme-attributes ()
  "Select faces using Helm, and insert them as faces in a theme.

For example, if you want to theme all the GNUS faces, you can
easily select them and then edit them from their defaults.

\(use C-spc to mark multiple faces\)"
  (interactive)
  (helm
   :sources
   `(((name . "HELM")
      (candidates . ,(face-list))
      (
       action
       . (("open" .
           (lambda (candidate)
             (mapc
              (lambda (face)
                (theme-tools-insert-theme-face-attributes face)
                (newline))
              (helm-marked-candidates))))))))))

(defun theme-tools-insert-theme-faces-not-themed (prefix)
  "Insert theme face defintion for unthemed faces with PREFIX."
  (interactive "sFace prefix: ")
  (let ((unthemed
         (theme-tools-list-unthemed-faces prefix)))
    (--each unthemed
      (theme-tools-insert-theme-face-attributes
       (symbol-name it))
      (newline))))

(defun theme-tools-insert-theme-faces-not-in-open-theme (prefix)
  "Insert theme face defintion for faces with PREFIX not in the theme open in the current buffer."
  (interactive "sFace prefix: ")
  (let ((not-in-theme
         (--remove
          (theme-tools-face-in-theme-open-p it)
          (theme-tools-list-faces-with-prefix prefix))))
    (--each not-in-theme
      (theme-tools-insert-theme-face-attributes
       (symbol-name it))
      (newline))))

(provide 'theme-tools)
;;; theme-tools.el ends here
