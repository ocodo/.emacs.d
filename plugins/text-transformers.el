;;; text-transformers --- A collection of text-transformers
;;; Commentary:
;;; Code:

(require 's)

(defun snake-case-at-point-or-region ()
  "Snake_case the current word or text selection."
  (interactive)
  (operate-on-point-or-region 's-snake-case))

(defun dasherise-at-point-or-region ()
  "Dasherise-the-current CamelCase or snake_case word or text selection."
  (interactive)
  (operate-on-point-or-region 's-dashed-words))

(defun upper-camelcase-at-point-or-region ()
  "UpperCamelCaseTheCurrent dashed-or-snake_case_words or any words in text selection."
  (interactive)
  (operate-on-point-or-region 's-upper-camel-case))

(defun lower-camelcase-at-point-or-region ()
  "LowerCamelCaseTheCurrent dashed or snake_case word or any words in text selection."
  (interactive)
  (operate-on-point-or-region 's-lower-camel-case))

(defun humanize-at-point-or-region ()
  "Humanize variable names, insert spaces instead of - or _ or un-CamelCase humps to spaced words."
  (interactive)
  (operate-on-point-or-region 's-capitalized-words))

(defun titleized-at-point-or-region ()
  "Convert snaked, dashed, underscored, camelcase, or spaced words in region to Title Case."
  (interactive)
  (operate-on-point-or-region 's-titleized-words))

(defun url-encode-string-at-point ()
  "URL Encode the current string at point."
  (interactive)
  (operate-on-point-or-region 'url-encode-url))

(defun operate-on-point-or-region (fn)
  "With the current region or string at point.
Apply this  to FN.  Replace with the result."
  (let (pos1 pos2 meat excerpt)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'symbol))
            pos2 (cdr (bounds-of-thing-at-point 'symbol))))
    (setq excerpt (buffer-substring-no-properties pos1 pos2))
    (setq meat (funcall fn excerpt))
    (delete-region pos1 pos2)
    (insert  meat)))


;; Case transform hydra
(bind-key "C-c C-x t"
          (defhydra case-transform (:color blue)
            "Transform case of word or region"
            ("h" humanize-at-point-or-region "Humanize text")
            ("d" dasherise-at-point-or-region "dasherise/kebab")
            ("u" upper-camelcase-at-point-or-region "UpperCamel")
            ("l" lower-camelcase-at-point-or-region "lowerCamel")
            ("s" snake-case-at-point-or-region "snake_underscore")
            ("t" titleized-at-point-or-region "Titleized")
            ("U" url-encode-string-at-point "URL encode")))

(provide 'text-transformers)

;;; text-transformers.el ends here
