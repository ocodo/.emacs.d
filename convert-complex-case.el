;; shell-command-on-region-replace
;; shell-command-output-to-buffer

(require 'thingatpt)


(defun un-camelcase-to-dasherize ()
  ""
  (interactive)
  (insert (un-camelcase-string 
   (thing-at-point 'symbol) 
   ))
  ) 
    
    




(defun un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
    Default for SEP is a hyphen \"-\".
    If third argument START is non-nil, convert words after that
    index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s
            (replace-match 
             (concat (or sep "-") 
                     (downcase (match-string 0 s))) 
             t nil s)))
    (downcase s)))


(defun l (s) 
  "Quick message logger"
  (progn
    (message (format "%s" s))))
