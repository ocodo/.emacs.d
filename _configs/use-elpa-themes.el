(require 'dash)
;; Custom themes from elpa/melpa/marmalade added to load-path
;; When they don't do it themeselves...
(-each
    (-map
     (lambda (item)
       (format "~/.emacs.d/elpa/%s" item))
     (-filter
      (lambda (item) (s-contains? "theme" item))
      (directory-files "~/.emacs.d/elpa")))
  (lambda (item)
    (add-to-list 'custom-theme-load-path item)))

(provide 'use-elpa-themes)
