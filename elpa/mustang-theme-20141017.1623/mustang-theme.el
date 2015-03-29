;;; mustang-theme.el --- port of vim's mustang theme
;; Author: martin haesler
;; URL: http://github.com/mswift42/mustang-theme
;; Package-Version: 20141017.1623
;;; Version: 0.3

;; original vim theme by Henrique C.Alves
;;(http://hcalves.deviantart.com/art/Mustang-Vim-Colorspcheme-98974484)

(deftheme mustang)

(custom-theme-set-faces
  'mustang 
        '(default ((t (:background "#202020" :foreground "#e2e2e5"))))
        '(font-lock-builtin-face ((t (:foreground "#808080"))))
        '(region ((t (:background "#3c414c" :foreground "#faf4c6"))))
        '(highlight ((t (:background "#3c414c"))))
	'(hl-line ((t (:background "#393939"))))
	'(fringe ((t (:background "#232323" :foreground "#cfbfad"))))
	'(cursor ((t (:background "#626262"))))
        '(show-paren-match-face ((t (:background "#ff9800"))))
        '(isearch ((t (:bold t :foreground "#202020" :background "#e2e2e5"))))
        '(mode-line ((t (:bold t :foreground "#808080" :background "#202020"))))
        '(mode-line-inactive ((t (:foreground "#696969" :background "#202020"))))
        '(mode-line-buffer-id ((t (:bold t :foreground "#ff9800" :background "#202020"))))
        '(minibuffer-prompt ((t (:bold t :foreground "#708090"))))
        '(default-italic ((t (:italic t))))
	'(font-lock-comment-face ((t (:foreground "#808080"))))
	'(font-lock-negation-char-face ((t (:foreground "#ff9800"))))
	'(font-lock-reference-face ((t (:foreground "#ff9800"))))
        '(font-lock-comment-delimiter-face ((t (:foreground "#808080"))))
	'(font-lock-constant-face ((t (:foreground "#ff9800"))))
        '(font-lock-doc-face ((t (:foreground "#7e8aa2"))))
        '(font-lock-function-name-face ((t (:foreground "#ffffff"))))
        '(font-lock-keyword-face ((t (:bold t :foreground "#808080"))))
        '(font-lock-preprocessor-face ((t (:foreground "#ff9800"))))
        '(font-lock-reference-face ((t (:bold t :foreground "#808bed"))))
        '(font-lock-string-face ((t (:foreground "#b1d631"))))
        '(font-lock-type-face ((t (:foreground "#7e8aa2"))))
        '(font-lock-variable-name-face ((t (:foreground "#ff9800"))))
        '(font-lock-warning-face ((t (:foreground "#ffffff" :background "#ff6523"))))
	'(link ((t (:foreground "#ff9800"))))
	'(org-hide ((t (:foreground "#708090"))))
        '(org-level-1 ((t (:bold t :foreground "#808080" :height 1.1))))
        '(org-level-2 ((t (:bold nil :foreground "#7e8aa2" :height 1.1))))
        '(org-level-3 ((t (:bold t :foreground "#df9f2d" :height 1.1))))
        '(org-level-4 ((t (:bold nil :foreground "#af4f4b" :height 1.0))))
        '(org-date ((t (:underline t :foreground "#f0ad6d") :height 1.1)))
        '(org-footnote  ((t (:underline t :foreground "#ad600b"))))
        '(org-link ((t (:underline t :foreground "#ff9800" ))))
        '(org-special-keyword ((t (:foreground "#ff9800"))))
        '(org-verbatim ((t (:foreground "#eeeeec" :underline t :slant italic))))
        '(org-block ((t (:foreground "#7e8aa2"))))
        '(org-quote ((t (:inherit org-block :slant italic))))
        '(org-verse ((t (:inherit org-block :slant italic))))
        '(org-todo ((t (:bold t :foreground "#ffffff"))))
        '(org-done ((t (:bold t :foreground "#708090"))))
        '(org-warning ((t (:underline t :foreground "#ff0000"))))
        '(org-agenda-structure ((t (:weight bold :foreground "#df9f2d"))))
        '(org-agenda-date ((t (:foreground "#ff9800" :height 1.2))))
        '(org-agenda-date-weekend ((t (:weight normal :foreground "#808bed"))))
        '(org-agenda-date-today ((t (:weight bold :foreground "#ff9800" :height 1.4))))
	'(org-scheduled ((t (:foreground "#eeeeec"))))
	'(font-latex-bold-face ((t (:foreground "#cd8b00"))))
	'(font-latex-italic-face ((t (:foreground "#808bed" :italic t))))
	'(font-latex-string-face ((t (:foreground "#708090"))))
	'(font-latex-match-reference-keywords ((t (:foreground "#708090"))))
	'(font-latex-match-variable-keywords ((t (:foreground "#708090"))))
	'(ido-only-match ((t (:foreground "#ff9800"))))
	'(org-sexp-date ((t (:foreground "#808080"))))
	'(ido-first-match ((t (:foreground "#b1d631"))))
	'(gnus-header-content ((t (:foreground "#ff9810"))))
	'(gnus-header-from ((t (:foreground "#f0e16a"))))
	'(gnus-header-name ((t (:foreground "#ff9800"))))
	'(gnus-header-subject ((t (:foreground "#ff8800"))))
	'(mu4e-view-url-number-face ((t (:foreground "#7e8aa2"))))
	'(mu4e-cited-1-face ((t (:foreground "#df9f2d"))))
	'(mu4e-cited-7-face ((t (:foreground "#808bed"))))
	'(slime-repl-inputed-output-face ((t (:foreground "#ff9800")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'mustang)

;;; mustang-theme.el ends here
