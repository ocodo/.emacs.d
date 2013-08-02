;;; pastels-on-dark-theme.el --- Pastels on Dark theme for Emacs 24

;; Author: Mats Persson
;; Maintainer: Shane Celis <shane (at) gnufoo (dot) org>
;; URL: http://gist.github.com/1906662
;; Version: 20120304.1222
;; X-Original-Version: 0.3
;; Keywords: theme, color

;;; Commentary:

;; Pastels on Dark was created by Mats Persson and popularized in
;; TextMate.app.  Ported to Emacs 24 by Shane Celis because I love it!

;; To install the theme, M-x `load-theme' then enter
;; `pastels-on-dark'.  If you run into any issues, check the
;; `custom-theme-load-path' variable.


;;; Code:

(deftheme pastels-on-dark
  "Pastels on Dark created by Mats Persson and popularized in TextMate.app.  Ported to Emacs 24 by Shane Celis because I love it!")

(custom-theme-set-faces
 'pastels-on-dark
 '(cursor ((t (:background "#FFFFFF"))))
 '(escape-glyph ((t (:foreground "#47B8D6"))))
 '(minibuffer-prompt ((t (:foreground "#47B8D6"))))
 '(highlight ((t (:background "#262626"))))
 '(region ((t (:background "#322A31"))))
 '(shadow ((t (:foreground "#555555"))))
 '(secondary-selection ((t (:background "#463849"))))
 '(trailing-whitespace ((t (:background "#FFD0D0"))))
 '(font-lock-builtin-face ((t (:foreground "#7171F3" :weight bold))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#ed5b15" :inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "#ed5b15"))))
 '(font-lock-constant-face ((t (:foreground "#DF7921" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "#A1A1FF"))))
 '(font-lock-keyword-face ((t (:foreground "#4856F7" :weight bold))))
 '(font-lock-preprocessor-face ((t (:foreground "#4856F7"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit bold :foreground "#666666"))))
 '(font-lock-regexp-grouping-construct ((t (:inherit bold :foreground "#666666"))))
 '(font-lock-string-face ((t (:foreground "#A2925E"))))
 '(font-lock-type-face ((t (:foreground "#DADADA"))))
 '(font-lock-variable-name-face ((t (:foreground "#C1C144"))))
 '(font-lock-warning-face ((t (:foreground "#EC9E00"))))
 '(link ((t (:foreground "#0066FF" :underline t))))
 '(link-visited ((t (:inherit link :foreground "#FF0066"))))
 '(fringe ((t nil)))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
 '(isearch ((t (:background "#463849" :foreground "#ffffff" :weight bold))))
 '(compilation-error ((t (:inherit error))))
 '(error ((t (:foreground "#B20006" :weight bold))))
 '(warning ((t (:foreground "#FF8000" :weight bold))))
 '(success ((t (:foreground "#00DD00" :weight bold))))
 '(compilation-line-number ((t (:foreground "#EC9E00"))))
 '(glyphless-char ((t (:background "#4F4D4D"))))
 '(lazy-highlight ((t (:background "#302733"))))
 '(default ((t (:background "#211D1D" :foreground "#DADADA")))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'pastels-on-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; pastels-on-dark-theme.el ends here
