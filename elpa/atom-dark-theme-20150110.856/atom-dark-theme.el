;;; atom-dark-theme.el --- An Emacs port of the Atom Dark theme from Atom.io.
;;
;;
;; Author: Jeremy Whitlock <jwhitlock@apache.org
;; Version: 20150110.856
;; X-Original-Version: 0.1
;; Keywords: themes atom dark
;; URL: https://github.com/whitlockjc/atom-dark-theme-emacs
;;
;; This file is not part of GNU Emacs.
;;
;; Licenese:
;;
;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary
;;
;; An Emacs port of the Atom Dark theme from Atom.io.
;;
;;; Code

(deftheme atom-dark
  "Atom Dark - An Emacs port of the Atom Dark theme from Atom.io.")

;; Testing
(custom-theme-set-faces
 'atom-dark
 
 ;; Basic
 '(button ((t (:inherit (link)))))
 '(cursor ((((background light)) (:background "black")) (((background dark)) (:background "white"))))
 '(default ((t (:inherit nil :stipple nil :background "#1d1f21" :foreground "#c5c8c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Source Code Pro"))))
 '(escape-glyph ((t (:foreground "#FF8000"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(header-line ((t (:foreground "grey90" :background "grey20"))))
 '(highlight ((t (:background "#444"))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
 '(link ((t (:inherit font-lock-keyword-face :underline t))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(minibuffer-prompt ((t (:foreground "#FF8000"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(region ((t (:background "grey70"))))
 '(secondary-selection ((t (:background "#262626"))))
 '(shadow ((t (:foreground "#7c7c7c"))))
 '(tooltip ((t (:inherit variable-pitch :background "#fff" :foreground "#333"))))
 '(trailing-whitespace ((t (:background "#562d56" :foreground "#FD5FF1"))))
 '(variable-pitch ((t (:family "Sans Serif"))))

 ;; Font-lock
 '(font-lock-builtin-face ((t (:foreground "#DAD085"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#7C7C7C"))))
 '(font-lock-constant-face ((t (:foreground "#99CC99"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#FFD2A7"))))
 '(font-lock-keyword-face ((t (:foreground "#96CBFE"))))
 '(font-lock-preprocessor-face ((t (:foreground "#8996A8"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit font-lock-string-face))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#C6A24F"))))
 '(font-lock-string-face ((t (:foreground "#8AE234"))))
 '(font-lock-type-face ((t (:foreground "#C6C5FE"))))
 '(font-lock-variable-name-face ((t (:inherit (default)))))
 '(font-lock-warning-face ((t (:foreground "#ff982d" :weight bold))))
 
 ;; mode-line
 '(mode-line ((t (:background "grey10" :foreground "#96CBFE"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "#1d1f21" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((default (:inherit (mode-line))) (((class color) (min-colors 88) (background light)) (:background "#7c7c7c" :foreground "grey20" :box (:line-width -1 :color "grey75" :style nil) :weight light)) (((class color) (min-colors 88) (background dark)) (:background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40" :style nil) :weight light))))

 ;; isearch
 '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3")) (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2")) (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4")) (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4")) (t (:inverse-video t))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))

 ;; diff-hl (https://github.com/dgutov/diff-hl)
 '(diff-hl-change ((t (:foreground "#E9C062" :background "#8b733a"))))
 '(diff-hl-delete ((t (:foreground "#CC6666" :background "#7a3d3d"))))
 '(diff-hl-insert ((t (:foreground "#A8FF60" :background "#547f30"))))

 ;; dired-mode
 '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
 '(dired-flagged ((t (:inherit (diff-hl-delete)))))
 '(dired-symlink ((t (:foreground "#FD5FF1"))))

 ;; guide-key (https://github.com/kai2nenobu/guide-key)
 '(guide-key/highlight-command-face ((t (:inherit (cursor)))))
 '(guide-key/key-face ((t (:inherit (font-lock-warning-face)))))
 '(guide-key/prefix-command-face ((t (:inherit (font-lock-keyword-face)))))
 
 ;; js2-mode (https://github.com/mooz/js2-mode)
 '(js2-error ((t (:foreground "#c00"))))
 '(js2-external-variable ((t (:inherit (font-lock-builtin-face)))))
 '(js2-function-param ((t (:foreground "#C6C5FE"))))
 '(js2-jsdoc-html-tag-delimiter ((t (:foreground "#96CBFE"))))
 '(js2-jsdoc-html-tag-name ((t (:foreground "#96CBFE"))))
 '(js2-jsdoc-tag ((t (:inherit (font-lock-doc-face):weight bold))))
 '(js2-jsdoc-type ((t (:inherit (font-lock-function-name-face)))))
 '(js2-jsdoc-value ((t (:inherit js2-function-param))))

 ;; powerline (https://github.com/milkypostman/powerline)
 '(powerline-active2 ((t (:background "grey10"))))
 )

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
		  (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'atom-dark)

;;; atom-dark-theme.el ends here
