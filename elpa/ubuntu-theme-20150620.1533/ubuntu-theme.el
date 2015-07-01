;;; ubuntu-theme.el --- A theme inspired by the default terminal colors in Ubuntu

;; Copyright (C) 2014, 2015 Francesc Rocher

;; Author: Francesc Rocher <francesc.rocher@gmail.com>
;; URL: http://github.com/rocher/ubuntu-theme
;; Package-Version: 20150620.1533
;; Version: 0.3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A color theme for Emacs 24 inspired by the default terminal colors in
;; Ubuntu.

;;; Code:

(deftheme ubuntu "Ubuntu color theme")

(custom-theme-set-faces
 'ubuntu
 '(button ((t (:inherit (link)))))
 '(cursor ((t (:background "white"))))
 '(default ((t (:inherit nil :stipple nil :background "#300a24" :foreground "#e0e0e0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :foundry "unknown" :family "Ubuntu Mono"))))
 '(diff-file-header ((t (:background "grey60" :foreground "black" :weight bold))))
 '(diff-refine-added ((t (:inherit diff-refine-change :background "#114411" :foreground "#11ff11"))))
 '(diff-refine-changed ((t (:background "#444411" :foreground "#ffff11"))))
 '(diff-refine-removed ((t (:inherit diff-refine-change :background "#441111" :foreground "#ff1111"))))
 '(ediff-current-diff-A ((t (:background "#553333" :foreground "ligth grey"))))
 '(ediff-current-diff-Ancestor ((t (:background "VioletRed" :foreground "white"))))
 '(ediff-current-diff-B ((t (:background "#335533" :foreground "light grey"))))
 '(ediff-current-diff-C ((t (:background "#888833" :foreground "white"))))
 '(ediff-even-diff-A ((t (:background "light grey" :foreground "dim grey"))))
 '(ediff-even-diff-Ancestor ((t (:background "ligth grey" :foreground "dim grey"))))
 '(ediff-even-diff-B ((t (:background "light grey" :foreground "dim grey"))))
 '(ediff-even-diff-C ((t (:background "light grey" :foreground "dim grey"))))
 '(ediff-fine-diff-A ((t (:background "#441111" :foreground "#ff1111"))))
 '(ediff-fine-diff-Ancestor ((t (:background "#114411" :foreground "#11ff11" :weight normal))))
 '(ediff-fine-diff-B ((t (:background "#114411" :foreground "#11ff11" :weight normal))))
 '(ediff-fine-diff-C ((t (:background "#444411" :foreground "#ffff11" :weight normal))))
 '(ediff-odd-diff-A ((t (:background "light grey" :foreground "dim gray"))))
 '(ediff-odd-diff-B ((t (:background "light grey" :foreground "dim gray"))))
 '(ediff-odd-diff-C ((t (:background "dim grey" :foreground "light grey"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(fixed-pitch ((t (:family "Ubuntu Mono"))))
 '(font-lock-builtin-face ((t (:foreground "#FF7FD4"))))
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#a5a5a5" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#8dd7e9"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#1D68C4" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "#2D78f4" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#5FB7CC"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#aa3939"))))
 '(font-lock-type-face ((t (:foreground "#7FFFD4"))))
 '(font-lock-variable-name-face ((t (:foreground "#ffffaa"))))
 '(font-lock-warning-face ((t (:inherit (error)))))
 '(fringe ((((class color) (background light)) (:background "grey95")) (((class color) (background dark)) (:background "grey10")) (t (:background "gray"))))
 '(git-gutter+-unchanged ((t (:background "yellow" :foreground "black"))))
 '(header-line ((t (:box nil :foreground "grey90" :background "grey20" :inherit (mode-line)))))
 '(highlight ((t (:background "#441133"))))
 '(isearch ((t (:background "palevioletred1" :foreground "brown4"))))
 '(isearch ((t (:background "palevioletred1" :foreground "brown4"))))
 '(isearch-fail ((t (:background "red4" :foreground "white"))))
 '(isearch-fail ((t (:background "red4" :foreground "white"))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
 '(lazy-highlight ((t (:background "khaki3" :foreground "black"))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "deep sky blue"))))
 '(link-visited ((t (:foreground "violet" :inherit (link)))))
 '(linum ((t (:background "#300a24" :foreground "light grey" :slant italic :height 0.85))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(minibuffer-prompt ((((background dark)) (:foreground "deep sky blue")) (((type pc)) (:foreground "magenta")) (t (:foreground "medium blue"))))
 '(mode-line ((t (:background "violetred4" :foreground "#FFFFf9" :box (:line-width 2 :color "violetred4")))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t nil)))
 '(mode-line-inactive ((t (:background "#200019" :foreground "#999999" :box (:line-width 2 :color "#200019")))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(region ((t (:foreground "#FFFFFF" :background "#1D68C4"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(tooltip ((t (:foreground "black" :background "lightyellow" :inherit (variable-pitch)))))
 '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))
 '(variable-pitch ((t (:family "Ubuntu"))))
 )


;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'ubuntu)

;;; ubuntu-theme.el ends here
