;;; dakrone-theme.el --- dakrone's custom dark theme
;; Copyright (C) 2013 Lee Hinman

;; Author: Lee Hinman <lee _AT_ writequit.org>
;; URL: https://github.com/dakrone/dakrone-theme
;; Package-Version: 20170801.1933
;; Version: 0.0.2
;; Keywords: color themes
;; This file is not part of GNU Emacs.

;;; License:
;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU Lesser General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
;; details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; A dark theme with lots of color
;;
;; To use, put the following in your Emacs config:
;;
;;   (load-theme 'dakrone t)
;;
;; Requires Emacs 24

;;; Code:

(deftheme dakrone
  "Dakrone's dark background theme.
Dark gray background with lots of greens, teals and blues.")

(custom-theme-set-faces
 'dakrone
 '(default ((t (:foreground "#fff8dc" :background "#1c1c1c"))))
 '(foreground-color ((t (:foreground "#fff8dc"))))
 ;; '(background-color . "#1a1a1a")
 ;; '(mouse-color . "black")
 ;; '(cursor-color . "white")
 ;; '(border-color . "black")
 ;; '(background-mode . dark)
 ;;'(mode-line ((t (:foreground "#bfebbf" :background "#2b2b2b"))))
 ;;'(mode-line-buffer-id ((t (:inherit zenburn-strong-1-face))))
 ;;'(mode-line-inactive ((t (:foreground "#5f7f5f"  :background "#2b2b2b"))))
 '(highlight ((t (:foreground "wheat" :background "#2f4f4f"))))
 '(linum ((t (:foreground "#333333"))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(bold ((t (:bold t))))
 '(italic ((t (:italic t))))
 '(bold-italic ((t (:bold t :italic t))))
 '(region ((t (:background "#333333"))))
 '(secondary-selection ((t (:background "#262626"))))
 '(underline ((t (:underline t))))
 '(info-node ((t (:foreground "yellow" :bold t :italic t))))
 '(info-menu-5 ((t (:underline t))))
 '(info-xref ((t (:foreground "yellow" :bold t))))
 '(diary-face ((t (:foreground "orange"))))
 '(calendar-today-face ((t (:underline t))))
 '(holiday-face ((t (:background "red"))))
 '(show-paren-mismatch-face ((t (:foreground "white" :background "red"))))
 '(paren-face ((t (:foreground "gray35"))))
 '(font-lock-comment-face ((t (:foreground "#656763"))))
 '(font-lock-string-face ((t (:foreground "#8AE234"))))
 ;;'(font-lock-keyword-face ((t (:foreground "#00ffff"))))
 '(font-lock-keyword-face ((t (:foreground "#729FCF"))))
 '(font-lock-builtin-face ((t (:foreground "#b0c4de"))))
 '(font-lock-function-name-face ((t (:foreground "#00ffff"))))
 '(font-lock-variable-name-face ((t (:foreground "#00ffff"))))
 ;;'(font-lock-type-face ((t (:foreground "yellow1"))))
 '(font-lock-type-face ((t (:foreground "#edd400"))))
 '(idle-highlight ((t (:background "#444444"))))
 '(font-lock-constant-face ((t (:foreground "#fa8072"))))
 '(font-lock-warning-face ((t (:foreground "gold" :bold t))))
 '(blank-space-face ((t (:background "#1e2426"))))
 '(blank-tab-face ((t (:foreground "black" :background "cornsilk"))))
 '(highline-face ((t (:background "gray35"))))
 ;; can't seem to get colorize-nicks and erc-pals to work together
 ;;'(erc-pal-face ((t (nil))))
 '(erc-pal-face ((t (:foreground "indianred"))))
 '(eshell-ls-directory-face ((t (:foreground "green" :bold t))))
 '(eshell-ls-symlink-face ((t (:foreground "Cyan" :bold t))))
 '(eshell-ls-executable-face ((t (:foreground "orange" :bold t))))
 '(eshell-ls-readonly-face ((t (:foreground "gray"))))
 '(eshell-ls-unreadable-face ((t (:foreground "#A9a9a9"))))
 '(eshell-ls-special-face ((t (:foreground "Magenta" :bold t))))
 '(eshell-ls-missing-face ((t (:foreground "Red" :bold t))))
 '(eshell-ls-archive-face ((t (:foreground "Orchid" :bold t))))
 '(eshell-ls-backup-face ((t (:foreground "#ffa07a"))))
 '(eshell-ls-product-face ((t (:foreground "#ffa07a"))))
 '(eshell-ls-clutter-face ((t (:foreground "blue" :bold t))))

 ;; Org
 ;;'(org-hide ((t (:foreground "#2e3436"))))
 '(org-level-1 ((t (:bold t :foreground "#edd400" :height 1.2))))
 '(org-level-2 ((t (:bold t :foreground "#729FCF" :height 1.1))))
 '(org-level-3 ((t (:bold t :foreground "#00ffff" :height 1.0))))
 '(org-block-background ((t (:background "#262626"))))

 ;; (org-date ((t (:underline t :foreground "magenta3"))))
 ;; (org-footnote  ((t (:underline t :foreground "magenta3"))))
 ;; (org-link ((t (:foreground "skyblue2" :background "#2e3436"))))
 ;; (org-special-keyword ((t (:foreground "brown"))))
 ;; (org-verbatim ((t (:foreground "#eeeeec" :underline t :slant italic))))
 ;; (org-block ((t (:foreground "#bbbbbc"))))
 ;; (org-quote ((t (:inherit org-block :slant italic))))
 ;; (org-verse ((t (:inherit org-block :slant italic))))
 ;; (org-todo ((t (:bold t :foreground "Red"))))
 ;; (org-done ((t (:bold t :foreground "ForestGreen"))))
 ;; (org-agenda-structure ((t (:weight bold :foreground "tomato"))))
 ;; (org-agenda-date ((t (:foreground "#6ac214"))))
 ;; (org-agenda-date-weekend ((t (:weight normal :foreground "dodger blue"))))
 ;; (org-agenda-date-today ((t (:weight bold :foreground "#edd400"))))

 '(sgml-start-tag-face ((t (:foreground "mediumspringgreen"))))
 '(custom-button-face ((t (:foreground "white"))))
 '(sgml-ignored-face ((t (:foreground "#333333" :background "gray60"))))
 '(sgml-doctype-face ((t (:foreground "orange"))))
 '(sgml-sgml-face ((t (:foreground "yellow"))))
 '(vc-annotate-face-0046FF ((t (:foreground "wheat" :background "black"))))
 '(custom-documentation-face ((t (:foreground "white"))))
 '(sgml-end-tag-face ((t (:foreground "greenyellow"))))
 '(linemenu-face ((t (:background "gray30"))))
 '(sgml-entity-face ((t (:foreground "gold"))))
 '(message-header-to-face ((t (:foreground "floral white" :bold t))))
 '(message-header-cc-face ((t (:foreground "ivory"))))
 '(message-header-subject-face ((t (:foreground "papaya whip" :bold t))))
 '(message-header-newsgroups-face ((t (:foreground "lavender blush" :bold t :italic t))))
 '(message-header-other-face ((t (:foreground "pale turquoise"))))
 '(message-header-name-face ((t (:foreground "light sky blue"))))
 '(message-header-xheader-face ((t (:foreground "blue"))))
 '(message-separator-face ((t (:foreground "sandy brown"))))
 '(message-cited-text-face ((t (:foreground "plum1"))))
 '(message-mml-face ((t (:foreground "ForestGreen"))))
 '(font-latex-bold-face ((t (nil))))
 '(font-latex-italic-face ((t (nil))))
 '(font-latex-math-face ((t (nil))))
 '(font-latex-sedate-face ((t (:foreground "Gray85"))))
 '(font-latex-string-face ((t (:foreground "orange"))))
 '(font-latex-warning-face ((t (:foreground "gold"))))
 '(widget-documentation-face ((t (:foreground "lime green"))))
 '(widget-button-face ((t (:bold t))))
 '(widget-field-face ((t (:background "#333333"))))
 '(widget-single-line-field-face ((t (:background "#333333"))))
 '(widget-inactive-face ((t (:foreground "wheat"))))
 '(widget-button-pressed-face ((t (:foreground "red"))))
 '(custom-invalid-face ((t (:foreground "yellow" :background "red"))))
 '(custom-rogue-face ((t (:foreground "pink" :background "black"))))
 '(custom-modified-face ((t (:foreground "white" :background "blue"))))
 '(custom-set-face ((t (:foreground "blue"))))
 '(custom-changed-face ((t (:foreground "wheat" :background "skyblue"))))
 '(custom-saved-face ((t (:underline t))))
 '(custom-state-face ((t (:foreground "light green"))))
 '(custom-variable-tag-face ((t (:foreground "skyblue" :underline t))))
 '(custom-variable-button-face ((t (:bold t :underline t))))
 '(custom-face-tag-face ((t (:foreground "white" :underline t))))
 '(custom-group-tag-face-1 ((t (:foreground "pink" :underline t))))
 '(custom-group-tag-face ((t (:foreground "skyblue" :underline t))))
 '(swbuff-current-buffer-face ((t (:foreground "red" :bold t))))
 '(ediff-current-diff-face-A ((t (:foreground "firebrick" :background "pale green"))))
 '(ediff-current-diff-face-B ((t (:foreground "DarkOrchid" :background "Yellow"))))
 '(ediff-current-diff-face-C ((t (:foreground "white" :background "indianred"))))
 '(ediff-current-diff-face-Ancestor ((t (:foreground "black" :background "VioletRed"))))
 '(ediff-fine-diff-face-A ((t (:foreground "Navy" :background "sky blue"))))
 '(ediff-fine-diff-face-B ((t (:foreground "black" :background "cyan"))))
 '(ediff-fine-diff-face-C ((t (:foreground "black" :background "Turquoise"))))
 '(ediff-fine-diff-face-Ancestor ((t (:foreground "black" :background "Green"))))
 '(ediff-even-diff-face-A ((t (:foreground "black" :background "#d3d3d3"))))
 '(ediff-even-diff-face-B ((t (:foreground "white" :background "Grey"))))
 '(ediff-even-diff-face-C ((t (:foreground "black" :background "#d3d3d3"))))
 '(ediff-even-diff-face-Ancestor ((t (:foreground "white" :background "Grey"))))
 '(ediff-odd-diff-face-A ((t (:foreground "white" :background "Grey"))))
 '(ediff-odd-diff-face-B ((t (:foreground "black" :background "#d3d3d3"))))
 '(ediff-odd-diff-face-C ((t (:foreground "white" :background "Grey"))))
 '(ediff-odd-diff-face-Ancestor ((t (:foreground "black" :background "#d3d3d3"))))
 '(header-line ((t (:foreground "grey90" :background "grey20"))))
 '(magit-item-highlight ((t (:background "#333333"))))
 '(magit-diff-add ((t (:foreground "green3" :background "#1a1a1a"))))
 '(magit-diff-del ((t (:foreground "red3" :background "#1a1a1a"))))
 '(magit-diff-file-header ((t (:background "#1a1a1a"))))
 '(magit-diff-hunk-header ((t (:background "#1a1a1a"))))
 '(diff-added ((t (:foreground "green4" :background "#1a1a1a"))))
 '(diff-removed ((t (:foreground "red3" :background "#1a1a1a"))))
 '(show-paren-match-face ((t (:background "#232323"))))
 '(mode-line ((t (:foreground "#bfebbf" :background "#4b8bb0"))))
 '(powerline-active1 ((t (:foreground "grey90" :background "grey22"))))
 '(powerline-active2 ((t (:background "grey22"))))
 '(powerline-inactive1 ((t (:foreground "grey40" :background "grey90"))))
 '(powerline-inactive1 ((t (:foreground "grey40" :background "grey90"))))
 '(clojure-parens ((t (:foreground "#696969"))))
 '(clojure-braces ((t (:foreground "#696969"))))
 '(clojure-brackets ((t (:foreground "#4682b4"))))
 '(clojure-keyword ((t (:foreground "#729FCF"))))
 '(clojure-namespace ((t (:foreground "#c476f1"))))
 '(clojure-java-call ((t (:foreground "#008b8b"))))
 '(clojure-special ((t (:foreground "#1BF21B"))))
 '(clojure-double-quote ((t (:foreground "#1BF21B"))))
 '(clojure-collapsed-fn ((t (:foreground "cyan")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dakrone)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; dakrone-theme.el ends here
