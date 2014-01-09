;;; gandalf-theme.el --- Gandalf color theme

;; Copyright (C) 2013 Peter Vasil

;; Author: Peter Vasil <mail@petervasil.net>
;; Keywords: color theme
;; Version: 20130809.1147
;; X-Original-Version: 0.1

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

;; This theme is a port of the overtone/emacs-live theme of the same
;; name (https://github.com/overtone/emacs-live) by Sam Aaron. The
;; original theme was designed for use with the color-theme package.
;; This theme adopts the new built-in theme support deftheme.

;;; Code:

(deftheme gandalf
  "Gandalf color theme by Sam Aaron, modified by ptrv")

(let ((class '((class color) (min-colors 89)))
      ;; gandalf color palette
      (gandalf-bg "#f2f2f2")
      (gandalf-fg "#000000")
      (gandalf-red-1 "#8b0000")
      (gandalf-red-2 "red1")
      (gandalf-blue-1 "dark blue")
      (gandalf-blue-2 "blue")
      (gandalf-blue-3 "#758BC6")
      (gandalf-blue-4 "#96CBFE")
      (gandalf-blue-5 "#27408b")
      (gandalf-blue-6 "slate blue")
      (gandalf-green-1 "dark green")
      (gandalf-green-2 "SeaGreen")
      (gandalf-green-3 "chartreuse3")
      (gandalf-green-4 "#00A000")
      (gandalf-cyan-1 "dark cyan")
      (gandalf-black-1 "#000000")
      (gandalf-pink-1 "violet red")
      (gandalf-pink-2 "deep pink")
      (gandalf-pink-3 "pink")
      (gandalf-brown-1 "brown")
      (gandalf-orange-1 "dark orange")
      (gandalf-orange-2 "orange")
      (gandalf-white-1 "white")
      (gandalf-yellow-1 "#FBDE2D")
      (gandalf-yellow-2 "yellow")
      (gandalf-gold-1 "#b8860b")
      (gandalf-gray-1 "gray10")
      (gandalf-gray-2 "#333333")
      (gandalf-gray-3 "gray25")
      (gandalf-gray-4 "grey30")
      (gandalf-gray-5 "gray40")
      (gandalf-gray-6 "gray50")
      (gandalf-gray-7 "gray60")
      (gandalf-gray-8 "gray70")
      (gandalf-gray-9 "gray80")
      (gandalf-gray-10 "gray85"))

  (custom-theme-set-faces
   'gandalf
   `(default ((,class (:background ,gandalf-bg :foreground ,gandalf-fg))))
   `(cursor ((,class (:background ,gandalf-red-1))))
   `(bold ((,class (:bold t))))
   `(bold-italic ((,class (:bold t))))
   `(border-glyph ((,class (nil))))
   `(buffers-tab ((,class (:background ,gandalf-white-1 :foreground ,gandalf-fg))))
   `(font-lock-builtin-face ((,class (:foreground ,gandalf-blue-1))))
   `(font-lock-comment-face ((,class (:italic t :foreground ,gandalf-green-1 :italic t))))
   `(font-lock-constant-face ((,class (:foreground ,gandalf-blue-1))))
   `(font-lock-doc-string-face ((,class (:foreground ,gandalf-fg))))
   `(font-lock-function-name-face ((,class (:foreground ,gandalf-blue-1 :bold t))))
   `(font-lock-keyword-face ((,class (:foreground ,gandalf-fg :bold t))))
   `(font-lock-preprocessor-face ((,class (:foreground ,gandalf-fg))))
   `(font-lock-reference-face ((,class (:foreground ,gandalf-cyan-1))))

   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,gandalf-fg))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,gandalf-fg))))

   `(font-lock-string-face ((,class (:foreground ,gandalf-green-1  :bold t))))

   `(font-lock-type-face ((,class (:foreground ,gandalf-blue-2))))
   `(font-lock-variable-name-face ((,class (:foreground ,gandalf-cyan-1))))
   `(font-lock-warning-face ((,class (:bold t :foreground ,gandalf-fg))))
   `(gui-element ((,class (:background ,gandalf-gray-5 :foreground ,gandalf-blue-4))))
   `(region ((,class (:background ,gandalf-blue-3))))
   `(mode-line ((,class (:background ,gandalf-gray-6 :foreground ,gandalf-white-1))))
   `(mode-line-inactive ((,class (:background ,gandalf-gray-6 :foreground ,gandalf-fg))))
   `(highlight ((,class (:background ,gandalf-gray-8))))
   `(isearch ((,class (:background ,gandalf-pink-2 :foreground ,gandalf-fg))))
   `(isearch-fail ((,class (:background ,gandalf-red-2))))
   `(query-replace ((,class (:background ,gandalf-gray-5))))
   `(hl-line ((,class (:background ,gandalf-gray-9))))
   `(Highline-face ((,class (:background ,gandalf-green-2))))
   `(italic ((,class (nil))))
   `(left-margin ((,class (nil))))
   `(text-cursor ((,class (:background ,gandalf-yellow-2 :foreground ,gandalf-fg))))
   `(toolbar ((,class (nil))))
   `(underline ((nil (:underline nil))))
   `(vertical-border ((,class (:background ,gandalf-black-1 :foreground ,gandalf-gray-2))))
   `(erc-default-face ((,class (:foreground ,gandalf-green-1))))

   `(ido-first-match ((,class (:foreground ,gandalf-fg :background ,gandalf-gray-8 :bold t))))
   `(ido-only-match ((,class (:foreground ,gandalf-green-1 :background ,gandalf-bg))))
   `(ido-subdir ((,class (:foreground ,gandalf-white-1 :background ,gandalf-blue-5 :bold t))))
   `(ido-indicator ((,class (:foreground ,gandalf-fg :background ,gandalf-pink-2))))
   `(minibuffer-prompt ((,class (:foreground ,gandalf-blue-1 :background ,gandalf-gray-10))))

   `(magit-item-highlight ((,class (:background ,gandalf-gray-3))))
   `(magit-diff-add ((,class (:foreground ,gandalf-green-3))))
   `(magit-diff-del ((,class (:foreground ,gandalf-pink-1))))
   `(magit-section-type ((,class (:foreground ,gandalf-pink-2))))
   `(magit-diff-hunk-header ((,class (:foreground ,gandalf-orange-2))))
   `(magit-branch ((,class (:foreground ,gandalf-gold-1))))

   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,gandalf-red-1))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,gandalf-green-1))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,gandalf-pink-2))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,gandalf-brown-1))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,gandalf-green-1))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,gandalf-blue-1))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,gandalf-orange-1))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,gandalf-blue-6))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,gandalf-gray-1))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,gandalf-white-1))))

   `(vhl/default-face ((,class (:background ,gandalf-gray-7))))

   `(undo-tree-visualizer-default-face ((,class (:foreground ,gandalf-gray-4))))
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,gandalf-pink-2 :background ,gandalf-gray-5))))

   `(markdown-link-face ((,class (:background ,gandalf-yellow-1))))

   ;; `(flycheck-error ((,class (:background ,gandalf-pink-3))))
   ;; `(flycheck-warning ((,class (:background ,gandalf-orange-2))))

   `(flymake-errline ((,class (:background ,gandalf-pink-3))))
   `(flymake-warnline ((,class (:background ,gandalf-orange-2))))

   `(eshell-prompt ((,class (:foreground ,gandalf-green-1 :bold t))))

   `(diff-added ((,class (:foreground ,gandalf-green-4))))
   `(diff-removed ((,class (:foreground ,gandalf-red-2))))
   )

  ;; (custom-theme-set-variables
  ;;  'gandalf
  ;;  )
  )

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'gandalf)

;;; gandalf-theme.el ends here
