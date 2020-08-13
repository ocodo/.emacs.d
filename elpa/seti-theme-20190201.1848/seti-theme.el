;;; seti-theme.el --- A dark colored theme, inspired by Seti Atom Theme
;;
;;
;; Author: Vlad Piersec <vlad.piersec@gmail.com>
;; Version: 0.2
;; Package-Version: 20190201.1848
;; Package-Commit: 9d76db0b91d4f574dd96ac80fad41da35bffa109
;; Keywords: themes
;; URL: https://github.com/caisah/seti-theme
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
;; Dark theme inspired by Atom's Seti
;;
;; Tries to reproduce the feel of https://github.com/jesseweed/seti-ui
;;; Code

(deftheme seti
  "Seti - A theme inspired by Seti Atom Theme")

(let ((blue "#55B5DB")
      (green "#9FCA56")
      (yellow "#DCCD69")
      (red "#CE4045")
      (purple "#A074C4")
      (background "#151718")
      (background-2 "#1E2326")
      (background-3 "#0D1011")
      (background-4 "#101112")
      (text "#D4D7D6")
      (text-2 "#858D8A")
      (text-3 "#41535B")
      (text-4 "#2F3C42")
      (text-highlight "#FFFFFF")
      (text-region "#434546")
      (text-dired "#A0A0A0")
      (input-text "#CCCCCC")
      (light-blue "#75E5F4")
      (dark-blue "#4F99D3")
      (intense-green "#8BE03C"))

  (custom-theme-set-faces
   'seti

   ;; Basics
   `(default ((t (:background ,background :foreground ,text))))
   `(cursor ((t (:background ,input-text :foreground ,background))))
   `(highlight ((t (:background ,text-highlight))))
   `(minibuffer-prompt ((t (:foreground ,dark-blue :weight bold))))
   `(region ((t (:background ,text-region))))
   `(error ((t (:foreground ,red :weight bold :underline (:color ,red :style line)))))

   `(isearch ((t (:background ,background :foreground ,text :box (:line-width 1 :color ,dark-blue) :weight bold))))
   `(lazy-highlight ((t (:background ,background :foreground ,text-2 :box (:line-width 1 :color ,dark-blue)))))
   `(mode-line ((t (:foreground ,text :background ,background-3 :underline (:color ,dark-blue :style line)))))
   `(mode-line-buffer-id ((t (:weight bold :foreground ,yellow))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-highlight ((t (:box (:line-width 3 :color ,dark-blue)))))
   `(mode-line-inactive ((t (:weight light :foreground ,text :background ,background-2))))
   `(secondary-selection ((t (:background ,background-2))))
   `(trailing-whitespace ((t (:background ,background-3))))
   `(match ((t (:weight bold :foreground ,background :background ,intense-green))))
   `(next-error ((t (:inherit (region)))))
   `(query-replace ((t (:inherit (isearch)))))

   ;; Font Lock
   `(font-lock-builtin-face ((t (:foreground ,purple))))
   `(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
   `(font-lock-comment-face ((t (:foreground ,text-3))))
   `(font-lock-constant-face ((t (:foreground ,red))))
   `(font-lock-doc-face ((t (:foreground ,blue))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,green))))
   `(font-lock-negation-char-face ((t nil)))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground ,blue))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,blue))))
   `(font-lock-warning-face ((t (:weight bold :inherit (error)))))

   ;; Parens
   `(show-paren-match ((t (:foreground ,text-2 :underline (:color ,dark-blue :style line)))))
   `(show-paren-mismatch ((t (:foreground ,text-2 :underline (:color ,red :style line)))))

   ;; Dired
   `(dired-directory ((t (:foreground ,text :weight extrabold))))
   `(dired-header ((t (:foreground "white"  :background ,blue :weight bold))))
   `(dired-ignored ((t (:foreground ,text-3))))
   `(dired-flagged ((t (:foreground ,red :weight bold))))
   `(dired-marked ((t (:background ,blue :foreground "white" :weight normal))))
   `(dired-perm-write ((t (:foreground ,yellow :weight ultra-bold))))
   `(dired-symlink ((t (:foreground ,light-blue :weight normal))))
   `(dired-warning ((t (:inherit (font-lock-warning-face)))))

   ;; helm
   `(helm-bookmark-directory ((t (:inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground ,text))))
   `(helm-bookmark-gnus ((t (:foreground ,text-2))))
   `(helm-bookmark-info ((t (:foreground ,text-2))))
   `(helm-bookmark-man ((t (:foreground ,text-2))))
   `(helm-bookmark-w3m ((t (:foreground ,text-2))))
   `(helm-buffer-directory ((t (:foreground ,text :background ,background))))
   `(helm-buffer-file ((t (:foreground ,text :background ,background))))
   `(helm-buffer-not-saved ((t (:foreground ,text-2 :background ,background))))
   `(helm-buffer-process ((t (:foreground ,green :background ,background))))
   `(helm-buffer-saved-out ((t (:foreground ,text :background ,background))))
   `(helm-buffer-size ((t (:foreground ,text :background ,background))))
   `(helm-candidate-number ((t (:background ,background :foreground ,green :inherit bold))))
   `(helm-ff-directory ((t (:foreground ,green :background ,background :inherit bold))))
   `(helm-ff-dotted-directory ((t (:foreground ,green :background ,background :inherit bold))))
   `(helm-ff-dotted-symlink-directory ((t (:foreground ,light-blue :background ,background :inherit bold))))
   `(helm-ff-executable ((t (:foreground ,intense-green :background ,background :weight normal))))
   `(helm-ff-file ((t (:foreground ,text :background ,background :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,red :background ,background :inherit bold))))
   `(helm-ff-prefix ((t (:foreground ,background :background ,green :weight normal))))
   `(helm-ff-symlink ((t (:foreground ,light-blue :background ,background :inherit bold))))
   `(helm-grep-cmd-line ((t (:foreground ,text :background ,background))))
   `(helm-grep-file ((t (:foreground ,text :background ,background))))
   `(helm-grep-finish ((t (:foreground ,text :background ,background))))
   `(helm-grep-lineno ((t (:foreground ,text-4 :background ,background :inherit bold))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-header ((t (:foreground ,text :background ,background-3 :underline (:color ,dark-blue :style line)))))
   `(helm-header-line-left-margin ((t (:foreground ,green :background ,nil))))
   `(helm-match ((t (:background ,text-highlight :foreground ,text-2))))
   `(helm-match-item ((t (:background ,text-highlight :foreground ,intense-green))))
   `(helm-moccur-buffer ((t (:foreground ,text-3 :background ,background))))
   `(helm-selection ((t (:background ,text-region))))
   `(helm-selection-line ((t (:background ,background-2))))
   `(helm-separator ((t (:foreground ,text-2 :background ,background))))
   `(helm-source-header ((t (:foreground ,text :background ,background-3 :underline (:color ,dark-blue :style line)))))
   `(helm-time-zone-current ((t (:foreground ,green :background ,background))))
   `(helm-time-zone-home ((t (:foreground ,text-2 :background ,background))))
   `(helm-visible-mark ((t (:foreground ,green :background ,background-3))))

   ;; helm-swoop
   `(helm-swoop-target-line-block-face ((t (:foreground ,text :background ,text-highlight))))
   `(helm-swoop-target-line-face ((t (:background ,text-highlight))))
   `(helm-swoop-target-word-face ((t (:background ,text-highlight :foreground ,text-2))))

   `(term ((t (:foreground ,text))))
   `(term-color-black ((t (:foreground ,background))))
   `(term-color-red ((t (:foreground ,red))))
   `(term-color-green ((t (:foreground ,green))))
   `(term-color-yellow ((t (:foreground ,yellow))))
   `(term-color-blue ((t (:foreground ,blue))))
   `(term-color-magenta ((t (:foreground ,purple))))
   `(term-color-cyan ((t (:foreground ,blue))))
   `(term-color-white ((t (:foreground ,text))))

   ;; Lines
   `(linum ((t (:foreground ,text-4  :weight light :height 0.9))))
   `(line-number ((t (:inherit linum))))
   `(line-number-current-line ((t (:inherit linum))))
   `(fringe ((t (:background ,background-3 :foreground ,text-4))))
   `(left-margin ((t (nil))))
   `(hl-line ((t (:background ,background-4)))))


  (custom-theme-set-variables
   'seti
   `(cursor-type 'bar)))

;;;###autoload
(and load-file-name
  (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


(provide-theme 'seti)
;;; seti-theme.el ends here
