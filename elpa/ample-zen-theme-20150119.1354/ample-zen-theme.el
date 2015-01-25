;;; ample-zen-theme.el --- AmpleZen Theme for Emacs 24

;; Filename: ample-zen-theme.el
;; Description: A Calm and Low Contract Dark Theme for Emacs 24
;; Author: Michael Wall
;; Version: 20150119.1354
;; X-Original-Version: 0.3
;; URL: https://github.com/mjwall/ample-zen
;; Keywords: theme, dark, emacs 24

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

;; A combination of zenburn-emacs by Bozhidar Batsov and Ample Theme
;; by Jordon Biondo.  This theme is built with Emacs 24.  Based on version
;; 0.2.4 of ample-theme found at
;; https://github.com/jordonbiondo/ample-theme and version 2.1 of
;; zenburn-emacs found at http://github.com/bbatsov/zenburn-emacs

;; Mostly, I liked the colors of ample and the better highlighting and
;; and robustness of zenburn

;;; Code:

(deftheme ample-zen "The AmpleZen color theme")

;;; Color Palette

(defvar ample-zen-colors-alist
  '(("ample-zen-fg-1"     . "#c9c9c9")
    ("ample-zen-fg"       . "#bdbdb3")
    ("ample-zen-fg+1"     . "#9b9b9b")
    ("ample-zen-bg-2"     . "#4c4c4c")
    ("ample-zen-bg-1"     . "#3b3b3b")
    ("ample-zen-bg-05"    . "#2e2e2e")
    ("ample-zen-bg"       . "#212121")
    ("ample-zen-bg+1"     . "#141414")
    ("ample-zen-bg+2"     . "#0a0a0a")
    ("ample-zen-bg+3"     . "#000000")
    ("ample-zen-red+1"    . "#AA5542")
    ("ample-zen-red"      . "#CC5542")
    ("ample-zen-red-1"    . "#dd5542")
    ("ample-zen-red-2"    . "#ee5542")
    ("ample-zen-red-3"    . "#ff5542")
    ("ample-zen-red-4"    . "#ff6642")
    ("ample-zen-orange-1" . "#cc8512")
    ("ample-zen-orange"   . "#fb8512")
    ("ample-zen-yellow"   . "#7d7c61")
    ("ample-zen-yellow-1" . "#bdbc61")
    ("ample-zen-yellow-2" . "#baba36")
    ("ample-zen-green-1"  . "#6abd50")
    ("ample-zen-green"    . "#6aaf50")
    ("ample-zen-green+1"  . "#6aa350")
    ("ample-zen-green+2"  . "#6a9550")
    ("ample-zen-green+3"  . "#6a8550")
    ("ample-zen-green+4"  . "#6a7550")
    ("ample-zen-cyan"     . "#9b55c3")
    ("ample-zen-blue+1"   . "#6380b3")
    ("ample-zen-blue"     . "#5180b3")
    ("ample-zen-blue-1"   . "#528fd1")
    ("ample-zen-blue-2"   . "#6CA0A3")
    ("ample-zen-blue-3"   . "#5C888B")
    ("ample-zen-blue-4"   . "#4C7073")
    ("ample-zen-blue-5"   . "#366060")
    ("ample-zen-magenta"  . "#DC8CC3"))
  "List of Ample-Zen colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro ample-zen-with-color-variables (&rest body)
  "`let' bind all colors defined in `ample-zen-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   ample-zen-colors-alist))
     ,@body))

;;; Theme Faces
(ample-zen-with-color-variables
  (custom-theme-set-faces
   'ample-zen
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,ample-zen-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,ample-zen-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,ample-zen-fg :background ,ample-zen-bg))))
   `(cursor ((t (:foreground ,ample-zen-fg :background ,ample-zen-orange-1))))
   `(escape-glyph ((t (:foreground ,ample-zen-yellow :bold t))))
   `(fringe ((t (:foreground ,ample-zen-fg :background ,ample-zen-bg))))
   `(header-line ((t (:foreground ,ample-zen-yellow
                                  :background ,ample-zen-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,ample-zen-bg-05))))
   `(success ((t (:foreground ,ample-zen-green :weight bold))))
   `(warning ((t (:foreground ,ample-zen-orange :weight bold))))
   `(error ((t (:foreground ,ample-zen-red+1 :weight bold))))
   `(menu ((t (:foreground ,ample-zen-fg :background ,ample-zen-bg))))
   `(minibuffer-prompt ((t (:foreground ,ample-zen-yellow))))
   `(mode-line
     ((,class (:foreground ,ample-zen-fg-1
                           :background ,ample-zen-bg+3
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,ample-zen-orange-1 :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,ample-zen-fg+1
                      :background ,ample-zen-bg-1
                      :box nil :weight light))))
   `(region ((,class (:background ,ample-zen-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,ample-zen-bg+2))))
   `(trailing-whitespace ((t (:background ,ample-zen-red))))
   `(vertical-border ((t (:foreground ,ample-zen-fg))))
   `(scroll-bar ((t (:background ,ample-zen-bg+2 :foreground ,ample-zen-fg+1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,ample-zen-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,ample-zen-green))))
   `(compilation-error-face ((t (:foreground ,ample-zen-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,ample-zen-fg))))
   `(compilation-info-face ((t (:foreground ,ample-zen-blue))))
   `(compilation-info ((t (:foreground ,ample-zen-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,ample-zen-green))))
   `(compilation-line-face ((t (:foreground ,ample-zen-yellow))))
   `(compilation-line-number ((t (:foreground ,ample-zen-yellow))))
   `(compilation-message-face ((t (:foreground ,ample-zen-blue))))
   `(compilation-warning-face ((t (:foreground ,ample-zen-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,ample-zen-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,ample-zen-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,ample-zen-yellow :weight bold))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,ample-zen-fg))))
   `(grep-error-face ((t (:foreground ,ample-zen-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,ample-zen-blue))))
   `(grep-match-face ((t (:foreground ,ample-zen-orange :weight bold))))
   `(match ((t (:background ,ample-zen-bg-1 :foreground ,ample-zen-orange :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,ample-zen-yellow-2 :weight bold :background ,ample-zen-bg-1))))
   `(isearch-fail ((t (:foreground ,ample-zen-fg :background ,ample-zen-red-4))))
   `(lazy-highlight ((t (:foreground ,ample-zen-yellow-2 :weight bold :background ,ample-zen-bg-05))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,ample-zen-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,ample-zen-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,ample-zen-green-1))))
   `(font-lock-constant-face ((t (:foreground ,ample-zen-green+4))))
   `(font-lock-doc-face ((t (:foreground ,ample-zen-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,ample-zen-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,ample-zen-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,ample-zen-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,ample-zen-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,ample-zen-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,ample-zen-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,ample-zen-red))))
   `(font-lock-type-face ((t (:foreground ,ample-zen-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,ample-zen-orange))))
   `(font-lock-warning-face ((t (:foreground ,ample-zen-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;; Third-party
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,ample-zen-cyan :weight bold))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,ample-zen-fg))))
   `(ack-file ((t (:foreground ,ample-zen-blue))))
   `(ack-line ((t (:foreground ,ample-zen-yellow))))
   `(ack-match ((t (:foreground ,ample-zen-orange :background ,ample-zen-bg-1 :weight bold))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:inherit font-lock-warning))))
   `(font-latex-sectioning-5-face ((t (:foreground ,ample-zen-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,ample-zen-yellow))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,ample-zen-bg+3 :foreground ,ample-zen-bg-2))))
   `(ac-selection-face ((t (:background ,ample-zen-blue-4 :foreground ,ample-zen-fg))))
   `(popup-face ((t (:background "black" :foreground ,ample-zen-bg+3))))
   `(popup-tip-face ((t (:background ,ample-zen-orange-1 :foreground ,ample-zen-bg+3))))
   `(popup-scroll-bar-foreground-face ((t (:background ,ample-zen-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,ample-zen-bg-1))))
   `(popup-isearch-match ((t (:background ,ample-zen-bg :foreground ,ample-zen-fg))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,ample-zen-green+1))))
   `(android-mode-error-face ((t (:foreground ,ample-zen-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,ample-zen-fg))))
   `(android-mode-verbose-face ((t (:foreground ,ample-zen-green))))
   `(android-mode-warning-face ((t (:foreground ,ample-zen-yellow))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,ample-zen-orange :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,ample-zen-red :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,ample-zen-green+1 :weight bold :underline t))))
;;;;; diff
   `(diff-added ((,class (:foreground ,ample-zen-green+4 :background nil))
                 (t (:foreground ,ample-zen-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,ample-zen-yellow))))
   `(diff-removed ((,class (:foreground ,ample-zen-red :background nil))
                   (t (:foreground ,ample-zen-red-3 :background nil))))
   `(diff-refine-added ((t :inherit diff-added :weight bold)))
   `(diff-refine-change ((t :inherit diff-changed :weight bold)))
   `(diff-refine-removed ((t :inherit diff-removed :weight bold)))
   `(diff-header ((,class (:background ,ample-zen-bg+2))
                  (t (:background ,ample-zen-fg :foreground ,ample-zen-bg))))
   `(diff-file-header
     ((,class (:background ,ample-zen-bg+2 :foreground ,ample-zen-fg :bold t))
      (t (:background ,ample-zen-fg :foreground ,ample-zen-bg :bold t))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,ample-zen-fg :background ,ample-zen-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,ample-zen-fg :background ,ample-zen-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,ample-zen-fg :background ,ample-zen-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,ample-zen-fg :background ,ample-zen-blue-5))))
   `(ediff-even-diff-A ((t (:background ,ample-zen-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,ample-zen-bg+1))))
   `(ediff-even-diff-B ((t (:background ,ample-zen-bg+1))))
   `(ediff-even-diff-C ((t (:background ,ample-zen-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,ample-zen-fg :background ,ample-zen-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,ample-zen-fg :background ,ample-zen-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,ample-zen-fg :background ,ample-zen-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,ample-zen-fg :background ,ample-zen-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,ample-zen-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,ample-zen-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,ample-zen-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,ample-zen-bg+2))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,ample-zen-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,ample-zen-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment))))
   `(eshell-ls-directory ((t (:foreground ,ample-zen-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,ample-zen-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,ample-zen-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning))))
   `(eshell-ls-product ((t (:inherit font-lock-doc))))
   `(eshell-ls-special ((t (:foreground ,ample-zen-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,ample-zen-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,ample-zen-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,ample-zen-red) :inherit unspecified))
      (t (:foreground ,ample-zen-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,ample-zen-orange) :inherit unspecified))
      (t (:foreground ,ample-zen-orange :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,ample-zen-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,ample-zen-orange :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,ample-zen-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,ample-zen-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,ample-zen-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,ample-zen-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,ample-zen-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,ample-zen-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,ample-zen-orange) :inherit unspecified))
      (t (:foreground ,ample-zen-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,ample-zen-red) :inherit unspecified))
      (t (:foreground ,ample-zen-red-1 :weight bold :underline t))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,ample-zen-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
   `(erc-default-face ((t (:foreground ,ample-zen-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,ample-zen-yellow))))
   `(erc-keyword-face ((t (:foreground ,ample-zen-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,ample-zen-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,ample-zen-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,ample-zen-green))))
   `(erc-pal-face ((t (:foreground ,ample-zen-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,ample-zen-orange :background ,ample-zen-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,ample-zen-green+1))))
   `(erc-underline-face ((t (:underline t))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,ample-zen-green :weight bold))))
   `(git-gutter:deleted ((t (:foreground ,ample-zen-red :weight bold))))
   `(git-gutter:modified ((t (:foreground ,ample-zen-yellow-2 :weight bold))))
   `(git-gutter:unchanged ((t (:foreground ,ample-zen-fg :weight bold))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,ample-zen-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,ample-zen-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,ample-zen-magenta :weight bold))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,ample-zen-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,ample-zen-blue))))
   `(gnus-summary-high-read ((t (:foreground ,ample-zen-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,ample-zen-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,ample-zen-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,ample-zen-blue))))
   `(gnus-summary-low-read ((t (:foreground ,ample-zen-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,ample-zen-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,ample-zen-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,ample-zen-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,ample-zen-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,ample-zen-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,ample-zen-fg))))
   `(gnus-summary-selected ((t (:foreground ,ample-zen-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,ample-zen-blue))))
   `(gnus-cite-10 ((t (:foreground ,ample-zen-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,ample-zen-yellow))))
   `(gnus-cite-2 ((t (:foreground ,ample-zen-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,ample-zen-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,ample-zen-green+2))))
   `(gnus-cite-5 ((t (:foreground ,ample-zen-green+1))))
   `(gnus-cite-6 ((t (:foreground ,ample-zen-green))))
   `(gnus-cite-7 ((t (:foreground ,ample-zen-red))))
   `(gnus-cite-8 ((t (:foreground ,ample-zen-red-1))))
   `(gnus-cite-9 ((t (:foreground ,ample-zen-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,ample-zen-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,ample-zen-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,ample-zen-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,ample-zen-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,ample-zen-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,ample-zen-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,ample-zen-bg+2))))
   `(gnus-signature ((t (:foreground ,ample-zen-yellow))))
   `(gnus-x ((t (:background ,ample-zen-fg :foreground ,ample-zen-bg))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,ample-zen-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,ample-zen-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,ample-zen-bg+1))
                   (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,ample-zen-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,ample-zen-orange :weight bold))))
   `(ido-incomplete-regex ((t (:foreground ,ample-zen-red-4))))
   `(ido-subdir ((t (:foreground ,ample-zen-yellow))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,ample-zen-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,ample-zen-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,ample-zen-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,ample-zen-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,ample-zen-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,ample-zen-red+1))))
   `(jabber-activity-face((t (:foreground ,ample-zen-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,ample-zen-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; linum-mode
   `(linum ((t (:foreground ,ample-zen-green+2 :background ,ample-zen-bg))))
;;;;; magit
   `(magit-section-title ((t (:foreground ,ample-zen-yellow :weight bold))))
   `(magit-header ((t (:foreground ,ample-zen-yellow :weight bold))))
   `(magit-branch ((t (:foreground ,ample-zen-orange :weight bold))))
   `(magit-item-highlight ((t (:background ,ample-zen-bg+1 :bold nil))))
   `(magit-log-sha1 ((t (:foreground ,ample-zen-blue-5))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment))))
   `(message-header-name ((t (:foreground ,ample-zen-green+1))))
   `(message-header-other ((t (:foreground ,ample-zen-green))))
   `(message-header-to ((t (:foreground ,ample-zen-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,ample-zen-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,ample-zen-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,ample-zen-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,ample-zen-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,ample-zen-green))))
   `(message-mml ((t (:foreground ,ample-zen-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,ample-zen-cyan :background ,ample-zen-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,ample-zen-bg :background ,ample-zen-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,ample-zen-bg :background ,ample-zen-red :weight bold))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,ample-zen-yellow))))
   `(nav-face-button-num ((t (:foreground ,ample-zen-cyan))))
   `(nav-face-dir ((t (:foreground ,ample-zen-green))))
   `(nav-face-hdir ((t (:foreground ,ample-zen-red))))
   `(nav-face-file ((t (:foreground ,ample-zen-fg))))
   `(nav-face-hfile ((t (:foreground ,ample-zen-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,ample-zen-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,ample-zen-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,ample-zen-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,ample-zen-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,ample-zen-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,ample-zen-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,ample-zen-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,ample-zen-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,ample-zen-bg+3 :strike-through t))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,ample-zen-fg :weight bold))))
   `(org-checkbox ((t (:background ,ample-zen-bg+2 :foreground "white"
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,ample-zen-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,ample-zen-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,ample-zen-green+3))))
   `(org-formula ((t (:foreground ,ample-zen-yellow-2))))
   `(org-headline-done ((t (:foreground ,ample-zen-green+3))))
   `(org-hide ((t (:foreground ,ample-zen-bg-1))))
   `(org-level-1 ((t (:foreground ,ample-zen-orange))))
   `(org-level-2 ((t (:foreground ,ample-zen-green+4))))
   `(org-level-3 ((t (:foreground ,ample-zen-blue-1))))
   `(org-level-4 ((t (:foreground ,ample-zen-yellow-2))))
   `(org-level-5 ((t (:foreground ,ample-zen-cyan))))
   `(org-level-6 ((t (:foreground ,ample-zen-green+2))))
   `(org-level-7 ((t (:foreground ,ample-zen-red-4))))
   `(org-level-8 ((t (:foreground ,ample-zen-blue-4))))
   `(org-link ((t (:foreground ,ample-zen-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,ample-zen-green+4))))
   `(org-scheduled-previously ((t (:foreground ,ample-zen-red-4))))
   `(org-scheduled-today ((t (:foreground ,ample-zen-blue+1))))
   `(org-sexp-date ((t (:foreground ,ample-zen-blue+1 :underline t))))
   `(org-special-keyword ((t (:foreground ,ample-zen-fg-1 :weight normal))))
   `(org-table ((t (:foreground ,ample-zen-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,ample-zen-orange))))
   `(org-todo ((t (:bold t :foreground ,ample-zen-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,ample-zen-red :weight bold :underline nil))))
   `(org-column ((t (:background ,ample-zen-bg-1))))
   `(org-column-title ((t (:background ,ample-zen-bg-1 :underline t :weight bold))))
;;;;; outline
   `(outline-1 ((t (:foreground ,ample-zen-orange))))
   `(outline-2 ((t (:foreground ,ample-zen-green+4))))
   `(outline-3 ((t (:foreground ,ample-zen-blue-1))))
   `(outline-4 ((t (:foreground ,ample-zen-yellow-2))))
   `(outline-5 ((t (:foreground ,ample-zen-cyan))))
   `(outline-6 ((t (:foreground ,ample-zen-green+2))))
   `(outline-7 ((t (:foreground ,ample-zen-red-4))))
   `(outline-8 ((t (:foreground ,ample-zen-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; powerline
   `(powerline-active1 ((t (:background ,ample-zen-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,ample-zen-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,ample-zen-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,ample-zen-bg+3 :inherit mode-line-inactive))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,ample-zen-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,ample-zen-green+2))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,ample-zen-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,ample-zen-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,ample-zen-green-1))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,ample-zen-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,ample-zen-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,ample-zen-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,ample-zen-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,ample-zen-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,ample-zen-green))))
   `( rainbow-delimiters-depth-12-face ((t (:foreground ,ample-zen-blue-5))))
;;;;; rbenv
   `(rbenv-active-ruby-face ((t (:foreground ,ample-zen-fg))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,ample-zen-blue))))
   `(rcirc-other-nick ((t (:foreground ,ample-zen-orange))))
   `(rcirc-bright-nick ((t (:foreground ,ample-zen-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,ample-zen-blue-2))))
   `(rcirc-server ((t (:foreground ,ample-zen-green))))
   `(rcirc-server-prefix ((t (:foreground ,ample-zen-green+1))))
   `(rcirc-timestamp ((t (:foreground ,ample-zen-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,ample-zen-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,ample-zen-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,ample-zen-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,ample-zen-green))))
   `(rpm-spec-doc-face ((t (:foreground ,ample-zen-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,ample-zen-red))))
   `(rpm-spec-macro-face ((t (:foreground ,ample-zen-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,ample-zen-red))))
   `(rpm-spec-package-face ((t (:foreground ,ample-zen-red))))
   `(rpm-spec-section-face ((t (:foreground ,ample-zen-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,ample-zen-blue))))
   `(rpm-spec-var-face ((t (:foreground ,ample-zen-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,ample-zen-orange))))
   `(rst-level-2-face ((t (:foreground ,ample-zen-green+1))))
   `(rst-level-3-face ((t (:foreground ,ample-zen-blue-1))))
   `(rst-level-4-face ((t (:foreground ,ample-zen-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,ample-zen-cyan))))
   `(rst-level-6-face ((t (:foreground ,ample-zen-green-1))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,ample-zen-red-3 :background ,ample-zen-bg :weight bold))))
   `(show-paren-match ((t (:foreground ,ample-zen-blue-1 :background ,ample-zen-bg :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-inputed-output-face ((t (:foreground ,ample-zen-red))))
;;;;; term
   `(term-color-black ((t (:foreground ,ample-zen-bg
                                       :background ,ample-zen-bg-1))))
   `(term-color-red ((t (:foreground ,ample-zen-red-2
                                       :background ,ample-zen-red-4))))
   `(term-color-green ((t (:foreground ,ample-zen-green
                                       :background ,ample-zen-green+2))))
   `(term-color-yellow ((t (:foreground ,ample-zen-orange
                                       :background ,ample-zen-yellow))))
   `(term-color-blue ((t (:foreground ,ample-zen-blue-1
                                      :background ,ample-zen-blue-4))))
   `(term-color-magenta ((t (:foreground ,ample-zen-magenta
                                         :background ,ample-zen-red))))
   `(term-color-cyan ((t (:foreground ,ample-zen-cyan
                                       :background ,ample-zen-blue))))
   `(term-color-white ((t (:foreground ,ample-zen-fg
                                       :background ,ample-zen-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,ample-zen-bg-05))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,ample-zen-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,ample-zen-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,ample-zen-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,ample-zen-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,ample-zen-green+2 :background ,ample-zen-bg))))
   `(w3m-lnum-match ((t (:background ,ample-zen-bg-1
                                     :foreground ,ample-zen-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,ample-zen-yellow))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,ample-zen-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,ample-zen-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,ample-zen-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,ample-zen-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,ample-zen-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,ample-zen-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,ample-zen-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,ample-zen-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,ample-zen-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,ample-zen-bg+1 :foreground ,ample-zen-bg+1))))
   `(whitespace-hspace ((t (:background ,ample-zen-bg+1 :foreground ,ample-zen-bg+1))))
   `(whitespace-tab ((t (:background ,ample-zen-red-1))))
   `(whitespace-newline ((t (:foreground ,ample-zen-bg+1))))
   `(whitespace-trailing ((t (:background ,ample-zen-red))))
   `(whitespace-line ((t (:background ,ample-zen-bg :foreground ,ample-zen-magenta))))
   `(whitespace-space-before-tab ((t (:background ,ample-zen-orange :foreground ,ample-zen-orange))))
   `(whitespace-indentation ((t (:background ,ample-zen-yellow :foreground ,ample-zen-red))))
   `(whitespace-empty ((t (:background ,ample-zen-yellow))))
   `(whitespace-space-after-tab ((t (:background ,ample-zen-yellow :foreground ,ample-zen-red))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,ample-zen-green+4))))
))

;;; Theme Variables
(ample-zen-with-color-variables
  (custom-theme-set-variables
   'ample-zen
;;;;; ansi-color
   `(ansi-color-names-vector [,ample-zen-bg ,ample-zen-red ,ample-zen-green ,ample-zen-yellow
                                          ,ample-zen-blue ,ample-zen-magenta ,ample-zen-cyan ,ample-zen-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,ample-zen-bg-05)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,ample-zen-red-1)
       ( 40. . ,ample-zen-red)
       ( 60. . ,ample-zen-orange)
       ( 80. . ,ample-zen-yellow-2)
       (100. . ,ample-zen-yellow-1)
       (120. . ,ample-zen-yellow)
       (140. . ,ample-zen-green-1)
       (160. . ,ample-zen-green)
       (180. . ,ample-zen-green+1)
       (200. . ,ample-zen-green+2)
       (220. . ,ample-zen-green+3)
       (240. . ,ample-zen-green+4)
       (260. . ,ample-zen-cyan)
       (280. . ,ample-zen-blue-2)
       (300. . ,ample-zen-blue-1)
       (320. . ,ample-zen-blue)
       (340. . ,ample-zen-blue+1)
       (360. . ,ample-zen-magenta)))
   `(vc-annotate-very-old-color ,ample-zen-magenta)
   `(vc-annotate-background ,ample-zen-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar ample-zen-add-font-lock-keywords nil
  "Whether to add font-lock keywords for ample-zen color names.
In buffers visiting library `ample-zen-theme.el' the ample-zen
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar ample-zen-colors-font-lock-keywords nil)

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'ample-zen)

;;;###autoload
(add-to-list 'safe-local-eval-forms
             '(when (require 'rainbow-mode nil t) (rainbow-mode 1)))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; ample-zen-theme.el ends here
