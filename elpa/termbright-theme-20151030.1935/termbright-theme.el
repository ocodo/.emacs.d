;;; termbright-theme.el --- a more usable theme for white-on-black terminals

;; Author: Brian Mastenbrook <brian@mastenbrook.net>
;; Keywords: themes
;; Package-Version: 20151030.1935
;; URL: https://github.com/bmastenbrook/termbright-theme-el
;; Version: 1.0
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;; This file was written by Brian Mastenbrook and is placed in the
;; public domain.

;;; Code:

(deftheme termbright
  "termbright, a more usable theme for white-on-black terminals")

(custom-theme-set-faces
 'termbright
 '(minibuffer-prompt ((t (:foreground "white" :weight bold))))
 '(highlight ((t (:foreground "black" :background "green"))))
 '(region ((t (:foreground "white" :background "blue"))))
 '(shadow ((t (:foreground "green"))))
 '(secondary-selection ((t (:foreground "black" :background "cyan"))))
 '(trailing-whitespace ((t (:background "red1"))))
 '(font-lock-builtin-face ((t (:weight bold :foreground "magenta"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "green" :weight bold))))
 '(font-lock-comment-face ((t (:weight bold :foreground "green"))))
 '(font-lock-constant-face ((t (:weight bold :foreground "magenta"))))
 '(font-lock-doc-face ((t (:foreground "grey"))))
 '(font-lock-function-name-face ((t (:weight bold :foreground "yellow"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "cyan"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground "magenta" :weight bold))))
 '(font-lock-regexp-grouping-backslash ((t (:weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:weight bold))))
 '(font-lock-string-face ((t (:foreground "grey"))))
 '(font-lock-type-face ((t (:foreground "yellow" :weight bold))))
 '(font-lock-variable-name-face ((t (:weight bold :foreground "red"))))
 '(font-lock-warning-face ((t (:foreground "red" :underline t :weight bold))))
 '(button ((t (:underline (:color foreground-color :style line)))))
 '(link ((t (:foreground "cyan" :underline t :weight bold))))
 '(link-visited ((t (:foreground "magenta4" :underline (:color foreground-color :style line)))))
 '(fringe ((t (:background "grey95"))))
 '(header-line ((t (:underline (:color foreground-color :style line) :inverse-video nil :background "blue" :foreground "white" :weight bold))))
 '(tooltip ((t (:foreground "black" :background "lightyellow" :family "helv"))))
 '(mode-line ((t (:weight bold :foreground "white" :background "blue"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:background "green" :foreground "black"))))
 '(mode-line-inactive ((t (:background "blue" :foreground "white" :weight bold))))
 '(isearch ((t (:foreground "cyan1" :background "magenta4"))))
 '(isearch-fail ((t (:background "red"))))
 '(lazy-highlight ((t (:background "turquoise3"))))
 '(match ((t (:foreground "black" :background "yellow"))))
 '(next-error ((t (:background "blue" :foreground "white"))))
 '(query-replace ((t (:background "magenta4" :foreground "cyan1")))))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun termbright-theme ()
  "Load the termbright-theme."
  (interactive)
  (load-theme 'termbright t))

(provide-theme 'termbright)

;;; termbright-theme.el ends here
