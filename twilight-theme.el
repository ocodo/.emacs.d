(defconst tw-macp (eq system-type 'darwin) "Are we running in Mac OS?")

(defun color-sRGB (color-tuple)
  (car color-tuple))

(defun color-genRGB (color-tuple)
  (car (cdr color-tuple)))

(defun tw-color (color-tuple)
  (if tw-macp
      (color-genRGB color-tuple)
    (color-sRGB color-tuple)))

(deftheme twilight
  "Inspired by the color scheme of deviantart.com (v2)")

;; Colors, in (sRGB GenericRGB)
(defvar tw-default-bg '("#2e3735" "#141414"))
(defvar tw-default-fg '("#b6beb4" "#F8F8F8"))
(defvar tw-cursor-bg '("#c73a7c" "#A7A7A7"))
(defvar tw-cursor-fg tw-default-bg)
(defvar tw-region-bg '("#afc81c" "#4f4f4f"))
(defvar tw-region-fg tw-default-fg)
(defvar tw-modeline-bg '("#3e4745" "#2b2b2b"))
(defvar tw-modeline-fg '("#3e4745" "#8fb28f"))
(defvar tw-modeline-inact-bg '("#666e64" "#2b2b2b"))
(defvar tw-modeline-inact-fg '("#666e64" "#5f7f5f"))
(defvar tw-fringe-bg tw-default-bg)
(defvar tw-mb-prompt-fg tw-region-bg)

(defvar tw-builtin-fg '("#d5b613" "#CACACA"))
(defvar tw-comment-delimiter-fg '("#706a53" "#5F5A60"))
(defvar tw-comment-fg '("#706a53" "#5F5A60"))
(defvar tw-constant-fg '("#bebeb4" "#CF6A4C"))
(defvar tw-doc-fg '("#706a53" "DarkOrange"))
(defvar tw-doc-string-fg '("#706a53" "#94bff3"))
(defvar tw-function-fg '("#bebeb4" "#9B703F"))
(defvar tw-keyword-fg '("#57a6ff" "#CDA869"))
(defvar tw-negation-char-fg '("#bebeb4" "#dcdccc"))
(defvar tw-preprocessor-fg '("#bebeb4" "Aquamarine"))
(defvar tw-reference-fg '("#bebeb4" "SlateBlue"))
(defvar tw-regexp-grouping-backslash-fg '("#bebeb4" "#E9C062"))
(defvar tw-regexp-grouping-construct-fg '("#bebeb4" "red"))
(defvar tw-string-fg '("#e3795c" "#8F9D6A"))
(defvar tw-type-fg '("#68b91a" "#9B703F"))
(defvar tw-var-fg '("#68b91a" "#7587A6"))
(defvar tw-warn-fg '("#c73a7c" "#b0256c"))
(defvar tw-link-fg '("#599bb0" "#4f89a3"))
(defvar tw-search-bg tw-link-fg)
(defvar tw-search-fg tw-default-bg)
(defvar tw-lazy-hl-bg '("#465451" "#37433f"))
(defvar tw-link-old-fg '("#818780" "#6f746c"))
(defvar tw-button-bg '("#242b2a" "#1c201f"))
(defvar tw-header-bg '("#3e4745" "#303735"))
(defvar tw-header-fg '("#868e84" "#747c70"))

(custom-theme-set-variables
 'twilight
 '(linum-format "%7d")
 '(powerline-color1 "#222")
 '(powerline-color2 "#333")
 '(fringe-mode 9)
)

(custom-theme-set-faces
 'twilight

 `(link ((t (:foreground ,(tw-color tw-link-fg) :underline t))))
 `(link-visited ((t (:foreground ,(tw-color tw-link-old-fg) :underline t))))
 `(button ((t (:background ,(tw-color tw-button-bg) :underline t))))

 ;;; basic coloring
 `(default ((t (:background ,(tw-color tw-default-bg)
                            :foreground ,(tw-color tw-default-fg)))))
 `(cursor ((t (:background ,(tw-color tw-cursor-bg)
                           :foreground ,(tw-color tw-cursor-fg)))))
 `(fringe ((t (:background "#1a1a1a"))))

 `(linum ((t (:background  "#181818" :foreground "#333"))))

 `(highlight ((t (:background ,(tw-color tw-cursor-bg)))))

 `(region ((t (:background ,(tw-color tw-region-bg)
                           :foreground ,(tw-color tw-region-fg)))))

 `(mode-line ((t (:background ,(tw-color tw-modeline-bg)
                              :foreground ,(tw-color tw-modeline-fg)))))
 `(mode-line-inactive ((t (:background ,(tw-color tw-modeline-inact-bg)
                                       :foreground ,(tw-color tw-modeline-inact-fg)))))
 `(minibuffer-prompt ((t (:foreground ,(tw-color tw-mb-prompt-fg)))))

 `(font-lock-builtin-face ((t (:foreground ,(tw-color tw-builtin-fg)))))
 `(font-lock-comment-delimiter-face ((t (:foreground ,(tw-color tw-comment-delimiter-fg)))))
 `(font-lock-comment-face ((t (:foreground ,(tw-color tw-comment-fg)))))
 `(font-lock-constant-face ((t (:foreground ,(tw-color tw-constant-fg)))))
 `(font-lock-doc-face ((t (:foreground ,(tw-color tw-doc-fg)))))
 `(font-lock-doc-string-face ((t (:foreground ,(tw-color tw-doc-string-fg)))))
 `(font-lock-function-name-face ((t (:foreground ,(tw-color tw-function-fg)))))
 `(font-lock-keyword-face ((t (:foreground ,(tw-color tw-keyword-fg)))))
 `(font-lock-negation-char-face ((t (:foreground ,(tw-color tw-negation-char-fg)))))
 `(font-lock-preprocessor-face ((t (:foreground ,(tw-color tw-preprocessor-fg)))))
 `(font-lock-reference-face ((t (:foreground ,(tw-color tw-reference-fg)))))
 `(font-lock-regexp-grouping-backslash ((t (:foreground ,(tw-color tw-regexp-grouping-backslash-fg)))))
 `(font-lock-regexp-grouping-construct ((t (:foreground ,(tw-color tw-regexp-grouping-construct-fg)))))
 `(font-lock-string-face ((t (:foreground ,(tw-color tw-string-fg)))))
 `(font-lock-type-face ((t (:foreground ,(tw-color tw-type-fg)))))
 `(font-lock-variable-name-face ((t (:foreground ,(tw-color tw-var-fg)))))
 `(font-lock-warning-face ((t (:foreground ,(tw-color tw-warn-fg)))))

 `(isearch ((t (:background ,(tw-color tw-search-bg)
                            :foreground ,(tw-color tw-search-fg)))))
 `(lazy-highlight ((t (:background ,(tw-color tw-lazy-hl-bg)))))
 `(header-line ((t (:background ,(tw-color tw-header-bg)
                                :foreground ,(tw-color tw-header-fg)))))

 )

;; Rainbow delimiters
(defun tw-rainbow-delim-set-face ()
  (set-face-attribute
   'rainbow-delimiters-depth-1-face nil
   :foreground (tw-color tw-default-fg))
  (set-face-attribute
   'rainbow-delimiters-depth-2-face nil
   :foreground (tw-color tw-builtin-fg))
  (set-face-attribute
   'rainbow-delimiters-depth-3-face nil
   :foreground (tw-color tw-keyword-fg))
  (set-face-attribute
   'rainbow-delimiters-depth-4-face nil
   :foreground (tw-color tw-string-fg))
  (set-face-attribute
   'rainbow-delimiters-depth-5-face nil
   :foreground (tw-color tw-region-bg))
  (set-face-attribute
   'rainbow-delimiters-depth-6-face nil
   :foreground (tw-color tw-link-fg))
  (set-face-attribute
   'rainbow-delimiters-depth-7-face nil
   :foreground (tw-color tw-comment-fg))
  (set-face-attribute
   'rainbow-delimiters-unmatched-face nil
   :foreground (tw-color tw-warn-fg)))

(eval-after-load "rainbow-delimiters" '(tw-rainbow-delim-set-face))

(provide-theme 'twilight)
