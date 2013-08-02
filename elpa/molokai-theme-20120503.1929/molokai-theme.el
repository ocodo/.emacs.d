(deftheme molokai
;; Version: 20120503.1929
  "emacs24 port of molokai for vim")

(custom-theme-set-faces
 'molokai
 '(cursor ((t (:background "#f8f8f0"))))
 '(font-lock-builtin-face ((t (:foreground "#ae81ff"))))
 '(font-lock-function-name-face ((t (:foreground "#a6e22e"))))
 '(font-lock-type-face ((t (:foreground "#66d9ef"))))
 '(font-lock-warning-face ((t (:inherit error))))
 '(error ((t (:background "#1e0010" :foreground "#960050"))))
 '(font-lock-keyword-face ((t (:foreground "#f92672"))))
 '(font-lock-delimiter-face ((t (:foreground "#8f8f8f"))))
 '(font-lock-constant-face ((t (:foreground "#fd971f"))))
 '(font-lock-string-face ((t (:foreground "#e6db74"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "#465457"))))
 '(show-paren-match ((t (:background "#fd971f" :foreground "#000000"))))
 '(esk-paren-face ((t (:foreground "grey50"))))
 '(whitespace-tab ((t (:background "#272822"))))
 '(mode-line ((t (:background "#808080" :foreground "#000000" :box nil))))
 '(mode-line-inactive ((t (:inherit (mode-line) :background "#080808" :foreground "#75715e" :box nil))))
 '(highlight ((t (:background "#403d3d" :inverse-video t))))
 '(region ((t (:inherit highlight :background "#49483e" :inverse-video nil))))
 '(markdown-header-delimiter-face ((t (:inherit font-lock-delimiter-face))))
 '(markdown-list-face ((t (:inherit font-lock-keyword-face))))
 '(markdown-link-face ((t (:foreground "#808080" :underline t))))
 '(markdown-header-face ((t (:foreground "#ef5939" :weight bold))))
 '(markdown-url-face ((t (:inherit font-lock-builtin-face))))
 '(hl-line ((t (:background "#293739" :inverse-video nil))))
 '(font-lock-number-face ((t (:foreground "#ae81ff"))))
 '(font-lock-variable-name-face ((t (:foreground "#fd971f"))))
 '(font-lock-preprocessor-face ((t (:foreground "#a6e22e"))))
 '(default ((t (:background "#1b1d1e" :foreground "#f8f8f2")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'molokai)

;;; molokai-theme.el ends here
