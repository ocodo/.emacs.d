(deftheme high-life
  "A light-on-dark theme with background tints.")

(let ((background  "#141414")
      (foreground "#dcdded")
      (selection "#1F1414")
      (hl-line "#111926")
      (cursor "#b23f1e")
      (comment "#00686E")
      (gray-1 "#878289")   (gray-1bg "#181818")
      (gray-2 "#2a3441")   (gray-2bg "#202020")
      (gray-3 "#b3adb4")   (gray-3bg "#0e1116")
      (gray-4 "#1f2730")
      (gray-5 "#242d38")
      (gray-6 "#192028")
      (red-1    "#682810" )    (red-1bg "#190F0F")
      (red-2    "#591F0F" )    (red-2bg "#190E0E")
      (brown-1  "#8F410E" )  (brown-1bg "#190F0F")
      (orange-1 "#aC3D1A" ) (orange-1bg "#191011")
      (yellow-1 "#aF771F" ) (yellow-1bg "#191410")
      (green-1  "#40571A" )  (green-1bg "#0D1910")
      (green-2  "#274F3A" )  (green-2bg "#0D1910")
      (blue-1   "#3F4F64" )   (blue-1bg "#0F1219")
      (blue-2   "#203A4C" )   (blue-2bg "#0D1926")
      (blue-3   "#004A4F" )   (blue-3bg "#091119")
      (blue-4   "#1A2F3D" )   (blue-4bg "#0B1019")
      (purple-1 "#543C5A" ) (purple-1bg "#12111a"))

  (custom-theme-set-faces
   'high-life

   ;; Basics
   `(default ((t (:background ,background :foreground ,foreground))))
   `(cursor ((t (:background ,cursor))))
   `(region ((t (:background ,selection))))
   `(highlight ((t (:foreground ,blue-3 :background ,blue-3bg))))
   `(hl-line ((t (:background ,hl-line))))
   `(minibuffer-prompt ((t (:foreground ,orange-1 :background ,orange-1bg))))
   `(escape-glyph ((t (:foreground ,purple-1 :background , purple-1bg))))

   ;; Font-lock stuff
   `(font-lock-builtin-face ((t (:foreground ,yellow-1 :background ,yellow-1bg))))
   `(font-lock-constant-face ((t (:foreground ,purple-1 :background ,purple-1bg))))
   `(font-lock-comment-face ((t (:foreground ,comment :background ,gray-2bg :italic t))))
   `(font-lock-doc-face ((t (:foreground ,gray-1 :background ,gray-1bg))))
   `(font-lock-doc-string-face ((t (:foreground ,gray-1 :background ,gray-1bg))))
   `(font-lock-function-name-face ((t (:foreground ,red-1 :background ,red-1bg))))
   `(font-lock-keyword-face ((t (:foreground ,orange-1 :background ,orange-1bg))))
   `(font-lock-negation-char-face ((t (:foreground ,yellow-1 :background ,yellow-1bg))))
   `(font-lock-preprocessor-face ((t (:foreground ,orange-1 :background ,orange-1bg))))
   `(font-lock-string-face ((t (:foreground ,green-1 :background ,green-1bg))))
   `(font-lock-type-face ((t (:foreground ,red-2 :background ,red-2bg :bold nil))))
   `(font-lock-variable-name-face ((t (:foreground ,blue-1 :background ,blue-1bg))))
   `(font-lock-warning-face ((t (:foreground ,red-2 :background ,red-2bg))))

   ;; UI related
   `(link ((t (:foreground ,blue-1 :background ,blue-1bg))))
   `(fringe ((t (:background ,gray-2bg))))

   ;; Linum
   `(linum ((t (:foreground ,gray-6 :background ,gray-1bg))))

   `(mode-line ((t (:foreground ,blue-1 :background ,blue-2bg :font "Lucida Grande"))))
   `(mode-line-inactive ((t (:foreground ,blue-4 :background ,gray-4))))

   `(vertical-border ((t (:background ,background :foreground ,gray-5))))


   ;; show-paren-mode
   `(show-paren-match ((t (:foreground ,orange-1 :background ,orange-1bg))))
   `(show-paren-mismatch ((t (:foreground ,red-2bg :background ,red-2))))

   ;; ido
   `(ido-only-match ((t (:foreground ,green-1 :background ,green-1bg))))
   `(ido-subdir ((t (:foreground ,purple-1 :background ,purple-1bg))))

   ;; whitespace-mode
   `(whitespace-empty ((t (:foreground ,yellow-1bg :background ,yellow-1))))
   `(whitespace-hspace ((t (:foreground ,gray-2))))
   `(whitespace-indentation ((t (:foreground ,gray-2))))
   `(whitespace-line ((t (:background ,gray-2))))
   `(whitespace-newline ((t (:foreground ,gray-2))))
   `(whitespace-space ((t (:foreground ,gray-2))))
   `(whitespace-space-after-tab ((t (:foreground ,gray-2))))
   `(whitespace-tab ((t (:foreground ,gray-2))))
   `(whitespace-trailing ((t (:foreground ,red-1bg :background ,red-1))))

   ;; flyspell-mode
   `(flyspell-incorrect ((t (:underline ,red-2))))
   `(flyspell-duplicate ((t (:underline ,red-2))))

   ;; magit
   `(magit-diff-add ((t (:foreground ,green-1))))
   `(magit-diff-del ((t (:foreground ,red-2))))
   `(magit-item-highlight ((t (:background ,gray-1bg))))

   ;; highlight-indentation-mode
   `(highlight-indentation-face ((t (:background ,gray-1bg))))
   `(highlight-indentation-current-column-face ((t (:background ,gray-4))))

   ;; ECB
   `(ecb-default-general-face ((t (:foreground ,gray-3 :background ,gray-1bg))))
   `(ecb-default-highlight-face ((t (:foreground ,red-1 :background ,red-1bg))))
   `(ecb-method-face ((t (:foreground ,red-1 :background ,red-1bg))))
   `(ecb-tag-header-face ((t (:background ,blue-2bg))))

   ;; org-mode
   `(org-date ((t (:foreground ,purple-1 :background ,purple-1bg))))
   `(org-done ((t (:foreground ,green-1 :background ,green-1bg))))
   `(org-hide ((t (:foreground ,gray-2 :background ,gray-1bg))))
   `(org-link ((t (:foreground ,blue-1 :background ,blue-1bg))))
   `(org-todo ((t (:foreground ,red-1 :background ,red-1bg))))
   )

  (custom-theme-set-variables
   'high-life

   ;; Powerline
   `(powerline-color1 ,gray-5)
   `(powerline-color2 ,gray-6)

   ;; ;; Fill Column Indicator mode
   `(fci-rule-color ,gray-6)
   `(fci-rule-character-color ,gray-6)

   `(ansi-color-names-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [,background ,red-1 ,green-1 ,yellow-1 ,blue-1 ,purple-1 ,blue-1 ,foreground])
   `(ansi-term-color-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [unspecified ,background ,red-1 ,green-1 ,yellow-1 ,blue-1 ,purple-1 ,blue-1 ,foreground])
   )
  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'high-life)
