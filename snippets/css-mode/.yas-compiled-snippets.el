;;; Compiled snippets and support files for `css-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'css-mode
                     '(("wi" "width: $1px;" "width" nil nil nil "/Users/jason/.emacs.d/snippets/css-mode/wi" nil nil)
                       ("va" "vertical-align: ${1:$$(yas/choose-value '(\" px\" \"%\" \"baseline\" \"sub\" \"super\" \"top\" \"text-top\" \"middle\" \"bottom\" \"text-bottom\" ))};" "vertical align" nil nil nil "/Users/jason/.emacs.d/snippets/css-mode/va" nil nil)
                       ("tt" "text-transform: ${1:$$(yas/choose-value '( \"none\" \"capitalize\" \"uppercase\" \"lowercase\" \"inherit\" ))}\n" "text transform" nil nil nil "/Users/jason/.emacs.d/snippets/css-mode/tt" nil nil)
                       ("td" "text-decoration: ${1:$$(yas/choose-value '(\"none\" \"underline\" \"overline\" \"line-through\" \"blink\" \"inherit\"))};\n\n" "text-decoration" nil nil nil "/Users/jason/.emacs.d/snippets/css-mode/td" nil nil)
                       ("ta" "text-align: ${1:$$(yas/choose-value '( \"center\" \"left\" \"right\" ))};" "text-align" nil nil nil "/Users/jason/.emacs.d/snippets/css-mode/ta" nil nil)
                       ("sel" "# yasnippet: css-mode\n# name: selector\n# -- \n${1:selector} {\n  $0\n}" "sel" nil nil nil "/Users/jason/.emacs.d/snippets/css-mode/sel" nil nil)
                       ("pos" "position: ${1:$$(yas/choose-value '( \"absolute\" \"fixed\" \"relative\" \"static\" \"inherit\" ))};" "position" nil nil nil "/Users/jason/.emacs.d/snippets/css-mode/pos" nil nil)
                       ("he" "height: $1px;" "height" nil nil nil "/Users/jason/.emacs.d/snippets/css-mode/he" nil nil)
                       ("fw" "font-weight: ${1:$$(yas/choose-value '( \"bold\" \"normal\" \"bolder\" \"lighter\" ))};" "font-weight" nil nil nil "/Users/jason/.emacs.d/snippets/css-mode/fw" nil nil)
                       ("fl" "float: ${1:$$(yas/choose-value '( \"left\" \"right\" \"none\" \"inherit\" ))};" "float" nil nil nil "/Users/jason/.emacs.d/snippets/css-mode/fl" nil nil)
                       ("col" "color: #$1;" "color" nil nil nil "/Users/jason/.emacs.d/snippets/css-mode/col" nil nil)
                       ("bor" "border${1:$$(yas/choose-value '( \"\" \"-top\" \"-right\" \"-bottom\" \"-left\" ))}: ${2:size}px ${3:color} ${4:$$(yas/choose-value '( \"dotted\" \"dashed\" \"solid\" \"double\" \"groove\" \"ridge\" \"inset\" \"outset\" ))};" "border" nil nil nil "/Users/jason/.emacs.d/snippets/css-mode/bor" nil nil)
                       ("bl" "${1:block}\n{\n  $0\n}" "block" nil nil nil "/Users/jason/.emacs.d/snippets/css-mode/bl" nil nil)))


;;; Do not edit! File generated at Sat Nov  7 12:16:41 2015
