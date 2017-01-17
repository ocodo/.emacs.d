;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'quail)
(require 'gnu-apl-util)

;;; ' ( ) + , - . /  :  ; < = >  ? [ ]
;;; \ _ ¨ ¯ × ÷ ← ↑ → ↓ ∆ ∇ ∘ ∣ ∧ ∨
;;; ∩ ∪ ∼ ≠ ≤ ≥ ≬ ⊂ ⊃ ⌈ ⌊ ⊤ ⊥ ⋆ ⌶ ⌷
;;; ⌸ ⌹ ⌺ ⌻ ⌼ ⌽ ⌾ ⌿ ⍀ ⍁ ⍂ ⍃ ⍄ ⍅ ⍆ ⍇
;;; ⍈ ⍉ ⍊ ⍋ ⍌ ⍍ ⍎ ⍏ ⍐ ⍑ ⍒ ⍓ ⍔ ⍕ ⍖ ⍗
;;; ⍘ ⍙ ⍚ ⍛ ⍜ ⍝ ⍞ ⍟ ⍠ ⍡ ⍢ ⍣ ⍤ ⍥ ⍦ ⍧
;;; ⍨ ⍩ ⍪ ⍫ ⍬ ⍭ ⍮ ⍯ ⍰ ⍱ ⍲ ⍳ ⍴ ⍵ ⍶ ⍷
;;; ⍸ ⍹ ⍺ ⎕ ○

;;; Keymap based on the image available at: http://www.sudleyplace.com/APL/Keyboard.ahtml
;;; GNU APL keyboard layout: http://commons.wikimedia.org/wiki/File:GNU_APL_keyboard_layout.png

(defvar gnu-apl--symbols '(;; Top row
                           ;; `
                           ("diamond" "◊" "`")
                           ;; 1
                           ("diaeresis" "¨" "1")
                           ("i-beam" "⌶" "!")
                           ;; 2
                           ("macron" "¯" "2")
                           ("del-tilde" "⍫" "@")
                           ;; 3
                           ("less-than" "<" "3")
                           ("del-stile" "⍒" "#")
                           ;; 4
                           ("less-than-or-equal-to" "≤" "4")
                           ("delta-stile" "⍋" "$")
                           ;; 5
                           ("equals" "=" "5")
                           ("circle-stile" "⌽" "%")
                           ;; 6
                           ("greater-than-or-equal-to" "≥" "6")
                           ("circle-backslash" "⍉" "^")
                           ;; 7
                           ("greater-than" ">" "7")
                           ("circled-minus" "⊖" "&")
                           ;; 8
                           ("not-equal-to" "≠" "8")
                           ("circle-star" "⍟" "*")
                           ;; 9
                           ("logical-or" "∨" "9")
                           ("down-caret-tilde" "⍱" "(")
                           ;; 0
                           ("logical-and" "∧" "0")
                           ("up-caret-tilde" "⍲" ")")
                           ;; -
                           ("multiplication-sign" "×" "-")
                           ("exclamation-mark" "!" "_")
                           ;; =
                           ("division-sign" "÷" "=")
                           ("quad-divide" "⌹" "+")

                           ;; First row
                           ;; q
                           ("question-mark" "?" "q")
                           ;; w
                           ("omega" "⍵" "w")
                           ("omega-underbar" "⍹" "W")
                           ;; e
                           ("epsilon" "∊" "e")
                           ("epsilon-underbar" "⍷" "E")
                           ;; r
                           ("rho" "⍴" "r")
                           ;; t
                           ("tilde" "∼" "t")
                           ("tilde-diaeresis" "⍨" "T")
                           ;; y
                           ("uparrow" "↑" "y")
                           ("yen-sign" "¥" "Y")
                           ;; u
                           ("downarrow" "↓" "u")
                           ;; i
                           ("iota" "⍳" "i")
                           ("iota-underbar" "⍸" "I")
                           ;; o
                           ("circle" "○" "o")
                           ("circle-diaeresis" "⍥" "O")
                           ;; p
                           ("star-operator" "⋆" "p")
                           ("star-diaeresis" "⍣" "P")
                           ;; [
                           ("leftarrow" "←" "[")
                           ("quote-quad" "⍞" "{")
                           ;; ]
                           ("rightarrow" "→" "]")
                           ("zilde" "⍬" "}")
                           ;; \
                           ("right-tack" "⊢" "\\")
                           ("left-tack" "⊣" "|")

                           ;; Second row
                           ;; a
                           ("alpha" "⍺" "a")
                           ("alpha-underbar" "⍶" "A")
                           ;; s
                           ("left-ceiling" "⌈" "s")
                           ;; d
                           ("left-floor" "⌊" "d")
                           ;; f
                           ("underscore" "_" "f")
                           ("del-tilde" "⍫" "F")
                           ;; g
                           ("nabla" "∇" "g")
                           ;; h
                           ("increment" "∆" "h")
                           ("delta-underbar" "⍙" "H")
                           ;; j
                           ("ring-operator" "∘" "j")
                           ("jot-diaeresis" "⍤" "J")
                           ;; k
                           ("apostrophe" "'" "k")
                           ("quad-diamond" "⌺" "K")
                           ;; l
                           ("quad" "⎕" "l")
                           ("squish-quad" "⌷" "L")
                           ;; ;
                           ("down-tack-jot" "⍎" ";")
                           ("identical-to" "≡" ":")
                           ;; '
                           ("up-tack-jot" "⍕" "'")
                           ("not-identical-to" "≢" "\"")

                           ;; Third row
                           ;; z
                           ("subset-of" "⊂" "z")
                           ;; x
                           ("superset-of" "⊃" "x")
                           ("greek-letter-chi" "χ" "X")
                           ;; c
                           ("intersection" "∩" "c")
                           ("left-shoe-stile" "⍧" "C")
                           ;; v
                           ("union" "∪" "v")
                           ;; b
                           ("up-tack" "⊥" "b")
                           ("pound-sign" "£" "B")
                           ;; n
                           ("down-tack" "⊤" "n")
                           ;; m
                           ("divides" "|" "m")
                           ;; ,
                           ("shoe-jot" "⍝" ",")
                           ("comma-bar" "⍪" "<")
                           ;; .
                           ("backslash-bar" "⍀" ">")
                           ;; /
                           ("slash-bar" "⌿" "/")
                           ("quad-colon" "⍠" "?")
                           
                           ;; Extras
                           ("pi" "π")
                           ("root" "√")
                           ("inverted-exclamation-mark" "¡")
                           ("quad-backslash" "⍂")
                           ("inverted-question-mark" "¿")
                           ))

(provide 'gnu-apl-symbols)
