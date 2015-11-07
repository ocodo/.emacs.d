;;; Compiled snippets and support files for `snippet-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'snippet-mode
                     '(("header" "# -*- mode: snippet -*-\n# name: $1\n# group: $2\n# key: $3\n# keybinding: $5\n# expand-env: ($7)\n# contributor: $6\n# --\n$0" "Snippet header" nil nil nil "/Users/jason/.emacs.d/snippets/snippet-mode/header" nil nil)
                       ("choose" "\\${$1:$$(yas/choose-value '( ${2:choices} ))\\}$0" "choose" nil nil nil "/Users/jason/.emacs.d/snippets/snippet-mode/choose" nil nil)))


;;; Do not edit! File generated at Sat Nov  7 12:16:41 2015
