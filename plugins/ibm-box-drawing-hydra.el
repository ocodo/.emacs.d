;;; ibm-box-drawing-hydra --- draw DOS style boxes with a hydra

;;; Commentary:
;; Use M-x load-library ibm-box-drawing-hydra (after placing this file in the Emacs lisp load-path)

;; Draw box with IBM single line box characters. - activate with C-x d
;;
;; Press Esc to exit.
;;
;; Essentially treats the left hand on a QWERTY layout
;; as the box (i.e. QWE,ASD,ZXC)
;; Horizontal and vertical lines are on R -> horz and V -> vert
;; All other keys are available so you can move and
;; space as required.  The bindings are active until you hit Esc.
;;
;; Full bindings here.
;; q -> top left     ┌
;; w -> top          ┬
;; e -> top right    ┐
;; a -> left         ├
;; s -> center       ┼
;; d -> right        ┤
;; z -> bottom left  └
;; x -> bottom       ┴
;; c -> bottom right ┘
;; r -> horizontal   ─
;; v -> vertical     │

;; You can use Artist mode for things like this too, this is just yet another way. I built it for me.

;;; Licence: GPL v3

;;; Code:

(global-set-key (kbd "C-x d")
                (defhydra hydra-draw-box (:color pink)
                  "Draw box with IBM single line box characters (ESC to Quit)."
                  ("ESC" nil :color blue) ;; Esc to exit.
                  ("q" (insert "┌") "top left ┌")
                  ("w" (insert "┬") "top ┬")
                  ("e" (insert "┐") "top right ┐")
                  ("a" (insert "├") "left ├")
                  ("s" (insert "┼") "center ┼")
                  ("d" (insert "┤") "right ┤")
                  ("z" (insert "└") "bottom left └")
                  ("x" (insert "┴") "bottom ┴")
                  ("c" (insert "┘") "bottom right ┘")
                  ("r" (insert "─") "horizontal ─")
                  ("v" (insert "│") "vertical │")))

(provide 'ibm-box-drawing-hydra)

;;; ibm-box-drawing-hydra.el ends here
