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
                  ("q" (lambda () (interactive) (insert "┌")) "top left ┌")
                  ("w" (lambda () (interactive) (insert "┬")) "top ┬")
                  ("e" (lambda () (interactive) (insert "┐")) "top right ┐")
                  ("a" (lambda () (interactive) (insert "├")) "left ├")
                  ("s" (lambda () (interactive) (insert "┼")) "center ┼")
                  ("d" (lambda () (interactive) (insert "┤")) "right ┤")
                  ("z" (lambda () (interactive) (insert "└")) "bottom left └")
                  ("x" (lambda () (interactive) (insert "┴")) "bottom ┴")
                  ("c" (lambda () (interactive) (insert "┘")) "bottom right ┘")
                  ("r" (lambda () (interactive) (insert "─")) "horizontal ─")
                  ("v" (lambda () (interactive) (insert "│")) "vertical │")))

(provide 'ibm-box-drawing-hydra)

;;; ibm-box-drawing-hydra.el ends here
