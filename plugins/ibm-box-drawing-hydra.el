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

(defun ibm-box-top-left ()
  "Insert box char top-left."
  (interactive)
  (insert "┌"))

(defun ibm-box-top ()
  "Insert box char top."
  (interactive)
  (insert "┬"))

(defun ibm-box-top-right ()
  "Insert box char top-right."
  (interactive)
  (insert "┐"))

(defun ibm-box-left ()
  "Insert box char left."
  (interactive)
  (insert "├"))

(defun ibm-box-center ()
  "Insert box char center."
  (interactive)
  (insert "┼"))

(defun ibm-box-right ()
  "Insert box char right."
  (interactive)
  (insert "┤"))

(defun ibm-box-bottom-left ()
  "Insert box char bottom-left."
  (interactive)
  (insert "└"))

(defun ibm-box-bottom ()
  "Insert box char bottom."
  (interactive)
  (insert "┴"))

(defun ibm-box-bottom-right ()
  "Insert box char bottom-right."
  (interactive)
  (insert "┘"))

(defun ibm-box-vert ()
  "Insert box char vertical."
  (interactive)
  (insert "│"))

(defun ibm-box-horz ()
  "Insert box char horizontal."
  (interactive)
  (insert "─"))

(global-set-key (kbd "C-x d")
                (defhydra hydra-draw-box (:color pink)
                  "Draw box with IBM single line box characters."
                  ("ESC" nil :color blue) ;; Esc to exit.
                  ("q" ibm-box-top-left      "top left ┌")
                  ("w" ibm-box-top           "top ┬")
                  ("e" ibm-box-top-right     "top right ┐")
                  ("a" ibm-box-left          "left ├")
                  ("s" ibm-box-center        "center ┼")
                  ("d" ibm-box-right         "right ┤")
                  ("z" ibm-box-bottom-left   "bottom left └")
                  ("x" ibm-box-bottom        "bottom ┴")
                  ("c" ibm-box-bottom-right  "bottom right ┘")
                  ("r" ibm-box-horz          "horizontal ─")
                  ("v" ibm-box-vert          "vertical │")))

(provide 'ibm-box-drawing-hydra)

;;; ibm-box-drawing-hydra.el ends here
