;;; ibm-box-drawing-hydra --- draw DOS style boxes with a hydra

;;; Commentary:

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

;;;###autoload
(global-set-key (kbd "C-x d")
                (defhydra hydra-draw-box (:color pink)
                  "Draw box with IBM single line box characters."
                  ("ESC" nil :color blue)
                  ("q" ibm-box-top-left  "top left ┌")
                  ("w" ibm-box-top       "top ┬")
                  ("e" ibm-box-top-right "top right ┐")
                  ("a" ibm-box-left "left ├")
                  ("s" ibm-box-center "center ┼")
                  ("d" ibm-box-right "right ┤")
                  ("z" ibm-box-bottom-left "bottom left └")
                  ("x" ibm-box-bottom "bottom ┴")
                  ("c" ibm-box-bottom-right "bottom right ┘")
                  ("r" ibm-box-horz "horizontal ─")
                  ("v" ibm-box-vert "vertical │")))

(provide 'ibm-box-drawing-hydra)

;;; ibm-box-drawing-hydra.el ends here
