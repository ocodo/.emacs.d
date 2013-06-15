(require 'cl)
(or (require 'xmlgen nil t)
    (require 'xmlgen "xml-gen"))

(defvar smt/user-selected-window nil)
(defvar smt/current-theme nil)

(defmacro smt/deftype (name &rest props)
  (declare (indent 1))
  (let* (( maker-sym
           (intern (concat "smt/make-"
                           (symbol-name name))))
         ( \definer-sym
           (intern (concat "smt/def" (symbol-name name))))
         ( namespace-sym
           (intern (concat "smt/" (symbol-name name) "s")))
         ( predicate-sym
           (intern (concat "smt/" (symbol-name name) "-p")))
         ( getter-prefix
           (concat "smt/"
                   (substring (symbol-name name)
                              0 1)
                   "-"))
         ( get-sym
           (intern (concat getter-prefix
                           "get")))
         ( prototype-getter-sym
           (intern (concat getter-prefix
                           "prototype"))))
    `(progn
       (defvar ,namespace-sym nil)
       (defun ,maker-sym (&rest pairs)
         (unless (memq :prototype pairs)
           (setf (getf pairs :prototype) 'archetype))
         pairs)
       (defmacro ,definer-sym (name &rest pairs)
         (declare (indent 1))
         `(let* (( object (,',maker-sym ,@pairs)))
            (setq ,',namespace-sym (remove-if (lambda (elem) (eq (car elem) ',name))
                                              ,',namespace-sym)
                  ,',namespace-sym (acons ',name object ,',namespace-sym))
            object))
       (put (quote ,definer-sym) 'common-lisp-indent-function
            '(1 &body))
       (,definer-sym archetype
           ,@(append (list :prototype nil :type (list 'quote name))
                     props))
       ,@(progn
          (let (result)
            (dotimes (iter (length props))
              (when (evenp iter)
                (push `(defun ,(intern
                                (concat getter-prefix
                                        (substring
                                         (symbol-name
                                          (nth iter props))
                                         1)))
                           (,name)
                         (smt/maybe-funcall
                          ,(list get-sym name (nth iter props))
                          ,name))
                      result)))
            result))
       (defun ,get-sym (object property)
         (smt/get object property ,namespace-sym))
       (defun ,prototype-getter-sym (object)
         (,get-sym object :prototype))
       (defun ,predicate-sym (object)
         (and (consp object)
              (eq ',name (smt/get object :type ,namespace-sym))))
       )))
(put 'smt/deftype 'common-lisp-indent-function
     '(1 &body))

(defun smt/get (object property &optional namespace resolution-stack)
  (when (symbolp object)
    (setq object (cdr (assoc object namespace))))
  (when (memq object resolution-stack)
    (error "Cyclic inheritance"))
  (cond ( (memq property object)
          (getf object property))
        ( (getf object :prototype)
          (let* (( prototype (getf object :prototype)))
            (smt/get prototype
                     property
                     namespace
                     (cons object resolution-stack))))))

(defun smt/maybe-funcall (thing &rest args)
  (if (or (functionp thing)
          ;; Will create an error should any proprty
          ;; contain a symbol that is not fboundp.
          (and (symbolp thing) (not (booleanp thing))))
      (apply thing args)
      thing))

;;;; Types
;;; Theme

(smt/deftype theme
  :background nil
  :overlay nil
  :defs nil
  :export 'smt/t-export-default
  :style 'smt/style-default
  :local-widgets nil
  :pixel-height (lambda (theme) (frame-char-height))
  :rows nil)


;;; Row

(smt/deftype row
  :align "left"
  :baseline 'smt/text-base-line-default
  :width 'smt/r-width-default
  :margin 0
  :always-visible nil
  :widgets nil
  :style nil
  :export 'smt/r-export-default)

(defun smt/make-row (&rest pairs)
  (unless (memq :prototype pairs)
    (setf (getf pairs :prototype) 'archetype))
  (when (equal (getf pairs :align) "center")
    (setf (getf pairs :align) "left")
    (setf (getf pairs :margin)
          (lambda (row)
            (floor
             (/ (- (smt/window-width)
                   (smt/r-width row))
                2)))))
  pairs)

(defun smt/r-export (row theme)
  (smt/maybe-funcall
   (smt/r-get row :export)
   row theme))

;;; Widget

(smt/deftype widget
  :style 'smt/style-default
  :on-click nil
  :text ""
  :width 'smt/w-width-default
  :export 'smt/w-export-default)

(defun smt/w-export (widget row theme)
  (smt/maybe-funcall
   (smt/w-get widget :export)
   widget row theme))

(defun smt/w-on-click (widget)
  (smt/w-get widget :on-click))

;;; EOF Types
;;;; Functions

(defun smt/window-active-p ()
  "Meant to be used from within mode-line-format."
  (or (eq smt/user-selected-window (selected-window))
      (and (eq (last-nonminibuffer-frame) (selected-frame))
           (not (cadr (window-list))))))

(defun smt/ranges-overlapping-p (r1 r2)
  (cond ( (<= (cdr r1) (car r2))
          nil)
        ( (<= (cdr r2) (car r1))
          nil)
        ( t t)))

(defun smt/r-range (row)
  (let (( left (smt/r-left row)))
    (cons left (+ left (smt/r-width row)))))

(defun smt/t-visible-rows (theme)
  (let* (( rows (mapcar (apply-partially 'smt/t-normalize-row theme)
                        (smt/t-rows theme))))
    (dotimes (iter (length rows))
      (when (nth iter rows)
        (let* (( current-row (nth iter rows))
               ( following-rows (nthcdr (1+ iter) rows))
               ( current-row-range
                 (smt/r-range current-row)))
          (dotimes (iter2 (length following-rows))
            (when (and (nth iter2 following-rows)
                       (not (smt/r-always-visible
                             (nth iter2 following-rows))))
              (let (( following-row-range
                      (smt/r-range (nth iter2 following-rows))))
                (when (or (smt/ranges-overlapping-p
                           current-row-range
                           following-row-range)
                          (minusp (car following-row-range)))
                  (setf (nth iter2 following-rows) nil))))))))
    (remove-if 'null rows)))

(defun smt/t-export-default-xml (theme)
  (let* (( width (smt/window-pixel-width))
         ( height (smt/t-pixel-height theme))
         ( rows (smt/t-visible-rows theme)))
    (xmlgen
     `(svg
       :xmlns "http://www.w3.org/2000/svg"
       :width ,width
       :height ,height
       ,@(smt/t-defs theme)
       ,@(smt/t-background theme)
       ,@(mapcar
          (lambda (row) (smt/r-export row theme))
          rows)
       ,@(smt/t-overlay theme)
       ))))

(defun* smt/define-keys (keymap &rest bindings)
  "Syntax example:
\(smt/define-keys fundamental-mode-map
  (kbd \"h\") 'backward-char
  (kbd \"l\") 'forward-char\)
 Returns the keymap in the end."
  (declare (indent 1))
  (while bindings
    (define-key keymap (pop bindings) (pop bindings)))
  keymap)
(put 'smt/define-keys 'common-lisp-indent-function
     '(4 &body))

(defun* smt/t-export-default (theme)
  ;; (return-from smt/t-export-default)
  (let* ((xml (smt/t-export-default-xml theme))
         (image (create-image xml 'svg t)))
    (propertize
     "."
     'pointer 'vdrag
     'display image
     'keymap (let (( map (make-sparse-keymap)))
               (smt/define-keys map
                 (kbd "<mouse-1>") 'smt/receive-click
                 (kbd "<nil> <header-line> <mouse-1>") 'smt/receive-click
                 (kbd "<nil> <mode-line> <mouse-1>") 'smt/receive-click
                 (kbd "<header-line> <mouse-1>") 'smt/receive-click
                 (kbd "<mode-line> <mouse-1>") 'smt/receive-click)
               map))))

(defun smt/r-width-default (row)
  (let (( widgets (smt/r-widgets row))
        ( total-width 0))
    (dolist (widget widgets)
      (setq widget
            (smt/t-normalize-widget
             (smt/get-current-theme)
             widget))
      (incf total-width (smt/w-width widget)))
    total-width))

(defun smt/t-normalize-widget (theme widget-or-name)
  (if (smt/widget-p widget-or-name)
      widget-or-name
      (or (cdr (assoc widget-or-name (smt/t-local-widgets theme)))
          (cdr (assoc widget-or-name smt/widgets))
          (error "Can't process widget: %s" widget-or-name))))

(defun smt/t-normalize-row (theme row-or-name)
  (if (smt/row-p row-or-name)
      row-or-name
      (or (cdr (assoc row-or-name smt/rows))
          (error "Can't process row: %s" row-or-name))))

(defun smt/r-export-default (row theme)
  `(text
    :text-anchor
    ,(if (equal (smt/r-align row) "left")
         "start"
         "end")
    :x ,(if (equal (smt/r-align row) "left")
            (* (smt/r-margin row)
               (frame-char-width))
            (- (smt/window-pixel-width)
               (* (smt/r-margin row)
                  (frame-char-width))))
    :y ,(smt/r-baseline row)
    ,@(mapcar (lambda (widget-or-name)
                (smt/w-export
                 (smt/t-normalize-widget
                  theme widget-or-name)
                 row theme))
              (smt/r-widgets row))))

(defun smt/w-export-default (widget row theme)
  `(tspan
    ,@(smt/combine-styles
       (smt/t-style theme)
       (smt/r-style row)
       (smt/w-style widget))
    ,(smt/w-text widget)))

(defun smt/w-width-default (widget)
  (length (smt/w-text widget)))

(defun* smt/r-receive-click (row theme event)
  (setq row (smt/t-normalize-row theme row))
  (let* (( click-char-location
           (floor (/ (car (third (second event)))
                     (frame-char-width))))
         ( window-width (smt/window-width))
         ( widgets (smt/r-widgets row))
         ( offset (smt/r-left row))
         current-widget-width)
    (dolist (widget widgets)
      (setq widget (smt/t-normalize-widget theme widget))
      (setq current-widget-width (smt/w-width widget))
      (when (and (<= offset click-char-location)
                 (< click-char-location
                    (+ offset current-widget-width)))
        (when (smt/w-on-click widget)
          (funcall (smt/w-on-click widget) widget event)
          (return-from smt/r-receive-click t))
        (error "Widget has no on-click handler"))
      (setq offset (+ offset current-widget-width)))
    nil))

(defun* smt/t-receive-click (theme event)
  (let (( rows (smt/t-visible-rows theme)))
    (ignore-errors
      (dolist (row rows)
        (setq row (smt/t-normalize-row theme row))
        (when (smt/r-receive-click row theme event)
          (return-from smt/t-receive-click))))
    (message "")))

(defun smt/receive-click (event)
  (interactive "e")
  (mouse-set-point event)
  (smt/t-receive-click
   (smt/get-current-theme)
   event))

(defun smt/r-left (row)
  (let (( margin (smt/r-margin row))
        ( width (smt/r-width row)))
    (if (equal "left" (smt/r-align row))
        margin
        (- (smt/window-width) (+ margin width)))))

;;; Methods EOF

(defun smt/window-pixel-width ()
  (let (( window-edges (window-pixel-edges)))
    (- (nth 2 window-edges) (nth 0 window-edges))))

(defun smt/window-width ()
  (let (( window-edges (window-edges)))
    (- (nth 2 window-edges) (nth 0 window-edges))))

(defun smt/points-to-pixels (points)
  ;; points = pixels * 72 / 96
  (/ (* 96 points) 72))

(defun smt/pixels-to-points (pixels)
  (/ (* pixels 72) 96))

(defun smt/font-pixel-size ()
  (ceiling
   (smt/points-to-pixels
    (/ (face-attribute 'default :height) 10))))

(defun smt/text-base-line-default (widget)
  ;; Should be this one, but empirically it doesn't work as well
  ;; (smt/font-pixel-size)
  (let ((font-size (* 0.7 (smt/font-pixel-size))))
    (floor
     (+ font-size
        (/ (- (frame-char-height)
              font-size)
           2)))))

(defun smt/style-default (theme)
  `(:font-family
    ,(face-attribute 'default :family)
    :font-size
    ;; ,(- (frame-char-height) 4)
    ,(concat (int-to-string
              (ceiling
               (/ (face-attribute 'default :height)
                  10.0)))
             "pt")
    ))

(defun* smt/filter-inset (&optional (dark-opacity 0.5) (light-opacity 0.5))
  `((filter
     :id "inset"
     (feOffset :in "sourceGraphic" :dx -1 :dy -1 :result "o_dark")
     (feOffset :in "sourceGraphic" :dx 2 :dy 2 :result "o_light")
     ;; http://www.w3.org/TR/SVG/filters.html#feColorMatrixElement
     ;; http://en.wikipedia.org/wiki/Matrix_multiplication#Illustration
     (feColorMatrix
      :type "matrix"
      :in "o_light" :result "o_light"
      :values ,(concat
                "0  0  0  0  1 "
                "0  0  0  0  1 "
                "0  0  0  0  1 "
                (format
                 "0  0  0  %s  0 "
                 light-opacity)
                ))
     (feColorMatrix
      :type "matrix"
      :in "o_dark" :result "o_dark"
      :values ,(concat
                "0  0  0  0  -1 "
                "0  0  0  0  -1 "
                "0  0  0  0  -1 "
                (format
                 "0  0  0  %s  0 "
                 dark-opacity)
                ))
     (feMerge
      (feMergeNode :in "o_dark")
      (feMergeNode :in "o_light")
      (feMergeNode :in "SourceGraphic")
      ))))

(defun smt/combine-styles (&rest plists)
  (cond
    ( (= 1 (length plists))
      (car plists))
    ( (null plists)
      nil)
    ( t (let (( plistC (copy-list (car plists)))
              ( plistB (cadr plists))
              key val)
          (dotimes (iter (/ (length plistB) 2))
            (setq key (nth (* 2 iter) plistB)
                  val (nth (1+ (* 2 iter)) plistB))
            (if (null val)
                (remf plistC key)
                (setf (getf plistC key) val)))
          (apply 'smt/combine-styles plistC (cddr plists))
          ))))

(defun smt/modeline-format ()
  (let ((theme (smt/get-current-theme)))
    (cond ( (smt/theme-p theme)
            (smt/t-export theme))
          ( (or (functionp theme)
                (symbolp theme))
            (funcall theme))
          ( t theme))))

(defun smt/get-current-theme ()
  (cdr (assoc smt/current-theme smt/themes)))

(defun smt/get-widget-by-name (name)
  (cdr (assoc name smt/widgets)))

(defun smt/reset ()
  (interactive)
  (let (( tests-where-loaded
          (featurep 'svg-mode-line-themes-tests)))
    (ignore-errors
      (unload-feature 'svg-mode-line-themes t))
    (ignore-errors
      (unload-feature 'svg-mode-line-themes-widgets t))
    (ignore-errors
      (unload-feature 'svg-mode-line-themes-core t))
    (ignore-errors
      (unload-feature 'svg-mode-line-themes-nasa t))
    (ignore-errors
      (unload-feature 'svg-mode-line-themes-black-crystal t))
    (ignore-errors
      (unload-feature 'svg-mode-line-themes-diesel t))
    (require (quote svg-mode-line-themes))
    (when tests-where-loaded
      (ignore-errors
        (unload-feature 'svg-mode-line-themes-tests t))
      (require (quote svg-mode-line-themes-tests)))))

(defun smt/register-user-location ()
  (setq smt/user-selected-window (selected-window)))

(provide 'svg-mode-line-themes-core)
;;; svg-mode-line-themes-core.el ends here
