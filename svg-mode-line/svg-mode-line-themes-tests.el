(require 'svg-mode-line-themes)
(require 'ert)

(smt/defwidget test-center-label
  :text "center")

(smt/defrow center-test
  :align "center"
  :widgets '(test-center-label))

(smt/deftheme test
  :prototype 'diesel
  :rows '(default-left
          center-test
          default-right
          ))

(ert-deftest smt/combine-styles ()
  (should (null (smt/combine-styles)))
  (should (null (smt/combine-styles '(:a 45) '(:a nil))))
  (should (equal (smt/combine-styles '(:a 45) nil)
                 '(:a 45)))
  (should (equal '(:a 45)
                 (smt/combine-styles '(:a 45))))
  (let (( result (smt/combine-styles '(:a 1) '(:b 2))))
    (should (= 1 (getf result :a)))
    (should (= 2 (getf result :b)))
    (should (= 4 (length result))))
  (let (( result (smt/combine-styles '(:a 1) '(:a 2))))
    (should (= 2 (getf result :a)))
    (should (= 2 (length result))))
  (let (( result (smt/combine-styles '(:a 1) '(:b 2) '(:c 3))))
    (should (= 1 (getf result :a)))
    (should (= 2 (getf result :b)))
    (should (= 3 (getf result :c)))
    (should (= 6 (length result)))))

(ert-deftest smt/ranges-overlapping-p ()
  (should (smt/ranges-overlapping-p '(0 . 10) '(5 . 6)))
  (should (smt/ranges-overlapping-p '(0 . 10) '(9 . 20)))
  (should (smt/ranges-overlapping-p '(3 . 10) '(3 . 5)))
  (should (smt/ranges-overlapping-p '(3 . 5) '(3 . 10)))
  (should-not (smt/ranges-overlapping-p '(0 . 0) '(0 . 0)))
  (should-not (smt/ranges-overlapping-p '(0 . 10) '(10 . 10)))
  (should-not (smt/ranges-overlapping-p '(0 . 0) '(0 . 10)))
  (should-not (smt/ranges-overlapping-p '(0 . 10) '(0 . 0)))
  )

(ert-deftest smt/row ()
  (let (( row (smt/make-row
               :margin 2
               :widgets (list
                         (smt/make-widget
                          :text "123")
                         (smt/make-widget
                          :text "456")))))
    (should (= (smt/r-width row) 6))
    (should (equal (smt/r-align row) "left"))
    (should (= (smt/maybe-funcall (smt/r-margin row)) 2))
    (should (= (smt/r-left row) 2))
    (should (equal (smt/r-range row) '(2 . 8)))))

(ert-deftest smt/theme-overlapping ()
  (let (( theme
          (smt/make-theme
           :rows
           (list (smt/make-row
                  :margin 2
                  :widgets
                  (list
                   (smt/make-widget
                    :text "123346")))
                 (smt/make-row
                  :margin 8
                  :widgets
                  (list
                   (smt/make-widget
                    :text "123346")))))))
    (should (= 2 (length (smt/t-visible-rows theme)))))
  (let (( theme
          (smt/make-theme
           :rows
           (list (smt/make-row
                  :margin 2
                  :widgets
                  (list
                   (smt/make-widget
                    :text "123346")))
                 (smt/make-row
                  :margin 7
                  :widgets
                  (list
                   (smt/make-widget
                    :text "123346")))))))
    (should (= 1 (length (smt/t-visible-rows theme)))))
  (let (( theme
          (smt/make-theme
           :rows
           (list (smt/make-row
                  :widgets
                  (list
                   (smt/make-widget
                    :text "first")))
                 (smt/make-row
                  :align "right"
                  :widgets
                  (list
                   (smt/make-widget
                    :text "123456")))
                 (smt/make-row
                  :align "right"
                  :widgets
                  (list
                   (smt/make-widget
                    :text "1234567")))))))
    (flet (( smt/window-width () 10))
      (should (= 4 (smt/r-left (second (smt/t-rows theme)))))
      (should (= 1 (length (smt/t-visible-rows theme))))
      (should (equal (smt/w-text
                      (car (smt/r-widgets
                            (car (smt/t-visible-rows theme)))))
                     "first"))
      ))
  (let (( theme
          (smt/make-theme
           :rows
           (list (smt/make-row
                  :widgets
                  (list
                   (smt/make-widget
                    :text "12345")))
                 (smt/make-row
                  :align "right"
                  :widgets
                  (list
                   (smt/make-widget
                    :text "12345")))))))
    (flet (( smt/window-width () 10))
      (should (= 2 (length (smt/t-visible-rows theme)))))))

(ert-deftest smt/objects ()
  (let (( namespace
          (list (cons 'archetype
                      (list :type 'type1
                            :shadow1 'shadow
                            :shadow2 'light))))
        ( obj1
          (list :prototype 'archetype
                :prop1 1
                :prop2 2
                :prop3 3
                :shadow1 nil
                :shadow2 'shadow)))
    (should (eq 'type1 (smt/get obj1 :type namespace)))
    (should (null (smt/get obj1 :shadow1 namespace)))
    (should (eq 'shadow (smt/get obj1 :shadow2 namespace)))
    ))

(smt/deftype atest
  :some-prop 34)

(smt/defatest self-reference
  :prototype 'self-reference)

(smt/defatest mutual-reference1
  :prototype 'mutual-reference2)

(smt/defatest mutual-reference2
  :prototype 'mutual-reference1)

(ert-deftest smt/cyclical-references ()
  (let (( anonymous (smt/make-atest)))
    (setf (getf anonymous :prototype) anonymous)
    (should-error (smt/a-some-prop anonymous))
    (should-error (smt/a-some-prop 'mutual-reference1))
    (should-error (smt/a-some-prop 'self-reference))))

(provide 'svg-mode-line-themes-tests)
;; svg-mode-line-themes-tests.el ends here
