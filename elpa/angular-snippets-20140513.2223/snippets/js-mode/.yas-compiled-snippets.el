;;; Compiled snippets and support files for `js-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
                     '(("ngw" "when(\"/$1\", {\n  templateUrl: \"$2\",\n  controller: \"$0\"\n})" "ngw" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/ngw.yasnippet" nil nil)
                       ("ngs" "service(\"$1\", function () {\n  $3\n});\n" "ngs" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/ngs.yasnippet" nil nil)
                       ("ngrwr" "$routeProvider.when(\"$1\", {\n  templateUrl: \"$2\",\n  controller: \"$3\",\n  resolve: {$0\n  }\n});\n" "ngrwr" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/ngrwr.yasnippet" nil nil)
                       ("ngrw" "$routeProvider.when(\"$1\", {\n  templateUrl: \"$2\",\n  controller: \"$3\"\n});\n$0" "ngrw" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/ngrw.yasnippet" nil nil)
                       ("ngro" "$routeProvider.otherwise({redirectTo: \"$1\"});\n$0" "ngro" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/ngro.yasnippet" nil nil)
                       ("ngm" "angular.module(\"$1\", [$2]);\n" "ngm" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/ngm.yasnippet" nil nil)
                       ("ngfi" "filter(\"$1\", function ($2) {\n  return function (input, $3) {\n    $3\n  };\n});\n" "ngfi" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/ngfi.yasnippet" nil nil)
                       ("ngfa" "factory(\"$1\", function ($2) {\n  $3\n})\n" "ngfa" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/ngfa.yasnippet" nil nil)
                       ("ngd" "directive('$1', function ($2) {\n  return function (scope, element, attrs$4) {\n    $3\n  };\n});\n" "ngd" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/ngd.yasnippet" nil nil)
                       ("ngc" "controller('$1', function ($scope, $2) {\n  $0\n});" "ngc" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/ngc.yasnippet" nil nil)
                       ("$w" "$scope.$watch(\"$1\", function (newValue, oldValue) {\n  $0\n});" "$w" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/$w.yasnippet" nil nil)
                       ("$va" "$scope.$1 = $1;\n$0" "$va" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/$va.yasnippet" nil nil)
                       ("$v" "$scope.$1 = $2;\n$0" "$v" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/$v.yasnippet" nil nil)
                       ("$on" "$scope.$on(\"$1\", function (event, $2) {\n  $0\n});" "$on" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/$on.yasnippet" nil nil)
                       ("$f" "$scope.$1 = function ($2) {\n  $0\n};\n" "$f" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/$f.yasnippet" nil nil)
                       ("$e" "$scope.$emit(\"$1\", $2);\n$0" "$e" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/$e.yasnippet" nil nil)
                       ("$b" "$scope.$broadcast(\"$1\", $2);\n$0" "$b" nil nil nil "/Users/jason/.emacs.d/elpa/angular-snippets-20140513.2223/snippets/js-mode/$b.yasnippet" nil nil)))


;;; Do not edit! File generated at Sat Nov  7 12:16:42 2015
