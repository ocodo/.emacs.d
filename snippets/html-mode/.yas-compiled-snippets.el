;;; Compiled snippets and support files for `html-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'html-mode
                     '(("kvi" "<div data-role=\"view\" data-layout=\"${1:layout}-layout\" id=\"${2:name}\">\n$0\n</div>\n" "kendo mobile view" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/html-mode/kvi" nil nil)
                       ("ktmp" "<!-- Template : ${1:Name} -->\n<script type=\"text/x-kendo-template\" id=\"${2:template-id}\">\n  <div>\n    $0\n  </div>\n</script>\n" "kendo template" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/html-mode/ktmp" nil nil)
                       ("kmodal" "<!-- Modal: ${1:name} -->\n<div id=\"$1\"\n     data-role=\"modalview\"  \n     data-model=\"${2:model}\"\n     class=\"modal-view\">    \n  <div data-role=\"header\" \n       class=\"modal-header\">\n    <div data-role=\"navbar\">\n      <span>${3:Heading}</span>\n      <a data-click=\"closeModalView\"\n         data-role=\"button\" \n         data-align=\"right\"><img src=\"img/close-modal-button.svg\"/></a>\n    </div>\n  </div>\n  <div class=\"modal-view-inset\">\n    $0\n  </div>\n  <div data-role=\"footer\"  \n       class=\"modal-footer\">\n    <a data-bind=\"click: ${5:okMethod}\"\n       data-role=\"button\">${6:Ok}</a>\n    <a data-click=\"closeModalView\"\n       data-role=\"button\" \n       data-align=\"right\">Cancel</a>\n  </div>\n</div>\n" "kendo modal" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/html-mode/kmodal" nil nil)
                       ("klay" "<div data-role=\"layout\" data-id=\"${1:name}-layout\">\n  <div data-role=\"header\">\n    ${2:header}\n  </div>\n  <div data-role=\"footer\">\n    ${3:footer}\n  </div>\n</div>\n$0" "kendo mobile layout" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/html-mode/klay" nil nil)
                       ("kdr" "data-role=\"${1:$$(yas/choose-value '(\"actionsheet\" \"button\" \"buttongroup\" \"footer\" \"header\" \"layout\" \"listview\" \"modalview\" \"navbar\" \"switch\" \"tabstrip\" \"touch\" \"view\"))}\"$0" "data-role " nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/html-mode/kdr" nil nil)
                       ("kbt" "data-bind=\"text: $0\"" "data-bind text" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/html-mode/kbt" nil nil)
                       ("kdi" "<img data-bind=\"attr: {src:'$1', alt:'$2' } \"/>$0\n" "img with bound attributes" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/html-mode/kbi" nil nil)
                       ("kbcl" "data-bind=\"click: $0\"" "data-bind click" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/html-mode/kbcl" nil nil)
                       ("kbc" "data-bind=\"currency: $0\"" "data-bind currency" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/html-mode/kbc" nil nil)
                       ("kash" "<ul data-role=\"actionsheet\" data-id=\"${1:name}-actionsheet\">\n  <li><a data-action=\"${2:handler}\">${3:label}</a></li>\n</ul>\n$0" "kendo mobile action sheet" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/html-mode/kash" nil nil)
                       ("kal" "<a data-role=\"button\" data-icon=\"${1:action}\" data-rel=\"actionsheet\" href=\"\\#${2:name}-actionsheet\"></a>\n$0" "kendo mobile link to actionsheet" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/html-mode/kal" nil nil)
                       ("kab" "<li><a data-action=\"${1:handler}\">${2:label}</a></li>\n$0" "kendo mobile actionsheet button" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/html-mode/kab" nil nil)
                       ("jqui" "<script src=\"http://ajax.googleapis.com/ajax/libs/jqueryui/1.9.2/jquery-ui.min.js\"></script>\n$0" "JQuery UI google hosted (1.9.2min) " nil nil nil "/Users/jason/.emacs.d/snippets/html-mode/jqueryui" nil nil)
                       ("jq" "<script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.8.3/jquery.min.js\"></script>\n$0" "JQuery google hosted (1.8.3min) " nil nil nil "/Users/jason/.emacs.d/snippets/html-mode/jquery" nil nil)
                       ("imgf" "<img src=\"${1:$$(yas/choose-value '((process-lines \"find_images\")))}\" alt=\"$2\"/>$0\n" "img with helper" nil nil nil "/Users/jason/.emacs.d/snippets/html-mode/imgf" nil nil)
                       ("handlebar" "<script id=\"$1\" type=\"text/x-handlebars-template\">\n$0\n</script>\n" "handlebars Template snippet" nil nil nil "/Users/jason/.emacs.d/snippets/html-mode/handlebar" nil nil)))


;;; Do not edit! File generated at Sat Nov  7 12:16:41 2015
