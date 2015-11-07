;;; Compiled snippets and support files for `haml-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'haml-mode
                     '(("wsd:" "wsDate: $1" "wsDate:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/wsd:" nil nil)
                       ("vi:" "visible: $1" "visible:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/vi:" nil nil)
                       ("v:" "value: ${1:bindable}$0" "value:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/v:" nil nil)
                       ("tog:" "toggle: $1" "toggle:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/tog:" nil nil)
                       ("ti:" "textInput: ${1:bindable}$0" "textInput:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/ti:" nil nil)
                       ("t:" "text: ${1:bindable}$0" "text:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/t:" nil nil)
                       ("sub:" "submit: $1" "submit:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/sub:" nil nil)
                       ("ss:" "spinnerSelector: $1" "spinnerSelector:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/ss:" nil nil)
                       ("pls:" "promiseLoadingScreen: $1" "promiseLoadingScreen:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/pls:" nil nil)
                       ("pag:" "paginator: $1" "paginator:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/pag:" nil nil)
                       ("o:" "options: ${1:collection}, value: ${2:selectedItem}, optionsText: '${3:textProperty}', optionsCaption: '${4:caption}'$0" "option:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/o:" nil nil)
                       ("kwi:" "/ ko with: $1\n$0\n/ /ko" "/ ko with:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/kwi:" nil nil)
                       ("kin:" "/ ko ifno: $1\n$0\n/ /ko" "/ ko ifnot: " nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/kin:" nil nil)
                       ("kif:" "/ ko if: $1\n$0\n/ /ko" "/ ko if:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/kif:" nil nil)
                       ("kfo:" "/ ko foreach: $1\n$0\n/ /ko" "/ ko foreach:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/kfo:" nil nil)
                       ("ht:" "html: $1" "html:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/ht:" nil nil)
                       ("hi:" "hidden: $1" "hidden:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/hi:" nil nil)
                       ("itab" "/ Bootstrap Table\n%table.table\n  %tr\n    %th ${1:First Column}\n    %th view\n    %th edit\n    %th delete\n  - @${2:Model}s.each do |$2|\n    %tr\n      %td= link_to $2.$1, $2\n      %td.span1\n        = link_to $2 do\n          .icon-info-sign\n      %td.span1\n        = link_to edit_$2_path($2) do\n          .icon-pencil\n      %td.span1\n        = link_to $2, {:confirm => 'Are you sure?', :method => :delete } do\n          .icon-trash\n$0" "haml-bootstrap-rails-list-view" nil nil
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/haml-mode/haml-bootstrap-rails-list-view" nil nil)
                       ("en:" "enable: $1" "enable:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/en:" nil nil)
                       ("dis:" "disable: $1" "disable:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/dis:" nil nil)
                       ("ddt:" "displayDateTime: $1" "displayDateTime:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/ddt:" nil nil)
                       ("dd:" "displayDate: $1" "displayDate:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/dd:" nil nil)
                       ("d=" "data-bind=\"$0\"" "data-bind" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/d=" nil nil)
                       ("cv:" "checkedValue: $1" "checkedValue:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/cv:" nil nil)
                       ("cs:" "css: ${1:$$(yas/choose-value '( \"{}\" \"\" ))}" "css:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/cs:" nil nil)
                       ("cls:" "clickSelect: $1" "clickSelect:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/cls:" nil nil)
                       ("cl:" "click: ${1:handler}$0" "click:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/cl:" nil nil)
                       ("ce:" "checked: $1" "checked:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/ce:" nil nil)
                       ("at:" "attr: $1" "attr:" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/at:" nil nil)
                       ("%ab" "%action-button(params=\"css: ${1:css}, confirmMessage: '${2:confirm ?}', click: ${3:click}, actionLabel: '${4:label}', workingLabel: '${5:working}', completedLabel: '${6:completed}', completed: ${7:completedObservable}, visible: ${8:visibleObservable}, disabled: ${9:disabledObservable}\")" "action-button with confirm" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/%ab-cfm" nil nil)
                       ("%ab" "%action-button(params=\"css: ${1:css}, click: ${2:click}, actionLabel: '${3:label}', workingLabel: '${4:working}', completedLabel: '${5:completed}', completed: ${6:completedObservable}, visible: ${7:visibleObservable}, disabled: ${8:disabledObservable}\")" "action-button with completed" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/%a-c" nil nil)
                       ("%ab" "%action-button(params=\"css: '${1:css}', actionLabel: '${2:label}', workingLabel: '${3:working}', click: ${4:click}\")" "action-button minimal" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/%a-bm" nil nil)
                       ("%ab" "%action-button(params=\"css: ${1:css}, actionLabel: '${2:label}', workingLabel: '${3:working}', click: ${4:click}, completedLabel: '${5:completed}', completed: ${6:completedObservable}, visible: ${7:visibleObservable}, disabled: ${8:disabledObservable}\")" "action-button" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/haml-mode/%a-" nil nil)))


;;; Do not edit! File generated at Sat Nov  7 12:16:41 2015
