;;; Compiled snippets and support files for `javascript-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'javascript-mode
                     '(("view" "${1:NAMESPACE}.views.${2:View} = Backbone.View.extend({\n    tagName: $3,\n    className: $4\n    el: \\$(\"$5\"),\n    template: \\$(\"$6\"),\n\n    initialize: function(){\n        _.bindAll(this, \"render\");\n    },\n\n    render: function(){\n        $(this.el).html(this.template.tmpl());\n        return this\n    },\n    $0\n\n});\n" "Backbone View" nil
                        ("backbone")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/view" nil nil)
                       ("v" "var ${1:name} = $0;\n" "var" nil nil nil "/Users/jason/.emacs.d/snippets/javascript-mode/v" nil nil)
                       ("tx" "text( $0 )${1:.}\n" "text" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/tx" nil nil)
                       ("tra" ".effect(\"transfer\", { to: \\$('${1:selector}') }, ${2:500});" "jQuery transfer()" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/tra" nil nil)
                       ("tog" "toggleClass( \"${1:class}\" )$0" "jQuery toggleClass()" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/tog" nil nil)
                       ("tmpl" "<script id=\"${1:tmpl_id}\" type=\"text/x-jquery-tmpl\">\n  $0\n</script>" "tmpl" nil
                        ("templating")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/tmpl" nil nil)
                       ("thi" "$(this).$0" "jQuery this" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/thi" nil nil)
                       ("t" "\\$(this).$0\n" "this" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/t" nil nil)
                       ("sli" "slideUp(${1:'slow'}).$0" "jQuery slideUp()" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/slid" nil nil)
                       ("sli" "slideDown(${1:'slow'}).$0" "jQuery slideDown()" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/sli" nil nil)
                       ("sib" "siblings(${1:\"#selector\"}).$0" "jQuery siblings()" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/sib" nil nil)
                       ("set" "set('${1:property}', ${2:value})\n$0\n" "mvvm - setter" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/set" nil nil)
                       ("rea" "$(document).ready( function(){\n    $0\n});" "jQuery Ready" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/rea" nil nil)
                       ("q" "\\$(\"<${1:tag}></$1>\")${2:.}\n" "jquery selector - new tag object" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/q4" nil nil)
                       ("q" "\\$(\"${1:tag}\")${2:.}\n" "jquery selector - tag" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/q3" nil nil)
                       ("q" "\\$(\".${1:class}\")${2: .}\n" "jquery selector class" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/q2" nil nil)
                       ("q" "\\$(\"#${1:id}\")${2:.}\n" "jquery selector - id" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/q1" nil nil)
                       ("post" "$.post(${1:url},\n     ${2:values},\n     function(data){\n        $0\n     }\n);" "jQuery selector" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/post" nil nil)
                       ("par" "parent(${1:item})$0" "jQuery Parent()" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/par" nil nil)
                       ("ne" "next(${1:'#selector'}).$0" "jQuery next()" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/ne" nil nil)
                       ("meth" "    this.$1 = function( ${2:args} )\n    {\n        $0\n    }" "meth" nil nil nil "/Users/jason/.emacs.d/snippets/javascript-mode/meth" nil nil)
                       ("map" "_.map(${1:iterable}, function(${2:args}){\n    $0\n});" "_.map" nil
                        ("_")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/map" nil nil)
                       ("lob" "console.log(JSON.stringify(${1:data},null,\"\\t\"));\n$0" "log object" nil nil nil "/Users/jason/.emacs.d/snippets/javascript-mode/lob" nil nil)
                       ("ks" "set('${1:property}', ${2:value})\n$0\n" "mvvm - setter" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/ks" nil nil)
                       ("kmo" "\\$('#${1:id}').data('kendoMobileModalView').open();" "open modal" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/kmo" nil nil)
                       ("kmc" "\\$('#${1:id}').data('kendoMobileModalView').close();" "close modal" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/kmc" nil nil)
                       ("kg" "get('${1:property}').$0\n" "mvvm - getter" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/kg" nil nil)
                       ("json" "\\$.getJSON( \"${1:url/to/hit}\", function( data ){\n  $0\n}); " "jQuery getJSON()" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/json" nil nil)
                       ("$" "\\$(${1:\"#selector\"}).$0" "jQuery selector" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/jquery-selector" nil nil)
                       ("it" "it('Should ${1:have a description}', function () {\n    var $0\n});" "Jasmine it" nil
                        ("Jasmine")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/it" nil nil)
                       ("is" "is(\"${1:condition}\")$0" "jQuery Click" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/is" nil nil)
                       ("init" "initialize: function(){\n    _.bindAll(this, \"render\");\n    $0\n}," "Backbone initialize" nil
                        ("backbone")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/init" nil nil)
                       ("if" "if(${1:condition}){\n    $2;\n}else{\n    $0;\n}" "if...else" nil nil nil "/Users/jason/.emacs.d/snippets/javascript-mode/ife" nil nil)
                       ("if" "if(${1:condition}){\n    $0\n}" "if" nil nil nil "/Users/jason/.emacs.d/snippets/javascript-mode/if" nil nil)
                       ("h" "html( $0 )${1:.}\n" "html here" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/ht" nil nil)
                       ("hov" "hover( \n    function(){\n    $0\n    },\n    function(){\n\n    }\n);" "jQuery selector" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/hov" nil nil)
                       ("header" "//\n// `(file-name-nondirectory (buffer-file-name))` v ${1:version}\n//\n// ${2:Short desc}\n//\n// Commentary:\n//\n// $0\n//\n// Additional Reserved Names:\n//\n// ${3:None}\n//\n// Code:" "File header" nil nil nil "/Users/jason/.emacs.d/snippets/javascript-mode/header" nil nil)
                       ("has" "hasClass( \"${1:class}\" )$0" "jQuery hasClass()" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/has" nil nil)
                       ("grease" "// ${1:Script Name}\n// version ${2:0.1}\n// ${3:2009-12-26}\n//\n// Copyright 2009 David Miller\n// @url           http://www.deadpansincerity.com\n//\n// This program is free software: you can redistribute it and/or modify\n//  it under the terms of the GNU General Public License as published by\n//  the Free Software Foundation, either version 3 of the License, or\n//  (at your option) any later version.\n//\n//  This program is distributed in the hope that it will be useful,\n//  but WITHOUT ANY WARRANTY; without even the implied warranty of\n//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n//  GNU General Public License for more details.\n//\n//  You should have received a copy of the GNU General Public License\n//  along with this program.  If not, see <http://www.gnu.org/licenses/>.\n//\n// --------------------------------------------------------------------\n//\n// This Greasemonkey script ${4:Long description}\n//\n// --------------------------------------------------------------------\n//\n// ==UserScript==\n// @name          $1\n// @namespace     http://code.deadpansincerity.com\n// @description   ${5:Short Description}\n// @include       ${6:domain}\n// ==/UserScript==\n$0\n" "grease" nil
                        ("greasemonkey")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/grease" nil nil)
                       ("get" "get('${1:property}').$0\n" "mvvm - getter" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/get2" nil nil)
                       ("get" "$.get(${1:url},\n     ${2:values},\n     function(data){\n        $0\n     }\n);" "jQuery selector" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/get" nil nil)
                       ("fun" "/** $1 */\nfunction ${2:my_function}( ${3:args} ){\n    $0\n}" "fun" nil nil nil "/Users/jason/.emacs.d/snippets/javascript-mode/fun" nil nil)
                       ("forin" "for( ${1:var} in ${2:obj} )\n{\n    $0\n}" "forin" nil nil nil "/Users/jason/.emacs.d/snippets/javascript-mode/forin" nil nil)
                       ("for" "for( var i=0; i < ${1:something}.${2:length}; i++){\n    $0\n}" "for" nil nil nil "/Users/jason/.emacs.d/snippets/javascript-mode/for" nil nil)
                       ("f" "find('${1:something}').$0\n" "find something" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/find" nil nil)
                       ("extru" "expect(${1:true}).toBeTruthy();\n$0" "Jasmine expect Truthy" nil
                        ("Jasmine")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/extru" nil nil)
                       ("ext" "_.extend(${1:Target}, {\n    $0\n});" "_.extend" nil
                        ("_")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/ext" nil nil)
                       ("exeq" "expect(${1:attribute}).toEqual(${2:expected});$0\n" "Jasmine expect Equal" nil
                        ("Jasmine")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/exeq" nil nil)
                       ("exca" "expect(${1:callable}).toHaveBeenCalledWith(${2:expected});$0\n" "Jasmine expect CalledWith" nil
                        ("Jasmine")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/exca" nil nil)
                       ("$.e" "$.each(${1:an-array}, function(i){\n    $0\n});" "jQuery each()" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/eac" nil nil)
                       ("e" "each(function(${1:index},${2:element}){\n    $0\n});" "jQuery .each()" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/e" nil nil)
                       ("dr" "\\$(document).ready( function() {\n    $0\n});\n" "document.ready" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/dr" nil nil)
                       ("doc" "// ${1:Title}\n//\n// $2" "Docblock" nil nil nil "/Users/jason/.emacs.d/snippets/javascript-mode/doc" nil nil)
                       ("desc" "\ndescribe('${1:Spec}', function (){\n    $0\n});" "Jasmine describe" nil
                        ("Jasmine")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/desc" nil nil)
                       ("cs" "css('${1:key}', '${2:value}')${3:.}\n" "css set" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/cssset" nil nil)
                       ("cg" "css('${1:key}')${2:.}\n" "css get" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/cssget" nil nil)
                       ("cg" "css('${1:key}')${2:.}\n" "css get" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/csg" nil nil)
                       ("click" "click( function(){\n    $0\n});" "jQuery Click" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/click" nil nil)
                       ("chi" "children( ${1:item} )$0" "Children" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/chi" nil nil)
                       ("cha" "change(function(){\n  $0\n});" "jQuery change()" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/chang" nil nil)
                       ("cha" "change(function(){\n    $0\n});" "jQuery change()" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/cha" nil nil)
                       ("{" "{\n    $0\n}\n" "add a block" nil nil nil "/Users/jason/.emacs.d/snippets/javascript-mode/b" nil nil)
                       ("fun" "// ${1:comment}\n${2:attribute}: function (${3:args}) {\n    $0\n}" "attr: function" nil nil nil "/Users/jason/.emacs.d/snippets/javascript-mode/attr-fun" nil nil)
                       ("att" "attr(${1:item})$0" "jQuery Attr()" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/att" nil nil)
                       ("at" "appendTo( $0 )\n" "appendto" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/at" nil nil)
                       ("as" "attr('${1:attr}', '${2:value}')${3:.}\n" "attrs - setter" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/as" nil nil)
                       ("ag" "attr('${1:attr}')${2:.}\n" "attr - getter" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/aq" nil nil)
                       ("ap" "append( $0 )\n" "append" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/ap" nil nil)
                       ("ajax" "\\$.ajax( {\n  url: $1,\n  dataType: 'json',\n  data: $2,\n  success: function(data){\n     $0\n  }\n});" "jQuery ajax" nil
                        ("jquery")
                        nil "/Users/jason/.emacs.d/snippets/javascript-mode/ajax" nil nil)))


;;; Do not edit! File generated at Sat Nov  7 12:16:41 2015
