;;; Compiled snippets and support files for `nxml-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'nxml-mode
                     '(("title" "<title>$1</title>" "<title>...</title>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/title" nil nil)
                       ("textarea" "<textarea name=\"$1\" id=\"$2\" rows=\"$3\" cols=\"$4\" tabindex=\"$5\"></textarea>" "<textarea ...></textarea>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/textarea" nil nil)
                       ("text" "<text x=\"${1:0}\" y=\"${2:0}\" fill=\"black\"></text>" "text" nil
                        ("svg")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/text" nil nil)
                       ("svg" "<?xml version=\"1.0\" standalone=\"no\"?>\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n<svg width=\"${1:15cm}\" height=\"${2:15cm}\" version=\"1.1\"\n     xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\n     \"http://www.w3.org/1999/xlink\">\n     $0\n</svg>" "svg stub" nil
                        ("svg")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/svg" nil nil)
                       ("style" "<style type=\"text/css\" media=\"${1:screen}\">\n  $0\n</style>" "<style type=\"text/css\" media=\"...\">...</style>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/style" nil nil)
                       ("span" "<span id=\"$1\">$2</span>" "<span id=\"...\">...</span>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/span.id" nil nil)
                       ("span" "<span class=\"$1\">$2</span>" "<span class=\"...\">...</span>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/span.class" nil nil)
                       ("span" "<span>$1</span>" "<span>...</span>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/span" nil nil)
                       ("script" "<script type=\"text/javascript\" src=\"$1\"></script>" "<script type=\"text/javascript\" src=\"...\"></script> " nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/script.javascript-src" nil nil)
                       ("script" "<script type=\"text/javascript\">\n  $0\n</script>" "<script type=\"text/javascript\">...</script> " nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/script.javascript" nil nil)
                       ("rect" "<rect x=\"${1:1}\" y=\"${2:1}\" width=\"${3:1198}\" height=\"${4:398}\"\n        fill=\"${5:none}\" stroke=\"${6:blue}\" stroke-width=\"${7:2}\"/>" "rect" nil
                        ("svg")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/rect" nil nil)
                       ("quote" "<blockquote>\n  $1\n</blockquote>" "<blockquote>...</blockquote>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/quote" nil nil)
                       ("q" "<blockquote>\n$0\n</blockquote>" "<blockquote>...</blockquote>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/q.yasnippet" nil nil)
                       ("pre" "<pre>\n  $0\n</pre>" "<pre>...</pre>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/pre" nil nil)
                       ("p" "<p>$1</p>" "<p>...</p>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/p" nil nil)
                       ("ngconfirm" "<button class=\"btn ${1:btn-primary} btn-huge btn-block ng-confirm\"\n        title=\"${2:Confirm}\"\n\n        action-text=\"${3:Are you sure?}\"\n        action-button-text=\"${4:Ok}\"\n        action-button-classes=\"${5:btn-huge btn-block}\"\n        action-button-main-class=\"${6:btn-success}\"\n        action-function=\"$7\"\n\n        cancel-button-main-class=\"${8:btn-danger}\"\n        cancel-button-text=\"${9:Cancel}\"\n        cancel-button-classes=\"${10:btn-huge btn-block}\"\n        cancel-function=\"${11:closeModal()}\" >\n        ${12:Button}\n</button>" "ng-confirm button and modal" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/ngconfirm" nil nil)
                       ("mailto" "<a href=\"mailto:$1@$2\">$0</a>" "<a href=\"mailto:...@...\">...</a>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/mailto" nil nil)
                       ("link" "<!--[if IE]>\n<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />\n<![endif]-->" "<!--[if IE]><link stylesheet=\"...\" /><![endif]-->" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/link.stylesheet-ie" nil nil)
                       ("link" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link stylesheet=\"...\" />" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/link.stylesheet" nil nil)
                       ("line" "<line x1=\"${1:0}\" y1=\"${2:0}\" x2=\"${3:0}\" y2=\"${4:10}\" stroke=\"${6:black}\" stroke-width=\"${7:1}\"/>" "line" nil
                        ("svg")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/line" nil nil)
                       ("kvi" "<div data-role=\"view\" data-layout=\"${1:layout}-layout\" id=\"${2:name}\">\n$0\n</div>\n" "kendo mobile view" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/kvi" nil nil)
                       ("ktmp" "<!-- Template : ${1:Name} -->\n<script type=\"text/x-kendo-template\" id=\"${2:template-id}\">\n  <div>\n    $0\n  </div>\n</script>\n" "kendo template" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/ktmp" nil nil)
                       ("kmodal" "<!-- Modal: ${1:name} -->\n<div id=\"$1\"\n     data-role=\"modalview\"  \n     data-model=\"${2:model}\"\n     class=\"modal-view\">    \n  <div data-role=\"header\" \n       class=\"modal-header\">\n    <div data-role=\"navbar\">\n      <span>${3:Heading}</span>\n      <a data-click=\"closeModalView\"\n         data-role=\"button\" \n         data-align=\"right\"><img src=\"img/close-modal-button.svg\"/></a>\n    </div>\n  </div>\n  <div class=\"modal-view-inset\">\n    $0\n  </div>\n  <div data-role=\"footer\"  \n       class=\"modal-footer\">\n    <a data-bind=\"click: ${5:okMethod}\"\n       data-role=\"button\">${6:Ok}</a>\n    <a data-click=\"closeModalView\"\n       data-role=\"button\" \n       data-align=\"right\">Cancel</a>\n  </div>\n</div>\n" "kendo modal" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/kmodal" nil nil)
                       ("klay" "<div data-role=\"layout\" data-id=\"${1:name}-layout\">\n  <div data-role=\"header\">\n    ${2:header}\n  </div>\n  <div data-role=\"footer\">\n    ${3:footer}\n  </div>\n</div>\n$0" "kendo mobile layout" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/klay" nil nil)
                       ("kdr" "data-role=\"${1:$$(yas/choose-value '(\"actionsheet\" \"button\" \"buttongroup\" \"footer\" \"header\" \"layout\" \"listview\" \"modalview\" \"navbar\" \"switch\" \"tabstrip\" \"touch\" \"view\"))}\"$0" "data-role " nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/kdr" nil nil)
                       ("kbt" "data-bind=\"text: $0\"" "data-bind text" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/kbt" nil nil)
                       ("kdi" "<img data-bind=\"attr: {src:'$1', alt:'$2' } \"/>$0\n" "img with bound attributes" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/kbi" nil nil)
                       ("kbcl" "data-bind=\"click: $0\"" "data-bind click" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/kbcl" nil nil)
                       ("kbc" "data-bind=\"currency: $0\"" "data-bind currency" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/kbc" nil nil)
                       ("kash" "<ul data-role=\"actionsheet\" data-id=\"${1:name}-actionsheet\">\n  <li><a data-action=\"${2:handler}\">${3:label}</a></li>\n</ul>\n$0" "kendo mobile action sheet" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/kash" nil nil)
                       ("kal" "<a data-role=\"button\" data-icon=\"${1:action}\" data-rel=\"actionsheet\" href=\"\\#${2:name}-actionsheet\"></a>\n$0" "kendo mobile link to actionsheet" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/kal" nil nil)
                       ("kab" "<li><a data-action=\"${1:handler}\">${2:label}</a></li>\n$0" "kendo mobile actionsheet button" nil
                        ("kendo")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/kab" nil nil)
                       ("jqui" "<script src=\"http://ajax.googleapis.com/ajax/libs/jqueryui/1.9.2/jquery-ui.min.js\"></script>\n$0" "JQuery UI google hosted (1.9.2min) " nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/jqueryui" nil nil)
                       ("jq" "<script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.8.3/jquery.min.js\"></script>\n$0" "JQuery google hosted (1.8.3min) " nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/jquery" nil nil)
                       ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/input" nil nil)
                       ("filetype" "<dict>\n	<key>CFBundleTypeExtensions</key>\n	<array>\n		<string>$1</string>\n	</array>\n	<key>CFBundleTypeIconFile</key>\n	<string>${2:-icon.icns}</string>\n	<key>CFBundleTypeName</key>\n	<string>${3:Doc type}</string>\n	<key>CFBundleTypeRole</key>\n	<string>Editor</string>\n</dict>\n" "Filetype segment of Info.plist" nil
                        ("plist")
                        nil "/Users/jason/.emacs.d/snippets/nxml-mode/info-plist-filetype" nil nil)
                       ("imgf" "<img src=\"${1:$$(yas/choose-value '((process-lines \"find_images\")))}\" alt=\"$2\"/>$0\n" "img with helper" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/imgf" nil nil)
                       ("img" "<img src=\"$1\" class=\"$2\" alt=\"$3\" />" "<img src=\"...\" class=\"...\" alt=\"...\" />" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/img" nil nil)
                       ("i" "<i>$0</i>" "<i>...</i>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/i.yasnippet" nil nil)
                       ("html" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">\n  $0\n</html>" "<html xmlns=\"...\">...</html>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/html.xmlns" nil nil)
                       ("html" "<html>\n  $0\n</html>" "<html>...</html>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/html" nil nil)
                       ("href" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/href" nil nil)
                       ("hr" "<hr />" "<hr />" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/hr" nil nil)
                       ("head" "<head>\n  $0\n</head>" "<head>...</head>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/head" nil nil)
                       ("handlebar" "<script id=\"$1\" type=\"text/x-handlebars-template\">\n$0\n</script>\n" "handlebars Template snippet" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/handlebar" nil nil)
                       ("form" "<form method=\"$1\" id=\"$2\" action=\"$3\">\n  $0\n</form>" "<form method=\"...\" id=\"...\" action=\"...\"></form>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/form" nil nil)
                       ("dov" "a mirror up here $3\n\n\n<dov ${1:id=\"${2:some_id and here comes another nested field: ${3:nested_shit}}\"}>\n    $0\n</dov>\n<dov $1>\n    actually some other shit and $3\n</dov>" "<dov...>...</dov>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/dov" nil nil)
                       ("div" "<div id=\"$1\" class=\"$2\">\n  $0\n</div>" "<div id=\"...\" class=\"...\">...</div>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/div.id-class" nil nil)
                       ("div" "<div id=\"$1\">\n  $0\n</div>" "<div id=\"...\">...</div>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/div.id" nil nil)
                       ("div" "<div class=\"$1\">\n  $0\n</div>" "<div class=\"...\">...</div>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/div.class" nil nil)
                       ("div" "<div${1: id=\"${2:some_id}\"}${3: class=\"${4:some_class}\"}>$0</div> " "<div...>...</div>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/div" nil nil)
                       ("code" "<code class=\"$1\">\n  $0\n</code>" "<code class=\"...\">...</code>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/code.class" nil nil)
                       ("code" "<code>\n  $0\n</code>" "<code>...</code>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/code" nil nil)
                       ("br" "<br />" "<br />" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/br" nil nil)
                       ("body" "<body$1>\n  $0\n</body>" "<body>...</body>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/body" nil nil)
                       ("b" "<b>$0</b>" "<b>...</b>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/b.yasnippet" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'nxml-mode
                     '(("h6" "<h6>$1</h6>" "<h6>...</h6>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/header/h6" nil nil)
                       ("h5" "<h5>$1</h5>" "<h5>...</h5>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/header/h5" nil nil)
                       ("h4" "<h4>$1</h4>" "<h4>...</h4>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/header/h4" nil nil)
                       ("h3" "<h3>$1</h3>" "<h3>...</h3>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/header/h3" nil nil)
                       ("h2" "<h2>$1</h2>" "<h2>...</h2>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/header/h2" nil nil)
                       ("h1" "<h1>$1</h1>" "<h1>...</h1>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/header/h1" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'nxml-mode
                     '(("ul" "<ul id=\"$1\">\n  $0\n</ul>" "<ul id=\"...\">...</ul>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/list/ul.id" nil nil)
                       ("ul" "<ul class=\"$1\">\n  $0\n</ul>" "<ul class=\"...\">...</ul>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/list/ul.class" nil nil)
                       ("ul" "<ul>\n  $0\n</ul>" "<ul>...</ul>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/list/ul" nil nil)
                       ("ol" "<ol id=\"$1\">\n  $0\n</ol>" "<ol id=\"...\">...</ol>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/list/ol.id" nil nil)
                       ("ol" "<ol class=\"$1\">\n  $0\n</ol>" "<ol class=\"...\">...</ol>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/list/ol.class" nil nil)
                       ("ol" "<ol>\n  $0\n</ol>" "<ol>...</ol>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/list/ol" nil nil)
                       ("li" "<li class=\"$1\">$2</li>" "<li class=\"...\">...</li>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/list/li.class" nil nil)
                       ("li" "<li>$1</li>" "<li>...</li>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/list/li" nil nil)
                       ("dt" "<dt>$1</dt>" "<dt> ... </dt>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/list/dt" nil nil)
                       ("dl" "<dl id=\"$1\">\n    $0\n</dl>" "<dl> ... </dl>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/list/dl.id" nil nil)
                       ("dl" "<dl>\n    $0\n</dl>" "<dl> ... </dl>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/list/dl" nil nil)
                       ("dd" "<dd>$1</dd>" "<dd> ... </dd>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/list/dd" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'nxml-mode
                     '(("meta" "<meta http-equiv=\"${1:Content-Type}\" content=\"${2:text/html; charset=UTF-8}\" />" "<meta http-equiv=\"...\" content=\"...\" />" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/meta/meta.http-equiv" nil nil)
                       ("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/meta/meta" nil nil)
                       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/meta/doctype.xhtml1_transitional" nil nil)
                       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/meta/doctype.xhtml1_strict" nil nil)
                       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/meta/doctype.xhtml1_1" nil nil)
                       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">" "DocType XHTML 1.0 frameset" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/meta/doctype.xhml1" nil nil)
                       ("doctype" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" "Doctype HTML 4.01 Strict" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/meta/doctype" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'nxml-mode
                     '(("tr" "<tr>\n  $0\n</tr>" "<tr>...</tr>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/table/tr" nil nil)
                       ("th" "<th$1>$2</th>" "<th>...</th>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/table/th" nil nil)
                       ("td" "<td$1>$2</td>" "<td>...</td>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/table/td" nil nil)
                       ("table" "<table width=\"$1\" cellspacing=\"$2\" cellpadding=\"$3\" border=\"$4\">\n  $0\n</table>" "<table ...>...</table>" nil nil nil "/Users/jason/.emacs.d/snippets/nxml-mode/table/table" nil nil)))


;;; Do not edit! File generated at Sat Nov  7 12:16:41 2015
