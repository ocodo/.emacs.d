;;; Compiled snippets and support files for `coffee-mode'
;;; contents of the .yas-setup.el support file:
;;;
(defun re-backward-match-group (rexp &optional n)
  "Grab the previous matches of regexp and return the contents of
  the n match group (first group match if no n arg is specified)"
  (save-excursion
    (unless n
      (setq n 1))
    (when (numberp n)
      (when (re-search-backward-lax-whitespace rexp)
        (when  (= (+ 2 (* n 2)) (length (match-data)))
          (match-string-no-properties n))))))

(defun jasmine-coffee-ng/before-each-module-name ()
  "Find the name of a module included by a previous beforeEach"
  (re-backward-match-group "beforeEach(? ?module(? ?['\"]\\(.*\\)['\"])? ?)?"))

(defvar jasmine-ng/dependency-types
  '("Controllers" "Directives" "Services" "Routes" "Filters"))
;;; Snippet definitions:
;;;
(yas-define-snippets 'coffee-mode
                     '(("when" "when ${1:value}\n  ${2:# body...}" "When" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/when" nil nil)
                       ("vali:" "validation:\n  validator: -> ${1:body}\n  message: \"${2:Message}\"$0" "Knockout Extend Validation" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/vali:" nil nil)
                       ("track:" "tracked: { allowNull: ${1:true} }" "Knockout Extend tracked" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/track:" nil nil)
                       ("throt:" "throttle: ${1:100}" "Knockout Extend throttle" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/throt:" nil nil)
                       ("sondr" "spyOn(J.dataStore.${1:class}, \"${2:method}\").andReturn ${3:value} $0" "spyOn dataStore andReturn" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/sondr" nil nil)
                       ("sondp" "spyOn(J.dataStore.${1:class}, \"${2:method}\").andPromise ${3:value} $0" "spyOn dataStore andPromise" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/sondp" nil nil)
                       ("sondc" "spyOn(J.dataStore.${1:class}, \"${2:method}\").andCallThrough() $0" "spyOn dataStore andCallThrough" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/sondc" nil nil)
                       ("sonar" "spyOn(${1:object}, \"${2:method}\").andReturn ${3:value} $0" "spyOn andReturn" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/sonar" nil nil)
                       ("sonap" "spyOn(${1:object}, \"${2:method}\").andPromise ${3:value} $0" "spyOn andPromise" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/sonap" nil nil)
                       ("sonac" "spyOn(${1:object}, \"${2:method}\").andCallThrough() $0" "spyOn andCallThrough" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/sonac" nil nil)
                       ("son" "spyOn(${1:object}, \"${2:method}\")$0" "spyOn" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/son" nil nil)
                       ("shex" "\n@SharedExamples.itBehavesLikeA${1:Thing} = (${2:args}) ->\n\n  describe '@SharedExamples.itBehavesLike$1', ->\n\n    $0" "sharedExample" nil
                        ("jasmine")
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/sharedExample" nil nil)
                       ("req" "${2/^.*?([\\w_]+).*$/\\L$1/} = require ${2:'${1:sys}'}$3" "require" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/require.yasnippet" nil "8A65E175-18F2-428F-A695-73E01139E41A")
                       ("req:" "required: ${1:true}" "Knockout Extend required" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/req:" nil nil)
                       ("rate:" "rateLimit: ${1:100}" "Knockout Extend rateLimit" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/rate:" nil nil)
                       ("pluck" "_.pluck ${1:l}, \"${2:key}\"\n" "_.pluck" nil
                        ("underscore")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/pluck" nil nil)
                       ("num:" "number: ${1:true}" "Knockout Extend number" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/num:" nil nil)
                       ("noti:" "notify: ${1:$$(yas/choose-value '( \"'always'\" \"'reference'\" \"'manual'\" \"function\" ))}" "Knockout Extend notify" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/noti:" nil nil)
                       ("min:" "min: ${1:10}" "Knockout Extend min" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/min:" nil nil)
                       ("map" "_.map ${1:l}, (${2:e})->\n  $0\n" "_.map" nil
                        ("underscore")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/map" nil nil)
                       ("log" "console.log $0" "log" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/log.yasnippet" nil "FBC44B18-323A-4C00-A35B-15E41830C5AD")
                       ("llog" "console.log \"$1\", $1" "llog" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/llog.yasnippet" nil "FBC44B18-323A-4C00-A35B-15E41830C5AD")
                       ("kop" "@${1:computedName} = ko.pureComputed =>\n   ${2:functionBody}\n" "ko.pureComputed" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/kop" nil nil)
                       ("koc" "@${1:computedName} = ko.computed =>\n   ${2:functionBody}\n" "ko.computed" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/koc" nil nil)
                       ("kob" "$1 = ko.observable $0\n" "ko.observable" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/kob" nil nil)
                       ("koa" "$1 = ko.observableArray $0\n" "ko.observableArray" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/koa" nil nil)
                       ("jvm" "J.viewModel" "J.viewModel" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/jvm" nil nil)
                       ("jset" "jset '$1', -> $0" "jset" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/jset" nil nil)
                       ("jlet" "jlet '$1', -> $0" "jlet" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/jlet" nil nil)
                       ("iti" "it 'should $1', inject ($2)->\n  $0" "it 'should ..' with inject" nil
                        ("angular-tests")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/iti" nil nil)
                       ("ithass" "it 'should have a ${1:', inject ($2)->\n" "it has string result" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/ithass" nil nil)
                       ("ithasm" "it 'should have a ${1:MethodName} method', inject (${2:$$(jasmine-coffee-ng/before-each-module-name)})->\n  expect($2.$1).toBeDefined()\n" "check module has method" nil
                        ("angular-tests")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/ithasm" nil nil)
                       ("ithase" "it 'should have ${1:something} that is equal to ${2:result}', ->\n  expect($1).toEqual('$2')" "it has equal" nil
                        ("angular-tests")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/ithase" nil nil)
                       ("ithasd" "it 'should have ${1:$$(jasmine-coffee-ng/before-each-module-name)}.${2:$$(yas/choose-value jasmine-ng/dependency-types)} as a dependency', ->\n  expect(hasModule('$1.$2')).toEqual true\n" "check module has dependency" nil
                        ("angular-tests")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/ithasd" nil nil)
                       ("ita" "it '${1:should}', asyncSpec (done)->\n  $0" "it 'should ..', asyncSpec (done)->" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/ita" nil nil)
                       ("it" "it 'should $1', ->\n  $0" "it 'should ..'" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/it" nil nil)
                       ("inx" "inNextTick ->\n  $0\n  done()\n, ${1:250}\n" "inNextTick" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/inx" nil nil)
                       ("fjx" "spyOn($, 'ajax').andReturn fakeAjax" "fakeAjax spy" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/fjx-spy" nil nil)
                       ("fjx" "fakeAjax.resolve(${1:data})\n" "fakeAjax resolve" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/fjx-resolve" nil nil)
                       ("fjx" "jlet 'fakeAjax', -> $.Deferred()\n" "fakeAjax deferred" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/fjx-deferred" nil nil)
                       ("fil" "_.filter ${1:l}, (${2:e})->\n  $0\n" "_.filter" nil
                        ("underscore")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/fil" nil nil)
                       ("fac" "factories.build${1:class}()\n" "factories.build" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/fac" nil nil)
                       ("etu" "expect($1).toBeUndefined()\n$0" "expect().toBeUndefined" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/etu" nil nil)
                       ("ett" "expect($1).toBeTruthy()\n$0" "expect().toBeTruthy" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/ett" nil nil)
                       ("etoth" "expect( -> ${1:fn()} ).toThrow(${2:e})\n$0" "expect().toThrow" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/etoth" nil nil)
                       ("etn" "expect($1).toBeNull()\n$0" "expect().toBeNull" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/etn" nil nil)
                       ("etm" "expect($1).toMatch($2)\n$0" "expect().toMatch" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/etm" nil nil)
                       ("ethbcw" "expect($1).toHaveBeenCalledWith($2)\n$0\n" "expect().toHaveBeenCalledWith" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/ethbcw" nil nil)
                       ("ethbc" "expect($1).toHaveBeenCalled()\n$0\n" "expect().toHaveBeenCalled" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/ethbc" nil nil)
                       ("etf" "expect($1).toBeFalsy()\n$0" "expect().toBeFalsy" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/etf" nil nil)
                       ("ete" "expect($1).toEqual($2)\n$0" "expect().toEqual" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/ete" nil nil)
                       ("etc" "expect($1).toContain($2)\n$0" "expect().toContain" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/etc" nil nil)
                       ("etbl" "expect($1).toBeLessThan($2)\n$0" "expect().toBeLessThan" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/etbl" nil nil)
                       ("etbg" "expect($1).toBeGreaterThan($2)\n$0" "expect().toBeGreaterThan" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/etbg" nil nil)
                       ("etbd" "expect($1).toBeDefined()\n$0" "expect().toBeDefined" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/etbd" nil nil)
                       ("etbap" "expect($1).toBeAPromise()\n$0\n" "expect().toBeAPromise" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/etbap" nil nil)
                       ("etb" "expect($1).toBe($2)\n$0\n" "expect().toBe" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/etb" nil nil)
                       ("entu" "expect($1).not.toBeUndefined()\n$0" "expect().not.toBeUndefined" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/entu" nil nil)
                       ("entt" "expect($1).not.toBeTruthy()\n$0" "expect().not.toBeTruthy" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/entt" nil nil)
                       ("entoth" "expect( -> ${1:fn()} ).not.toThrow(${2:e})\n$0" "expect().not.toThrow" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/entoth" nil nil)
                       ("entn" "expect($1).not.toBeNull()\n$0" "expect().not.toBeNull" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/entn" nil nil)
                       ("entm" "expect($1).not.toMatch($2)\n$0" "expect().not.toMatch" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/entm" nil nil)
                       ("enthbcw" "expect($1).not.toHaveBeenCalledWith($2)\n$0\n" "expect().not.toHaveBeenCalledWith" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/enthbcw" nil nil)
                       ("enthbc" "expect($1).not.toHaveBeenCalled()\n$0\n" "expect().not.toHaveBeenCalled" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/enthbc" nil nil)
                       ("entf" "expect($1).not.toBeFalsy()\n$0" "expect().not.toBeFalsy" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/entf" nil nil)
                       ("ente" "expect($1).not.toEqual($2)\n$0" "expect().not.toEqual" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/ente" nil nil)
                       ("entc" "expect($1).not.toContain($2)\n$0" "expect().not.toContain" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/entc" nil nil)
                       ("entbl" "expect($1).not.toBeLessThan($2)\n$0" "expect().not.toBeLessThan" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/entbl" nil nil)
                       ("entbg" "expect($1).not.toBeGreaterThan($2)\n$0" "expect().not.toBeGreaterThan" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/entbg" nil nil)
                       ("entbd" "expect($1).not.toBeDefined()\n$0" "expect().not.toBeDefined" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/entbd" nil nil)
                       ("entbap" "expect($1).not.toBeAPromise()\n$0\n" "expect().not.toBeAPromise" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/entbap" nil nil)
                       ("entb" "expect($1).not.toBe($2)\n$0\n" "expect().not.toBe" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/entb" nil nil)
                       ("des" "describe '$1', ->\n  $0\n" "describe" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/des" nil nil)
                       ("deb" "debugger" "debugger" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/deb" nil nil)
                       ("ddeps" "describe \"Dependencies:\", ->\n  deps = undefined\n  hasModule = (m) ->\n    deps.indexOf(m) >= 0\n\n  beforeEach ->\n    deps = module.value(\"${1:$$(jasmine-coffee-ng/before-each-module-name)}\").requires\n\n  $0" "setup describe dependencies" nil
                        ("angular-tests")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/ddeps" nil nil)
                       ("dctrl" "describe \"${1:controller}\", ->\n  {$scope, ctrl} = [null, null]\n\n  beforeEach inject ($rootScope, $controller)->\n    $scope = $rootScope.$new()\n    ctrl  = $controller \"$1\", $scope: $scope\n\n  $0" "describe controller" nil
                        ("angular-tests")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/dctrl" nil nil)
                       ("dat:" "date: ${1:true}" "Knockout Extend date" nil
                        ("knockout")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/dat:" nil nil)
                       ("cspyv" "${1:var} = jasmine.createSpy('${2:object_method}')" "Create spy variable" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/cspyv" nil nil)
                       ("cspyp" "${1:property} : jasmine.createSpy('${2:object_method}')" "Create spy property" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/cspyp" nil nil)
                       ("cspy" "jasmine.createSpy('${2:object_method}')" "Create spy" nil
                        ("jasmine")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/cspy" nil nil)
                       ("chain" "_.chain ${1:a}\n$0\n" "_.chain" nil
                        ("underscore")
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/Users/jason/.emacs.d/snippets/coffee-mode/chain" nil nil)
                       ("bp" "debugger" "(bp for ruby reflexes) debugger" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/bp" nil nil)
                       ("bmod" "beforeEach module \"$1\"\n  $0" "beforeEach module" nil
                        ("angular-tests")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/bmod" nil nil)
                       ("bef" "beforeEach ->\n  $0\n" "beforeEach" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/bef" nil nil)
                       ("asy" "asyncSpec (done) ->" "asyncSpec (done) ->" nil
                        ("jasmine")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/asyncspec" nil nil)
                       ("ameth" "${1:abstractMethod}: (${2:parameters}) -> throw \"[Abstract Method: $1 ($2) ${3:return type}]\"" "Abstract method" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/ameth" nil nil)
                       ("unl" "${1:action} unless ${2:condition}" "Unless" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/Unless.yasnippet" nil "E561AECD-5933-4F59-A6F7-FA96E1203606")
                       ("try" "try\n  $1\ncatch ${2:error}\n  $3" "Try .. Catch" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/Try __ Catch.yasnippet" nil "CAFB0DED-5E23-4A84-AC20-87FBAF22DBAC")
                       ("ifte" "if ${1:condition} then ${2:value} else ${3:other}" "Ternary If" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/Ternary If.yasnippet" nil "CF0B4684-E4CB-4E10-8C25-4D15400C3385")
                       ("swi" "switch ${1:object}\n  when ${2:value}\n    ${3:# body...}" "Switch" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/Switch.yasnippet" nil "3931A7C6-F1FB-484F-82D1-26F5A8F779D0")
                       ("Raw javascript.yasnippet" "\\`${1:`pbpaste`}\\`" "Raw javascript" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/Raw javascript.yasnippet" nil "422A59E7-FC36-4E99-B01C-6353515BB544")
                       ("forr" "for ${1:name} in [${2:start}..${3:finish}]${4: by ${5:step}}\n  ${6:# body...}" "Range comprehension (inclusive)" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/Range comprehension (inclusive).yasnippet" nil "E0F8E45A-9262-4DD6-ADFF-B5B9D6CE99C2")
                       ("forrex" "for ${1:name} in [${2:start}...${3:finish}]${4: by ${5:step}}\n  ${6:# body...}" "Range comprehension (exclusive)" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/Range comprehension (exclusive).yasnippet" nil "FA6AB9BF-3444-4A8C-B010-C95C2CF5BAB3")
                       ("foro" "for ${1:key}, ${2:value} of ${3:Object}\n  ${4:# body...}" "Object comprehension" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/Object comprehension.yasnippet" nil "9D126CC5-EA14-4A40-B6D3-6A5FC1AC1420")
                       ("#" "#{${1:`yas/selected-text`}}" "Interpolated Code" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/Interpolated Code.yasnippet" nil "C04ED189-6ACB-44E6-AD5B-911B760AD1CC")
                       ("if" "if ${1:condition}\n  ${2:# body...}" "If" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/If.yasnippet" nil "F4FDFB3A-71EF-48A4-93F4-178B949546B1")
                       ("ife" "if ${1:condition}\n  ${2:# body...}\nelse\n  ${3:# body...}" "If .. Else" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/If __ Else.yasnippet" nil "2AD19F12-E499-4715-9A47-FC8D594BC550")
                       ("fun" "(${1:args}) ->\n  ${2:# body...}\n\n" "Function" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/Function.yasnippet" nil "F2E2E79A-A85D-471D-9847-72AE40205942")
                       ("elif" "else if ${1:condition}\n  ${2:# body...}" "Else if" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/Else if.yasnippet" nil nil)
                       ("cla" "class ${1:ClassName}${2: extends ${3:Ancestor}}\n\n  ${4:constructor: (${5:args}) ->\n    ${6:# body...}}\n  $7" "Class" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/Class.yasnippet" nil "765ACBD3-380A-4CF8-9111-345A36A0DAE7")
                       ("bfun" "(${1:args}) =>\n  ${2:# body...}" "Function (bound)" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/Bound Function.yasnippet" nil "20BDC055-ED67-4D0E-A47F-ADAA828EFF2B")
                       ("fora" "for ${1:name} in ${2:array}\n  ${3:# body...}" "Array Comprehension" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/Array comprehension.yasnippet" nil "2D4AC0B4-47AA-4E38-9A11-09A48C2A9439")
                       ("fnhttp" "(d, s, h, c)-> $0" "$http callback" nil
                        ("angular-coffee")
                        nil "/Users/jason/.emacs.d/snippets/coffee-mode/Angular Http Callback.yasnippet" nil nil)
                       ("alc" "alert${1:$$(re-backward-match-group \"console.log\\\\\\\\(.*\\\\\\\\)\\\\n\")}\n$0\n" "alert from last console.log" nil nil nil "/Users/jason/.emacs.d/snippets/coffee-mode/Alert Last Console.log.yasnippet" nil nil)))


;;; Do not edit! File generated at Sat Nov  7 12:16:41 2015
