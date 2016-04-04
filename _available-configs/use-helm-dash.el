(require 'helm-dash)

(defun helm-dash-start-localhost-server ()
  "Start a localhost server on port 5000 via a ruby sub process, and serve dash docsets."
  (start-process "dash-docset-localhost-server" "*dash-docset-localhost-server*"
                 "ruby" "-run" "-e" "httpd" helm-dash-docsets-path "-p5000"))

(defun helm-dash-browse-localhost-url (url)
  "Convert file:///URL to http://localhost:5000/ for use with dash local server"
  (let* ((docsets (concat "file://" helm-dash-docsets-path))
         (localurl (replace-regexp-in-string (regexp-quote docsets) "http://localhost:5000" url nil 'literal)))
    (unless (processp 'dash-docset-localhost-server)
      (helm-dash-start-localhost-server))
    (message "%s" localurl)
    (browse-url localurl)))

(setq helm-dash-browser-func 'helm-dash-browse-localhost-url)

(defvar helm-dash-required-docsets '() "A list of required helm-dash-docsets")

(setq helm-dash-required-docsets
      '(
        ;; TODO: Fix list, make alist of installed docset name and install name
        ;; (or maybe figure out why they differ.)

        Ruby
        Ruby_on_Rails_4
        Haml
        SVG
        HTML

        CoffeeScript
        JavaScript

        Jasmine
        AngularJS
        BackboneJS
        KnockoutJS
        Lo-Dash
        Meteor
        D3JS
        jQuery_UI
        jQuery
        UnderscoreJS
        MomentJS
        YUI
        NodeJS
        EmberJS
        Express
        Jade

        Bootstrap_2
        Bootstrap_3
        Foundation
        Font_Awesome

        CSS
        Sass
        Less

        Emacs_Lisp
        Common_Lisp
        Lua_5.3

        Elixir
        Clojure
        Dart
        Go
        R
        Scala
        Play_Scala
        Groovy
        Grails
        Haskell
        OCaml

        LaTeX

        Chef
        Ansible
        Bash
        Nginx
        PostgreSQL

        Python_2
        NumPy
        Django
        Flask
        OpenCV_Python
        OpenCV_C

        C++
        Boost
        C
        GLib

        OpenGL2
        OpenGL3
        OpenGL4

        ElasticSearch

        Android

        Swift
        AppleScript
        OS_X
        iOS
        ))

(defun ocodo-helm-dash-install-docsets ()
  "Install required docsets"
  (interactive)
  (dolist (doc (mapcar 'symbol-name helm-dash-required-docsets))
    (let ((docset (replace-regexp-in-string "_" " " doc)))
      (message (format "Check docset: '%s'" docset))
      (when (not (member docset (helm-dash-installed-docsets)))
        (message (format "Installing docset: '%s'" docset))
        (helm-dash-install-docset doc)))))

(defun ocodo-helm-dash-upgrade-docsets ()
  "Upgrade installed docsets"
  (interactive)
  (dolist (doc (helm-dash-installed-docsets))
    (message (format "Upgrading docset '%s'" doc))
    (helm-dash-update-docset doc)))

;; TODO: Setup Dash docsets activation of jslib + JS
;; TODO: Setup Dash docsets activation of jslib + coffeescript
;; TODO: Setup Dash docsets activation of rails stack
;; TODO: Setup Dash docsets activation of html + css + ...
;; TODO: Setup Dash docsets activation of scss + css + ...
;; TODO: Setup Dash docsets activation of less + css + ...
;; TODO: Setup Dash docsets activation of gems / ruby related
;; TODO: Setup Dash docsets activation of languages of interest (elixir, haskell, ocaml, ...etc)
;; TODO: Setup Dash docsets activation of c + opengl, opencv
;; TODO: Setup Dash docsets activation of python + tools (django, numpy etc)

;; ;; By default, no docsets are enabled.
;; (setq helm-dash-common-docsets nil)

;; (defun helm-dash-js-libs ()
;;   (interactive)
;;   (setq-local
;;    helm-dash-docsets
;;    '("BackboneJS" "jQuery" "jQuery_UI" "Jasmine" "Jade" "KnockoutJS" "Lo-Dash" "NodeJS" "YUI" "EmberJS")))

;; (defun helm-dash-js ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("JavaScript")))
;; (add-hook 'js2-mode-hook 'helm-dash-js)
;; (add-hook 'js2-mode-hook 'helm-dash-js-libs)

;; (defun helm-dash-coffeescript ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("CoffeeScript")))
;; (add-hook 'coffee-mode-hook 'helm-dash-coffeescript)
;; (add-hook 'coffee-mode-hook 'helm-dash-js-libs)

;; (defun helm-dash-haml ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("Haml" "CSS")))
;; (add-hook 'haml-mode-hook 'helm-dash-haml)

;; (defun helm-dash-html ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("Html" "Font_Awesome" "CSS")))
;; (add-hook 'html-mode-hook 'helm-dash-html)
;; (add-hook 'web-mode-hook 'helm-dash-html)

;; (defun helm-dash-css ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("CSS")))
;; (add-hook 'css-mode-hook 'helm-dash-css)

;; (defun helm-dash-sass ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("CSS" "Sass")))
;; (add-hook 'sass-mode-hook 'helm-dash-sass)

;; (defun helm-dash-less ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("CSS" "Less")))
;; (add-hook 'less-mode-hook 'helm-dash-less)

;; (defun helm-dash-shell ()
;;   (interactive)
;;   (setq-local helm-dash-docsets '("Bash")))
;; (add-hook 'sh-mode-hook 'helm-dash-shell)

(provide 'use-helm-dash)
