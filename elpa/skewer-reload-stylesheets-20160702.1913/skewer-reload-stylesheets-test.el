;;; skewer-reload-stylesheets-test.el --- Test package interactively

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; This is just a quick hack to make it easier to play with the package
;; interactively in a clean Emacs instance, without having to jump through hoops.
;;
;; Ideally I'd have an actual test suite, but this is a place to start.

;;; Code:

(require 'scss-mode)

(defvar skewer-reload-stylesheets-test-file load-file-name)

(defun skewer-reload-stylesheets-test-setup ()
  "Set up as much of the skewer infrastructure as we can."
  (setq debug-on-error t)

  (setq httpd-port 9000)
  (run-skewer)

  (let* ((project-dir (file-name-directory skewer-reload-stylesheets-test-file))
         (test-html-file (concat project-dir "/test/css-reloading.html"))
         (test-html-url (concat "file:///" test-html-file))
         (test-css-file (concat project-dir "/test/overrides.css")))
    (find-file test-css-file)
    (skewer-reload-stylesheets-mode t)
    (skewer-reload-stylesheets-reload-on-save)

    (browse-url test-html-url)))

(provide 'skewer-reload-stylesheets-test)
;;; skewer-reload-stylesheets-test.el ends here
