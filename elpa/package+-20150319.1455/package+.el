;;; package+.el --- Extensions for the package library.

;; Copyright (C) Ryan Davis

;; Author: Ryan Davis <ryand-ruby@zenspider.com>
;; Keywords: extensions, tools
;; Package-Version: 20150319.1455
;; Package-Requires: ()
;; URL: TBA
;; Doc URL: TBA
;; Compatibility: GNU Emacs: 24.3?, 24.4

;;; The MIT License:

;; http://en.wikipedia.org/wiki/MIT_License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Provides extensions to `package.el` for Emacs 24 and later.

;; Declares a manifest of packages that should be installed on this
;; system, installing any missing packages and removing any installed
;; packages that are not in the manifest.
;;
;; This makes it easy to keep a list of packages under version control
;; and replicated across all your environments, without having to have
;; all the packages themselves under version control.
;;
;; Example:
;;
;;    (package-initialize)
;;    (add-to-list 'package-archives
;;      '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;    (unless (package-installed-p 'package+)
;;      (package-install 'package+))
;;
;;    (package-manifest 'ag
;;                      'expand-region
;;                      'magit
;;                      'melpa
;;                      'package+
;;                      'paredit
;;                      'ruby-mode
;;                      'ssh
;;                      'window-number)

;;; Note:

;; Call (package-refresh-contents) before running (package-manifest ...)
;; if you want to ensure you're using the latest package data.

;; package-version-for, package-delete-by-name, package-maybe-install,
;; package-cleanup, package-deps-for, and package-transitive-closure
;; are all going to be submitted upstream to emacs. They're in here
;; and only defined if package-cleanup is not already defined. If my
;; contributions get accepted upstream, they'll be deleted here at
;; some point.

;; If automatic package cleanup is not desired (for example, if you have
;; locally-installed packages you want to keep), you can disable this
;; functionality by setting package-disable-cleanup, like so:
;; 
;;    (setq package-disable-cleanup 1)
;;    (package manifest 'foo
;;                      'bar
;;                      ... )

;;; Code:

(require 'package)

(unless (fboundp 'package-desc-version)
  ;; provide compatibilty back to 24.3--hopefully.
  (defsubst package-desc-version (desc)
    "Extract version from a package description vector."
    (aref desc 0)))

(unless (fboundp 'package-cleanup)
  (require 'cl)

  (defun package-details-for (name)
    (let ((v (cdr (assoc name (append package-alist package-archive-contents)))))
      (and v (if (consp v)
                 (car v)                ; emacs 24+
               v))))                    ; emacs 23

  (defun package-version-for (name)
    "Returns the installed version for a package with a given NAME."
    (package-desc-version (package-details-for name)))

  (defun package-delete-by-name (name)
    "Deletes a package by NAME"
    (message "Removing %s" name)
    (package-delete (package-details-for name)))

  (defun package-maybe-install (name)
    "Installs a package by NAME, but only if it isn't already installed."
    (unless (package-installed-p name)
      (message "Installing %s" name)
      (package-install name)))

  (defun package-deps-for (pkg)
    "Returns the dependency list for PKG or nil if none or the PKG doesn't exist."
    (unless package-archive-contents
      (package-refresh-contents))
    (let ((v (package-details-for pkg)))
      (and v (package-desc-reqs v))))

  (defun package-transitive-closure (pkgs)
    "Return a list of dependencies for PKGS, including dependencies of dependencies."
    (let ((prev)
          (deps pkgs))
      (while (not (equal prev deps))
        (setq prev deps)
        (dolist (pkg deps)
          (dolist (new-pkg (mapcar 'car (package-deps-for pkg)))
            (add-to-list 'deps new-pkg))))
      deps))

  (defun package-cleanup (packages)
    "Delete installed packages not explicitly declared in PACKAGES."
    (let ((removes (set-difference (mapcar 'car package-alist)
                                   (package-transitive-closure packages))))
      (mapc 'package-delete-by-name removes))))

;;;###autoload
(defun package-manifest (&rest manifest)
  "Ensures MANIFEST is installed and uninstalls other packages.
MANIFEST declares a list of packages that should be installed on
this system, installing any missing packages and removing any
installed packages that are not in the manifest.

This makes it easy to keep a list of packages under version
control and replicated across all your environments, without
having to have all the packages themselves under version
control."
  (package-initialize)

  (unless package-archive-contents      ; why? package-install has this.
    (package-refresh-contents))

  (condition-case err
      (mapc 'package-maybe-install (package-transitive-closure manifest))
    (error (message "Couldn't install package: %s" err)))
  (unless (boundp 'package-disable-cleanup) (package-cleanup manifest)))

(provide 'package+)

;;; package+.el ends here
