;;; go-gopath.el --- Will guess GOPATH using gb and projectile.

;; Author: Andrew Kirilenko <andrew.kirilenko.main@gmail.com>
;; URL: http://github.com/iced/go-gopath/
;; Package-Version: 20160705.1034
;; Package-Requires: ((cl-lib "0.5"))

;;; License

;; Copyright (c) 2016 Andrew Kirilenko

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'cl-lib)

(defun go-gopath-gb-env-line (name)
  (unless (eq buffer-file-name nil)
    (let ((gbe-env (split-string (shell-command-to-string "gb env") "\n")))
      (cl-find-if (lambda (e) (string-prefix-p name e)) gbe-env))))

(defun go-gopath-gb-env-value (name)
  (let ((env-line (go-gopath-gb-env-line name)))
    (unless (eq env-line nil)
      (substring env-line (+ (length name) 2) -1))))

(defun go-gopath-gb-root ()
  (if (executable-find "gb")
      (go-gopath-gb-env-value "GB_PROJECT_DIR")))

(defun go-gopath-projectile-root ()
  (if (fboundp 'projectile-project-p)
      (if (projectile-project-p)
          (projectile-project-root))))

(defun go-gopath-root ()
  (or
   (go-gopath-gb-root)
   (go-gopath-projectile-root)
   (getenv "GOPATH")
   default-directory))

(defun go-gopath-expand-gopath (gopath)
  (let* ((expanded-gopath (expand-file-name "." gopath))
         (expanded-vendor-gopath (expand-file-name "vendor" gopath)))
    (if (file-exists-p expanded-vendor-gopath)
        (concat expanded-gopath path-separator expanded-vendor-gopath)
      expanded-gopath)))

;;;###autoload
(defun go-gopath-set-gopath (gopath)
  (interactive
   (list
    (read-directory-name "GOPATH=" (expand-file-name "." (go-gopath-root)))))
  (setenv "GOPATH" (go-gopath-expand-gopath gopath)))


(provide 'go-gopath)
;;; go-gopath.el ends here
