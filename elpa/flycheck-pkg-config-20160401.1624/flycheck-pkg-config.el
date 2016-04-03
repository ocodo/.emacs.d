;;; flycheck-pkg-config.el --- configure flycheck using pkg-config  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: flycheck
;; Package-Version: 20160401.1624
;; Version: 0.1
;; Package-Requires: ((dash "2.8.0") (s "1.9.0") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Flycheck defines a `flycheck-clang-include-path' variable that it
;; searches for headers when checking C/C++ code.
;;
;; This package provides a convenient way of adding libraries to that
;; list, using pkg-config and completion.

;;; Code:

(require 's)
(require 'dash)
(require 'flycheck)

(defvar flycheck-pkg-config--libs nil)

(defun flycheck-pkg-config--ignore-case-less-p (s1 s2)
  (string< (downcase s1) (downcase s2)))

;;;###autoload
(defun flycheck-pkg-config ()
  "Configure flycheck to use additional includes
when checking the current buffer."
  (interactive)
  ;; Find out all the libraries installed on this system.
  (unless flycheck-pkg-config--libs
    (let* ((all-libs-with-names
            (shell-command-to-string "pkg-config --list-all"))
           (lines (s-split "\n" (s-trim all-libs-with-names)))
           (libs (--map (-first-item (s-split " " it)) lines)))
      (setq flycheck-pkg-config--libs (-sort #'flycheck-pkg-config--ignore-case-less-p libs))))

  (let* (;; Prompt for a library name.
         (lib-name (completing-read "Library name: " flycheck-pkg-config--libs))
         ;; Find the include flags, e.g. "-I/usr/lib/foo"
         (pkgconfig-cmd (format "pkg-config --cflags %s" lib-name))
         (cc-args (s-trim (shell-command-to-string pkgconfig-cmd))))
    (if (s-starts-with? "-I" cc-args)
        ;; pkg-config has found a library with this name.
        (let* ((include-args (s-split " " cc-args))
               (lib-paths (--map (s-chop-prefix "-I" it) include-args)))
          ;; Only set in this buffer.
          (make-local-variable 'flycheck-clang-include-path)
          ;; Add include paths to `flycheck-clang-include-path' unless
          ;; already present.
          (setq flycheck-clang-include-path
                (-union flycheck-clang-include-path
                        lib-paths))
          (message "flycheck-clang-include-path: %s"
                   flycheck-clang-include-path))
      ;; Otherwise, no such library with this name.
      (user-error cc-args))))

(provide 'flycheck-pkg-config)
;;; flycheck-pkg-config.el ends here
