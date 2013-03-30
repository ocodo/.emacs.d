;;; flymake-lua.el --- Flymake for Lua
;; Author: Sebastien Roccaserra
;; URL: https://github.com/sroccaserra/emacs/blob/master/flymake-lua.el
;; Version: 1.0

;;
;; Flymake for Lua
;;
;; Usage:
;;   (require 'flymake-lua)
;;   (add-hook 'lua-mode-hook 'flymake-lua-load)
;;
;; Note: litterally stolen from Steve Purcell's Flymake Ruby.
;; See http://github.com/purcell/emacs.d/blob/master/site-lisp/flymake-ruby/flymake-ruby.el
;;

(require 'flymake)

(defgroup flymake-lua nil
  "Flymake Lua Customizations")

(defcustom flymake-luac-program "luac"
  "How to invoke luac. Other possible value: /usr/local/bin/luac."
  :type 'file
  :group 'flymake-lua)

(defun flymake-create-temp-in-system-tempdir (filename prefix)
  (make-temp-file (or prefix "flymake-lua")))

(defun flymake-lua-init ()
  (list flymake-luac-program
        (list "-p" (flymake-init-create-temp-buffer-copy
                    'flymake-create-temp-in-system-tempdir))))

(defvar flymake-lua-allowed-file-name-masks '(("\\.lua\\'" flymake-lua-init)))
(defvar flymake-lua-err-line-patterns '(("^.*luac[0-9.]*\\(.exe\\)?: *\\(.*\\):\\([0-9]+\\): \\(.*\\)$"
                                    2 3 nil 4)))

;;;###autoload
(defun flymake-lua-load ()
  (interactive)
  (when (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
    (set (make-local-variable 'flymake-allowed-file-name-masks) flymake-lua-allowed-file-name-masks)
    (set (make-local-variable 'flymake-err-line-patterns) flymake-lua-err-line-patterns)
    (flymake-mode t)))

(provide 'flymake-lua)

;;; flymake-lua.el ends here
