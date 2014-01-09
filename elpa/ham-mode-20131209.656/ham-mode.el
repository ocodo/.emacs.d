;;; ham-mode.el --- Html As Markdown. Transparently edit an html file using markdown.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/ham-mode
;; Version: 20131209.656
;; X-Original-Version: 1.1.1
;; Package-Requires: ((html-to-markdown "1.2") (markdown-mode "2.0"))
;; Keywords: convenience emulation wp
;; Prefix: ham
;; Separator: -

;;; Commentary:
;;
;; ### Seamlessly edit an html file using markdown. ###
;; 
;; **H**TML **a**s **M**arkdown.
;; 
;; This package defines a major-mode, `ham-mode', which allows you to
;; edit HTML files exactly as if they were Markdown files. Activate it
;; while visiting an HTML file. The buffer will be converted to Markdown,
;; but the file will still be kept in HTML format behind the scenes. Each
;; time you save the Markdown buffer, the file will be updated with the
;; HTML.
;; 
;; **Why?** This is mainly designed to be used with web interfaces which
;; take HTML text (such as some email clients) but whose editors pale in
;; comparison to Emacs (obviously).
;; 
;; This major mode will allow you edit your email (or whatever else
;; you're writing) with the full power of `markdown-mode'. In fact, you
;; will usually be able to write richer structures then client's web
;; interface would normally allow you to (lists within lists, for
;; instance). Just check out <C-h> C-f markdown-mode RET< to> see the full
;; range of commands available for editing.
;; 
;; Instructions
;; ------
;; 
;; To use this package, simply:
;; 
;; 1. Install it from Melpa (M-x `package-install' RET ham-mode) and the
;; `ham-mode' command will be autoloaded.
;; 2. Activate it inside any HTML files you'd like to edit as Markdown.
;; You can manually invoke M-x `ham-mode', or add it to `auto-mode-alist'
;; so that it can load automatically.  
;; For instance, the following snippet will activate `ham-mode' in any
;; `.htm' file containing the word *email*.
;; 
;;         (add-to-list 'auto-mode-alist '(".*email.*\\.html?\\'" . ham-mode))
;; 
;; 

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 

;;; Change Log:
;; 1.1.1 - 2013/12/09 - Fix html4tags in ham-mode-markdown-command.
;; 1.1   - 2013/12/07 - ham-mode-md2html-hook.
;; 1.0   - 2013/12/05 - Created File.
;;; Code:
(require 'html-to-markdown)
(require 'markdown-mode)

(defconst ham-mode-version "1.1.1" "Version of the ham-mode.el package.")
(defconst ham-mode-version-int 3 "Version of the ham-mode.el package, as an integer.")
(defun ham-bug-report ()
  "Opens github issues page in a web browser. Please send any bugs you find.
Please include your emacs and ham-mode versions."
  (interactive)
  (message "Your ham-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           ham-mode-version emacs-version)
  (browse-url "https://github.com/Bruce-Connor/ham-mode/issues/new"))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here starts ham-mode
(defcustom ham-mode-markdown-command
  (list (or (executable-find "markdown")
            (executable-find "Markdown"))
        ;; "--html4tags"
        'file)
  "Command used to convert markdown contents into hmtl.

This variable is a list:
  First element is the full path to the markdown executable.
  Other elements are either the symbol 'file (replaced with the
  filename), or strings (arguments to the passed to the
  executable).

Unfortunately, this variable depends on the implementation so you
might have to tweak it. Some versions of the markdown command
need an \"--html4tags\" argument in order to produce the right
output, while other versions will give an error if you pass them
that argument.
Meanwhile, other implementations require that you specifically
enable some features.

If your executable isn't generating good results (some don't
support all features) you can try to install pandoc and set this
variable to:

    '(\"pandoc\" \"--from\" \"markdown\" \"--to\" \"html\" \"--standalone\" file)"
  :type '(cons (string :tag "Path to the markdown command.")
               (repeat (choice (const :tag "The file being edited." file)
                               (string :tag "String argument."))))
  :group 'html-to-markdown
  :package-version '(ham-mode . "1.1.1"))
(put 'ham-mode-markdown-command 'risky-local-variable-p t)

(defvar ham-mode-md2html-hook nil
  "Hook run after the Markdown buffer is saved as HTML.

Functions in this hook must take one argument, the file name.
They also shouldn't call `save-buffer' or anything like that,
because this is called as an `after-save-hook', so that could
lead to an infinite loop.")

(defun ham-mode--save-as-html ()
  "Take the current markdown buffer, and OVERWRITE its file with HTML.

This is meant to be used as an `after-save-hook', because it
assumes the buffer has already been saved.

The buffer contents won't change (will remain as markdown), but
the visited file will contain HTML code. This means the buffer
and file contents will not match (that's intended). As long as
this is an `after-save-hook', that will happen every time the
buffer is saved, and the file will remain an HTMLized version of
the current buffer."
  (interactive)
  (unless (and (car ham-mode-markdown-command)
	       (file-executable-p (car ham-mode-markdown-command)))
    (error "Can't find the markdown executable! Is it installed? See `ham-mode-markdown-command'"))
  (let ((file (buffer-file-name))
        output return)
    (unless file
      (error (substitute-command-keys "This buffer isn't visiting a file. \\[write-file] to save it.")))
    (setq output 
          (with-temp-buffer
            (setq return
                  (apply 'call-process
                         (car ham-mode-markdown-command)
                         nil t nil
                         (mapcar
                          (lambda (x) (if (eq x 'file) file x))
                          (cdr ham-mode-markdown-command))))
            (buffer-string)))
    (if (/= return 0)
        (error "markdown command failed with output:\n%s" output)
      (write-region output nil file nil t)
      (run-hook-with-args 'ham-mode-md2html-hook file))))

;;;###autoload
(define-derived-mode ham-mode markdown-mode "Ham"
  "Html As Markdown. Transparently edit an html file using markdown.

When this mode is activated in an html file, the buffer is
converted to markdown and you may edit at will, but the file is
still saved as html behind the scenes. 

To have it activate automatically on html files, do something like:
  (add-to-list 'auto-mode-alist '(\".*\\\\.html\\\\'\" . ham-mode))

Initial conversion uses the `html-to-markdown-this-buffer'
command (handled entirely in elisp by this package :-D).

Subsequent conversions (after every save) are handled by the
markdown executable (which needs to be installed on your system).
See `ham-mode-markdown-command' and `ham-mode--save-as-html' on
how to customize this part."
  :group 'html-to-markdown
  (html-to-markdown-this-buffer)
  (set-buffer-modified-p nil)
  (add-hook 'after-save-hook 'ham-mode--save-as-html nil :local))

(provide 'ham-mode)
;;; ham-mode.el ends here.
