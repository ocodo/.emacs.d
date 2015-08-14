;; Rubu mode init

(require 'ruby-mode)

(defun ruby-toggle-symbol-at-point ()
  "Dirt simple, just prefix current word with a colon."
  (interactive)
  (operate-on-point-or-region 'ruby-toggle-symbol-name))

(defun ruby-make-interpolated-string-at-point-or-region ()
  "Simple conversion of string/reigion to ruby interpolated string."
  (interactive)
  (operate-on-point-or-region 'ruby-interpolated-string))

(defun ruby-interpolated-string (s)
  "Make a ruby interpolated string entry S is a string."
  (format "#{%s}" s))

(defun ruby-prepend-colon (s)
  "Prepend a colon on the provided string S."
  (format ":%s" s))

(defun ruby-toggle-symbol-name (s)
  "Toggle colon prefix on string S."
  (if (s-matches? "^:.*" s)
      (s-replace ":" "" s)
    (ruby-prepend-colon s)))

;; Saved macro to replace selection with try(:selection)
(fset 'ruby-selected-to-try-call [?\C-w ?t ?r ?y ?\( ?: ?\C-y right])

;; Saved macro to replace var assignment with a let (rspec)
(fset 'rspec-var-to-let
   [?l ?e ?t ?  ?\C-s ?= left delete backspace ?\C-c ?: ?\" left delete delete ?\C-  ?\C-r ?  right ?\{ right left ?\C-x ?r ?r ?\( right left ?\C-s ?  right left ?\C-  ?\C-e ?\{ left ?  ?\C-a ?\C-s ?\{ left right ?  end])

(dolist (p '("\\.rb$"
             "Guardfile"
             "^Rakefile$"
             "\.rake$"
             "\.rxml$"
             "\.rhtml$"
             "\.erb$"
             "\.rjs$"
             "\.irbrc$"
             "\.builder$"
             "\.ru$"
             "\.rabl$"
             "\.gemspec$"
             "Gemfile$"
             "^.pryrc$"
             "^config.ru$"))
  (add-to-list 'auto-mode-alist (cons p 'ruby-mode)))

(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-mode-hook 'ruby-end-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))
;; (add-hook 'ruby-mode-hook 'company-robe)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(define-key ruby-mode-map (kbd "C-c #") 'ruby-make-interpolated-string-at-point-or-region)
(define-key ruby-mode-map (kbd "C-c :") 'ruby-toggle-symbol-at-point)
(define-key ruby-mode-map (kbd "C-c {") 'ruby-toggle-block)
(define-key ruby-mode-map (kbd "C-c +") 'ruby-toggle-hash-syntax)

(provide 'init-ruby)
