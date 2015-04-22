;;; clean-mode-line
;;; http://blog.jr0cket.co.uk/2013/01/tweeking-emacs-modeline-for-clojure.html
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas-minor-mode . " γ")
    (paredit-mode . " Φ")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (whitespace-mode . " б")
    (undo-tree-mode . " τ")
    (volatile-highlights-mode . " υ")
    (elisp-slime-nav-mode . " δ")
    (nrepl-mode . " ηζ")
    (nrepl-interaction-mode . " ηζ")
    (projectile-mode . " п")
    (ropemacs-mode . " р")
    (rainbow-mode . " ιρις")
    (flymake-mode . " ικαρ")
    (elpy-mode . " επ")
    ;; Major modes
    (clojure-mode . "Λ")
    (hi-lock-mode . "")
    (python-mode . "Π")
    (emacs-lisp-mode . "ЕЛ")
    (lisp-interaction-mode . "Λ")
    (markdown-mode . "📓")
    (coffee-mode . "☕")
    (less-css-mode . ""))

  "Alist for `clean-mode-line'.
When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;; idea from http://blog.jr0cket.co.uk/2013/01/tweeking-emacs-modeline-for-clojure.html
;;; Greek letters - C-u C-\ greek ;; C-\ to revert to default
;;; ς Ε Ρ Τ Υ Θ Ι Ο Π Α Σ Δ Φ Γ Η Ξ Κ Λ Ζ Χ Ψ Ω Β Ν Μ
;;; ς ε ρ τ υ θ ι ο π α σ δ φ γ η ξ κ λ ζ χ ψ ω β ν μ
;;; Russian letters
;;; А Б В Г Д Е Ё Ж З И Й К Л М Н О П Р С Т У Ф Х Ц Ч Ш Щ Ы Э Ю Я
;;; а б в г д е ё ж з и й к л м н о п р с т у ф х ц ч ш щ ы э ю я
