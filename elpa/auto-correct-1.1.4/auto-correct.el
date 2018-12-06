;;; auto-correct.el --- Remembers and automatically fixes past corrections -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author: Ian Dunn <dunni@gnu.org>
;; Maintainer: Ian Dunn <dunni@gnu.org>
;; Keywords: editing
;; Version: 1.1.4

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To enable, use:

;; M-x `auto-correct-mode'

;; After that, any future corrections made with flyspell or Ispell (or any other
;; supported package) will be automatically corrected for you as you type.

;; For example, if you type "befroe" and fixed it with `ispell-word',
;; `auto-correct-mode' will change "befroe" to "before" every time you type it
;; from then on.

;; Corrections are only made when `auto-correct-mode' is enabled.  Expansion is
;; case-insensitive, so trying to fix alice as Alice won't work.  Use the
;; captain package for this instead.

;; Auto-correct is controlled further by `auto-correct-predicate'.  In order to
;; enable auto-correct in a given buffer, the function to which
;; `auto-correct-predicate' is set must return true at the current point.

;; For example, the following will tell auto-correct to only correct mistakes in
;; a programming mode buffer that fall within a comment:

;; (add-hook 'prog-mode-hook
;;    (lambda ()
;;      (setq auto-correct-predicate (lambda () (nth 8 (syntax-ppss (point)))))))

;; Or for text modes, work all the time:

;; (add-hook 'text-mode-hook
;;           (lambda ()
;;             (setq auto-correct-predicate (lambda () t))))

;; Or don't work in source blocks in Org mode:

;; (add-hook
;;  'org-mode-hook
;;  (lambda ()
;;    (setq auto-correct-predicate
;;          (lambda () (not (org-in-src-block-p))))))

;; Behind the scenes, auto-correct uses an abbrev table, so in order to clean
;; out or modify any fixes auto-correct has learned, use `list-abbrevs'.  This
;; also means that fixes are saved between Emacs sessions along with the abbrev
;; tables.

;; Ispell and flyspell are the only two packages that auto-correct supports out
;; of the box, but it's possible to add support for any package that corrects
;; text:

;; 1. Create a function that calls `auto-correct--add-or-update-correction' with
;; the old text and the corrected text from your package.

;; 2. Write a function to activate and deactivate support for your package.  It
;; should take a single argument, which is a boolean indicating whether to
;; activate or deactivate support.

;; 3. Call `auto-correct-handle-support', passing t as the first argument and
;; your function as the second.  To disable support, pass nil as the first
;; argument instead.

;; 4. You're done.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'thingatpt)

(defgroup auto-correct nil
  "Auto correction support."
  :prefix "auto-correct-"
  :group 'editing)

;; Core Functionality

(defun auto-correct--default-predicate ()
  "The default predicate for determining whether auto-correct should run.

Disabled by default."
  nil)

(defvar-local auto-correct-predicate #'auto-correct--default-predicate
  "Predicate to check whether automatic corrections should be made.

This should be a function of no arguments that returns non-nil if
auto-correct should operate on the current text.

This is buffer-local so it can be set to a value that works best
with each different mode.

This is `auto-correct--default-predicate' by default, which keeps
auto-correct disabled.  This is to prevent auto-correct from
happening all the time.")

(defun auto-correct-expand-p ()
  "Return non-nil if auto-correct should operate on the current point.

To customize this behavior, set `auto-correct-predicate'."
  (funcall auto-correct-predicate))

(define-abbrev-table 'auto-correct-abbrev-table nil
  "Abbrev table where automatic corrections are stored."
  :enable-function #'auto-correct-expand-p)

(defun auto-correct--get-abbrev-table (local)
  "Get the abbrev table to use with auto-correct.

If LOCAL is non-nil, use the local table if it exists.
Otherwise, use auto-correct's abbrev table."
  (if local
      (or local-abbrev-table auto-correct-abbrev-table)
    auto-correct-abbrev-table))

(defun auto-correct--add-or-update-correction (before after &optional local)
  "Add or update a correction into auto-correct's table.

BEFORE is the misspelled word, and AFTER is the correct spelling.

Optional argument LOCAL determines whether to make the correction
locally.  If nil, the correction will be made whenever
`auto-correct-mode' is enabled."
  (let ((table (auto-correct--get-abbrev-table local))
        (bef (downcase before))
        (aft (downcase after)))
    (define-abbrev table bef aft nil :count 1)
    ;; Save the abbrevs.
    (write-abbrev-file)
    (message "\"%s\" now expands to \"%s\"" bef aft)))

;; The mode

;;;###autoload
(define-minor-mode auto-correct-mode
  "Activate automatic corrections.

Auto correct expansions will only work when this mode is enabled,
but auto-correct can be trained with `auto-correct-fix-and-add'
even if this mode is disabled.

When this mode is enabled, corrections made with flyspell and
Ispell will be made automatically after fixing them once.

In order to add corrections to the auto-correct abbrev table in
flyspell (and thus have them corrected later), set
`flyspell-use-global-abbrev-table-p' to non-nil.

In order to set corrections as local using Ispell, use
the command `auto-correct-toggle-ispell-local'.

\\{auto-correct-mode-map}"
  :group 'auto-correct
  :global t
  :init-value nil
  :lighter " Auto-Correct")

;; Only enable the abbrev list when auto-correct-mode is active.
(add-to-list 'abbrev-minor-mode-table-alist
             `(auto-correct-mode ,auto-correct-abbrev-table)
             'append
             #'equal)

(defsubst auto-correct--support-function (base-function)
  "Return a function that calls BASE-FUNCTION with `auto-correct-mode' as its argument."
  `(lambda ()
     (funcall (quote ,base-function) auto-correct-mode)))

(defun auto-correct-handle-support (activate support-fun)
  "Helper function to add or remove auto-correct support for a package.

If ACTIVATE is non-nil, add support, otherwise remove it.
SUPPORT-FUN is a function that takes a single argument: a boolean
indicating whether to activate or deactivate support."
  (if activate
      (add-hook 'auto-correct-mode-hook (auto-correct--support-function support-fun))
    (remove-hook 'auto-correct-mode-hook (auto-correct--support-function support-fun)))
  ;; If `auto-correct-mode' is enabled, activate or deactivate support.
  (when auto-correct-mode
    (funcall support-fun activate)))

;; Flyspell Support

(defvar-local auto-correct--flyspell-old-word nil)

(defvar flyspell-auto-correct-word)
(defvar flyspell-use-global-abbrev-table-p)
(defvar flyspell-insert-function)

(defun auto-correct--flyspell-do-correct-wrapper (oldfun replace poss word cursor-location start end save)
  "Wraps `flyspell-do-correct' to store the word it's correcting."
  (let ((auto-correct--flyspell-old-word word))
    (funcall oldfun replace poss word cursor-location start end save)))

(defun auto-correct-flyspell-insert (word)
  "Insert WORD and add it as a correction.

The original (misspelled) word is drawn from the variable
`flyspell-auto-correct-word' (if coming from
`flyspell-auto-correct-word') or `auto-correct--flyspell-old-word'
if coming from `flyspell-do-correct'.

When `auto-correct-mode' is enabled, this function is set as
`flyspell-insert-function'."
  ;; If coming from `flyspell-auto-correct-word' (the function), use
  ;; `flyspell-auto-correct-word' (the variable) for the old word.  Otherwise,
  ;; we're coming from `flyspell-do-correct', so use our stored old word.
  (let ((old-word (or flyspell-auto-correct-word
                      auto-correct--flyspell-old-word))
        (new-word word)
        (local (not flyspell-use-global-abbrev-table-p)))
    (auto-correct--add-or-update-correction old-word new-word local)))

(defun auto-correct--activate-flyspell-support (activate)
  "Activate or deactivate auto-correct support for flyspell.

If ACTIVATE is non-nil, activate support for flyspell.
Otherwise, deactivate it.

Activation means adding `auto-correct-flyspell-insert' to
`flyspell-insert-function'."
  (if activate
      (progn
        (advice-add 'flyspell-do-correct :around
                    #'auto-correct--flyspell-do-correct-wrapper)
        (add-function :before flyspell-insert-function
                      #'auto-correct-flyspell-insert))
    (remove-function flyspell-insert-function #'auto-correct-flyspell-insert)
    (advice-remove 'flyspell-do-correct #'auto-correct--flyspell-do-correct-wrapper)))

;; Silence the byte-compiler; this will be enabled shortly
(defvar auto-correct-enable-flyspell-support)

(defun auto-correct-defer-flyspell-support ()
  ;; Don't fully activate flyspell support until after it's loaded.
  (with-eval-after-load 'flyspell
    (auto-correct-handle-support
     auto-correct-enable-flyspell-support
     'auto-correct--activate-flyspell-support)))

(defun auto-correct-set-enable-flyspell-support (sym val)
  (set sym val)
  (auto-correct-defer-flyspell-support))

(defcustom auto-correct-enable-flyspell-support t
  "Whether to automatically correct corrections made in flyspell.

Support will not be enabled until after flyspell has been loaded.

Use the following to set this manually to NEW-VALUE:

(setq auto-correct-enable-flyspell-support NEW-VALUE)
(auto-correct-defer-flyspell-support)"
  :group 'auto-correct
  :type 'boolean
  :set 'auto-correct-set-enable-flyspell-support)

;; Ispell support

(defvar ispell-following-word)

(defvar auto-correct--ispell-use-local-table nil
  "Whether to use the local table with Ispell.

Toggle this interactively with `auto-correct-toggle-ispell-local'.")

(defun auto-correct-toggle-ispell-local ()
  "Toggle whether to use the local or auto-correct table for Ispell."
  (interactive)
  (setq auto-correct--ispell-use-local-table
        (not auto-correct--ispell-use-local-table))
  (message "Auto-Correct is now using the %s table"
           (if auto-correct--ispell-use-local-table "local" "global")))

(defun auto-correct--ispell-handler (ispell-result)
  "Add ISPELL-RESULT as a correction.

The original (misspelled) word is drawn from the function
`word-at-point'.

This is intended to be added as advice to `ispell-command-loop'."
  (when-let ((word-before (word-at-point))
             (correction ispell-result))
    (when (and correction (consp correction))
      ;; The correction was entered by hand.
      (setq correction (car correction)))
    (if (and (not (or (eq correction 0)  ;; Word was corrected from list
                    (eq correction 'quit))) ;; Session was exited
             (not (equal word-before correction))) ;; Word was corrected
        (auto-correct--add-or-update-correction word-before correction
                                                auto-correct--ispell-use-local-table)))
  ispell-result)

(defun auto-correct--activate-ispell-support (activate)
  "Activate or deactivate Ispell auto-correct support.

If ACTIVATE is non-nil, activate support for Ispell.  Otherwise,
deactivate it.

Activating means adding advice to `ispell-command-loop' that adds
the result as a correction."
  (if activate
      (advice-add 'ispell-command-loop :filter-return
                  #'auto-correct--ispell-handler)
    (advice-remove 'ispell-command-loop #'auto-correct--ispell-handler)))

;; We don't defer ispell support because adding advice will work even if the
;; feature hasn't been loaded yet.

(defcustom auto-correct-enable-ispell-support t
  "Whether to automatically correct corrections made in Ispell."
  :group 'auto-correct
  :type 'boolean
  :set (lambda (sym val)
         (set sym val)
         (auto-correct-handle-support
          val
          'auto-correct--activate-ispell-support)))

;; Standalone (piggybacks on Ispell)

;;;###autoload
(defun auto-correct-fix-and-add (local)
  "Use `ispell-word' to fix a misspelled word at point.

Once the misspelled word is fixed, auto-correct will remember the
fix and auto-correct it from then on, so long as
`auto-correct-mode' is enabled.

With a non-nil argument LOCAL (interactively, the prefix argument),
create a fix for the typo that will be auto-corrected for buffers
using the current local mode.

This is pointless to use when `auto-correct-mode' is enabled;
instead, use `ispell-word' and `auto-correct-toggle-ispell-local'
to use the local abbrev table."
  (interactive "P")
  (let ((auto-correct--ispell-use-local-table local))
    (auto-correct--ispell-handler (ispell-word ispell-following-word 'quietly))))

;;;###autoload
(defun auto-correct-scan-buffer ()
  "Scan current buffer for misspelled words.

When a misspelled word is found, offer to correct the misspelled
word and auto-correct the typo in the future.

When `auto-correct-mode' is enabled, use the `ispell' command
instead."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Stop from being prompted to save the personal dictionary after every
    ;; change.
    (cl-letf (((symbol-function 'ispell-pdict-save) #'ignore))
      (while (forward-word)
        (auto-correct-fix-and-add nil)))
    (ispell-pdict-save)))

;;;###autoload
(defun auto-correct-scan-region (start end)
  "Scan the region between START and END for misspelled words.

Interactively, START and END are the current region.

When a misspelled word is found, offer to correct the misspelled
word and auto-correct the typo in the future.

When `auto-correct-mode' is enabled, use the `ispell' command
instead."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (auto-correct-scan-buffer)))

;;;###autoload
(defun auto-correct-scan ()
  "Scan the buffer or region for misspelled words.

When a misspelled word is found, offer to correct the misspelled
word and auto-correct the typo in the future.

When `auto-correct-mode' is enabled, use the `ispell' command
instead."
  (interactive)
  (if (region-active-p)
      (auto-correct-scan-region (region-beginning) (region-end))
    (auto-correct-scan-buffer)))

;;;; ChangeLog:

;; 2018-06-23  Ian Dunn  <dunni@gnu.org>
;; 
;; 	auto-correct: Add support for flyspell-correct-word-*
;; 
;; 	* auto-correct.el (auto-correct--flyspell-old-word): New variable.
;; 	 (auto-correct--flyspell-do-correct-wrapper): Wrapper for
;; 	flyspell-do-correct.
;; 	 (auto-correct-flyspell-insert): Use the new variable.
;; 	 (auto-correct--activate-flyspell-support): Advise flyspell-do-correct.
;; 
;; 2018-01-01  Ian Dunn  <dunni@gnu.org>
;; 
;; 	Updated copyright on auto-correct, captain, and vigenere
;; 
;; 2017-10-21  Ian Dunn  <dunni@gnu.org>
;; 
;; 	auto-correct: Defer loading of flyspell support until after flyspell has
;; 	been enabled
;; 
;; 	* packages/auto-correct/auto-correct.el: Bump version
;; 	 (auto-correct-defer-flyspell-support):
;; 	 (auto-correct-set-enable-flyspell-support): New functions.
;; 	 (auto-correct-enable-flyspell-support): Use them for customization
;; 	setting.
;; 
;; 2017-10-19  Ian Dunn  <dunni@gnu.org>
;; 
;; 	auto-correct: Fixed handle-support bug and bumped version
;; 
;; 	* packages/auto-correct/auto-correct.el
;; 	(auto-correct--support-function): New
;; 	 helper function.
;; 	 (auto-correct-handle-support): Use it, and use the correct hook
;; 	variable.
;; 
;; 2017-10-09  Ian Dunn  <dunni@gnu.org>
;; 
;; 	auto-correct: Bumped version to 1.1.1
;; 
;; 	* packages/auto-correct/auto-correct.el: Bump version
;; 
;; 2017-10-09  Ian Dunn  <dunni@gnu.org>
;; 
;; 	auto-correct: Changed default predicate to a function and added examples
;; 
;; 	* packages/auto-correct/auto-correct.el (Commentary): Added predicate
;; 	examples
;; 	 (auto-correct--default-predicate): New defun
;; 	 (auto-correct-predicate): Make it the default
;; 	 (auto-correct-expand-p): Don't check for predicate being nil
;; 
;; 2017-09-05  Ian Dunn  <dunni@gnu.org>
;; 
;; 	auto-correct.el: Reverted last commit
;; 
;; 	* auto-correct.el (auto-correct-predicate): Use nil by default because
;; 	it's more
;; 	 convenient to explicitly enable auto-correct rather than disable it
;; 	 everywhere.
;; 
;; 2017-09-05  Ian Dunn  <dunni@gnu.org>
;; 
;; 	auto-correct.el: Changed default predicate to enable auto-correct
;; 
;; 2017-09-04  Ian Dunn  <dunni@gnu.org>
;; 
;; 	auto-correct.el: Cleaned up support activation/deactivation
;; 
;; 	* auto-correct.el (auto-correct-activate-functions):
;; 	 (auto-correct-deactivate-functions): Removed in favor of normal mode
;; 	hooks.
;; 	 (auto-correct-mode): Removed use of
;; 	auto-correct-(de)activate-functions.
;; 	 (auto-correct--add-support):
;; 	 (auto-correct--remove-support): Merged into one function.
;; 	 (auto-correct-handle-support): Renamed from
;; 	`auto-correct--handle-support'.
;; 	 (auto-correct--flyspell-activate):
;; 	 (auto-correct--flyspell-deactivate): Merged into...
;; 	 (auto-correct--activate-flyspell-support): This.
;; 	 (auto-correct-enable-flyspell-Support): Use the new function.
;; 	 (auto-correct--ispell-activate):
;; 	 (auto-correct--ispell-deactivate): Merged into...
;; 	 (auto-correct--activate-ispell-support): This.
;; 	 (auto-correct-enable-ispell-support): Use the new function.
;; 
;; 2017-09-04  Ian Dunn  <dunni@gnu.org>
;; 
;; 	Added auto-correct package
;; 
;; 	Single-file package to automatically make corrections found by packages
;; 	such as Ispell and flyspell.
;; 
;; 	* packages/auto-correct/auto-correct.el: Added.
;; 


(provide 'auto-correct)

;;; auto-correct.el ends here
