;;; use-git-gutter --- initialize git-gutter
;;; Commentary:
;;; Code:

(if (window-system)
    (progn (require 'git-gutter-fringe+))
  (progn (require 'git-gutter+)))

(defhydra hydra-git-gutter (:body-pre (git-gutter+-mode 1)
                            :hint nil)
  "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
  ("j" git-gutter+-next-hunk)
  ("k" git-gutter+-previous-hunk)
  ("h" (progn (goto-char (point-min))
              (git-gutter+-next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter+-previous-hunk 1)))
  ("s" git-gutter+-stage-hunks)
  ("r" git-gutter+-revert-hunk)
  ("p" git-gutter+-popup-hunk)
  ("R" git-gutter+-set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter+-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter+-clear))
       :color blue))

(add-hook 'prog-mode-hook 'git-gutter+-turn-on)
(add-hook 'css-mode-hook 'git-gutter+-turn-on)

(provide 'use-git-gutter)
;;; use-git-gutter ends here
