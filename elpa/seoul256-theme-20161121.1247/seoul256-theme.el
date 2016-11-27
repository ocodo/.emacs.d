;;; seoul256-theme.el --- Low-contrast color scheme based on Seoul Colors.

;; Copyright (C) 2016 Anand Iyer

;; Author: Anand Iyer <anand.ucb@gmail.com>
;; Maintainer: Anand Iyer <anand.ucb@gmail.com>
;; URL: http://github.com/anandpiyer/seoul256-emacs
;; Package-Version: 20161121.1247
;; Created: 21 October 2016
;; Version: 0.3.1
;; Keywords: theme
;; Package-Requires: ((emacs "24.3"))

;;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; A port of the Seoul256 color scheme to Emacs.
;;
;; This port contains some modifications, partly due to the differences
;; ih customization flexibility offered by Emacs and Vim.  However, I have
;; tried to stay true to the original as much as possible.

;;; Credits:

;; Junegunn Choi created the original theme for Vim on which this port is based.

;;; Code:

(require 'cl-lib)

(when (version< emacs-version "24.3")
  (error "Requires Emacs 24.3 or later"))

(defgroup seoul256 nil
  "seoul256 theme"
  :group 'faces)

(defcustom seoul256-background 237
  "Background color for seoul256 scheme."
  :type 'number
  :group 'seoul256)

(defcustom seoul256-alternate-background 253
  "Alternate background color for seoul256 scheme."
  :type 'number
  :group 'seoul256)

(deftheme seoul256 "Low-contrast color scheme based on Seoul Colors")

(defvar seoul256-default-colors-alist
  '((16 . "#000000") (17 . "#0C0077") (18 . "#14009F") (19 . "#1B00C5") (20 . "#2200E8") (21 .
    "#2900FF") (22 . "#007600") (23 . "#007475") (24 . "#00739E") (25 . "#0071C3") (26 .
    "#006EE7") (27 . "#006BFF") (28 . "#009E00") (29 . "#009D72") (30 . "#009C9C") (31 .
    "#009AC2") (32 . "#0098E6") (33 . "#0096FF") (34 . "#00C300") (35 . "#00C26F") (36 .
    "#00C19A") (37 . "#00C0C1") (38 . "#00BFE5") (39 . "#00BDFF") (40 . "#00E600") (41 .
    "#00E56B") (42 . "#00E497") (43 . "#00E3BF") (44 . "#00E2E3") (45 . "#00E1FF") (46 .
    "#00FF00") (47 . "#00FF66") (48 . "#00FF94") (49 . "#00FFBC") (50 . "#00FFE1") (51 .
    "#00FFFF") (52 . "#7F0000") (53 . "#800075") (54 . "#81009E") (55 . "#8200C3") (56 .
    "#8300E7") (57 . "#8500FF") (58 . "#717400") (59 . "#727272") (60 . "#73709C") (61 .
    "#746EC2") (62 . "#766CE6") (63 . "#7868FF") (64 . "#629C00") (65 . "#639B6F") (66 .
    "#649A9A") (67 . "#6698C1") (68 . "#6897E5") (69 . "#6A95FF") (70 . "#49C200") (71 .
    "#4BC16C") (72 . "#4DC098") (73 . "#4FBFBF") (74 . "#52BDE3") (75 . "#54BCFF") (76 .
    "#07E500") (77 . "#12E468") (78 . "#18E395") (79 . "#1EE2BD") (80 . "#25E1E2") (81 .
    "#2BE0FF") (82 . "#00FF00") (83 . "#00FF63") (84 . "#00FF92") (85 . "#00FFBB") (86 .
    "#00FFE0") (87 . "#00FFFF") (88 . "#AA0000") (89 . "#AA0072") (90 . "#AB009C") (91 .
    "#AC00C2") (92 . "#AD00E6") (93 . "#AE00FF") (94 . "#A07200") (95 . "#A1706F") (96 .
    "#A16E9A") (97 . "#A26CC1") (98 . "#A369E5") (99 . "#A566FF") (100 . "#979B00") (101 .
    "#989A6D") (102 . "#989898") (103 . "#9997BF") (104 . "#9A95E4") (105 . "#9C93FF") (106 .
    "#8AC000") (107 . "#8BC06A") (108 . "#8CBF96") (109 . "#8DBEBE") (110 . "#8EBCE2") (111 .
    "#90BBFF") (112 . "#79E400") (113 . "#7AE365") (114 . "#7BE294") (115 . "#7CE1BC") (116 .
    "#7DE0E1") (117 . "#7FDFFF") (118 . "#5FFF00") (119 . "#60FF60") (120 . "#62FF90") (121 .
    "#63FFBA") (122 . "#65FFDF") (123 . "#67FFFF") (124 . "#D10000") (125 . "#D2006F") (126 .
    "#D2009A") (127 . "#D300C1") (128 . "#D300E5") (129 . "#D400FF") (130 . "#CA6F00") (131 .
    "#CA6D6C") (132 . "#CB6B98") (133 . "#CC69BF") (134 . "#CC66E3") (135 . "#CD63FF") (136 .
    "#C39900") (137 . "#C4986A") (138 . "#C49796") (139 . "#C595BE") (140 . "#C693E2") (141 .
    "#C791FF") (142 . "#BABF00") (143 . "#BBBE66") (144 . "#BCBD94") (145 . "#BCBCBC") (146 .
    "#BDBBE1") (147 . "#BEB9FF") (148 . "#AFE300") (149 . "#B0E262") (150 . "#B0E191") (151 .
    "#B1E0BA") (152 . "#B2DFE0") (153 . "#B3DEFF") (154 . "#A0FF00") (155 . "#A1FF5C") (156 .
    "#A2FF8E") (157 . "#A2FFB8") (158 . "#A3FFDE") (159 . "#A5FFFF") (160 . "#F60000") (161 .
    "#F7006B") (162 . "#F70097") (163 . "#F800BF") (164 . "#F800E3") (165 . "#F900FF") (166 .
    "#F16C00") (167 . "#F16A68") (168 . "#F16895") (169 . "#F265BD") (170 . "#F363E2") (171 .
    "#F35FFF") (172 . "#EB9700") (173 . "#EC9565") (174 . "#EC9494") (175 . "#ED93BC") (176 .
    "#ED91E1") (177 . "#EE8FFF") (178 . "#E4BD00") (179 . "#E5BC62") (180 . "#E5BC91") (181 .
    "#E6BBBA") (182 . "#E7B9E0") (183 . "#E7B8FF") (184 . "#DCE100") (185 . "#DCE15D") (186 .
    "#DDE08F") (187 . "#DDDFB8") (188 . "#DEDEDE") (189 . "#DFDDFF") (190 . "#D1FF00") (191 .
    "#D2FF57") (192 . "#D2FF8B") (193 . "#D3FFB6") (194 . "#D4FFDC") (195 . "#D4FFFF") (196 .
    "#FF0000") (197 . "#FF0066") (198 . "#FF0094") (199 . "#FF00BC") (200 . "#FF00E1") (201 .
    "#FF00FF") (202 . "#FF6700") (203 . "#FF6563") (204 . "#FF6392") (205 . "#FF61BB") (206 .
    "#FF5EE0") (207 . "#FF5AFF") (208 . "#FF9400") (209 . "#FF9360") (210 . "#FF9291") (211 .
    "#FF90BA") (212 . "#FF8EDF") (213 . "#FF8CFF") (214 . "#FFBB00") (215 . "#FFBA5C") (216 .
    "#FFBA8E") (217 . "#FFB9B8") (218 . "#FFB7DE") (219 . "#FFB6FF") (220 . "#FFE000") (221 .
    "#FFDF57") (222 . "#FFDE8B") (223 . "#FFDDB6") (224 . "#FFDCDC") (225 . "#FFDBFF") (226 .
    "#FCFF00") (227 . "#FCFF51") (228 . "#FDFF88") (229 . "#FDFFB4") (230 . "#FEFFDA") (231 .
    "#FEFEFE") (232 . "#060606") (233 . "#171717") (234 . "#252525") (235 . "#323232") (236 .
    "#3F3F3F") (237 . "#4A4A4A") (238 . "#565656") (239 . "#606060") (240 . "#6B6B6B") (241 .
    "#757575") (242 . "#7F7F7F") (243 . "#888888") (244 . "#929292") (245 . "#9B9B9B") (246 .
    "#A4A4A4") (247 . "#ADADAD") (248 . "#B6B6B6") (249 . "#BFBFBF") (250 . "#C7C7C7") (251 .
    "#D0D0D0") (252 . "#D8D8D8") (253 . "#E0E0E0") (254 . "#E9E9E9") (255 . "#F1F1F1") (256 .
    "#FFFFFF")))

(defvar seoul256-override-colors-alist
  '()
  "Use this alist to override the theme's default colors.")

(defvar seoul256-colors-alist
  (append seoul256-default-colors-alist seoul256-override-colors-alist))

(defvar seoul256-current-bg nil
  "Current background used by seoul256 theme.")

(defun seoul256-apply (theme style dark-fg light-fg dark-bg light-bg)
  "Apply theme THEME, a STYLE variant of seoul256 theme using DARK-FG, LIGHT-FG, DARK-BG and LIGHT-BG as main colors."
  (cl-flet ((hex (dark light)
                 (let ((color-id dark))
                   (when (string= style "light")
                     (setq color-id light))
                   (cdr (assoc color-id seoul256-colors-alist)))))

    (custom-theme-set-faces
     theme

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;; in-built
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; basic ui
     '(button                       ((t (:underline t))))
     `(cursor                       ((t (:background ,(hex light-bg dark-bg)))))
     `(default                      ((t (:foreground ,(hex dark-fg light-fg) :background ,(hex dark-bg light-bg)))))
     `(fringe                       ((t (:foreground ,(hex dark-fg light-fg) :background ,(hex (- dark-bg 1) (- light-bg 1))))))
     `(header-line                  ((t (:foreground ,(hex 256 16)))))
     `(highlight                    ((t (:background ,(hex (- dark-bg 1) (- light-bg 1))))))
     `(hl-line                      ((t (:background ,(hex (- dark-bg 1) (- light-bg 1))))))
     `(isearch                      ((t (:foreground ,(hex 220 220) :background ,(hex (+ dark-bg 1) 238)))))
     `(isearch-lazy-highlight-face  ((t (:inherit match))))
     `(isearch-fail                 ((t (:foreground ,(hex 196 196) :background ,(hex (+ dark-bg 3) 253)))))
     `(link                         ((t (:foreground ,(hex 73 23)))))
     `(link-visited                 ((t (:foreground ,(hex 72 22)))))
     `(linum                        ((t (:foreground ,(hex 101 101) :background ,(hex (+ dark-bg 1) (- light-bg 2))))))
     `(match                        ((t (:foreground ,(hex dark-fg 255) :background ,(hex 24 74)))))
     `(minibuffer-prompt            ((t (:foreground ,(hex 74 24) :weight bold))))
     `(region                       ((t (:background ,(hex 23 152)))))

     ;; font-lock
     `(font-lock-builtin-face            ((t (:foreground ,(hex 179 94)))))
     `(font-lock-comment-delimiter-face  ((t (:foreground ,(hex 65 65)))))
     `(font-lock-comment-face            ((t (:foreground ,(hex 65 65)))))
     `(font-lock-constant-face           ((t (:foreground ,(hex 73 23)))))
     `(font-lock-doc-face                ((t (:inherit font-lock-comment-face))))
     `(font-lock-function-name-face      ((t (:foreground ,(hex 187 58)))))
     `(font-lock-keyword-face            ((t (:foreground ,(hex 168 168)))))
     `(font-lock-preprocessor-face       ((t (:foreground ,(hex 143 58)))))
     `(font-lock-string-face             ((t (:foreground ,(hex 109 30)))))
     `(font-lock-type-face               ((t (:foreground ,(hex 217 96)))))
     `(font-lock-variable-name-face      ((t (:foreground ,(hex 173 131)))))
     `(font-lock-warning-face            ((t (:foreground ,(hex 52 174) :weight bold))))

     ;; ido-mode
     `(ido-first-match  ((t (:foreground ,(hex 220 220) :background ,(hex (+ dark-bg 1) 238)))))

     ;; mode-line
     `(mode-line            ((t (:foreground ,(hex 187 187) :background ,(hex 95 95) :height 0.95))))
     `(mode-line-buffer-id  ((t (:foreground ,(hex 230 230)))))
     `(mode-line-emphasis   ((t (:foreground ,(hex 256 256) :slant italic))))
     `(mode-line-highlight  ((t (:foreground ,(hex 228 228)))))
     `(mode-line-inactive   ((t (:foreground ,(hex (+ dark-bg 2) (- light-bg 2)) :background ,(hex 238 238)))))

     ;; show-paren
     `(show-paren-match     ((t (:foreground ,(hex 226 200) :background ,(hex (+ dark-bg 1) (- light-bg 3)) :weight bold :underline t))))
     `(show-paren-mismatch  ((t (:foreground ,(hex 226 226) :background ,(hex 196 196)) :weight bold)))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;; package-specific
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; company
     `(company-preview                       ((t (:foreground ,(hex dark-fg light-fg) :background ,(hex dark-bg light-bg)))))
     `(company-preview-common                ((t (:foreground ,(hex dark-fg light-fg) :background ,(hex dark-bg light-bg)))))
     `(company-preview-search                ((t (:foreground ,(hex dark-fg light-fg) :background ,(hex dark-bg light-bg)))))
     `(company-scrollbar-bg                  ((t (:background ,(hex (+ dark-bg 2) (- light-bg 2))))))
     `(company-scrollbar-fg                  ((t (:background ,(hex (- dark-bg 2) (- light-bg 6))))))
     `(company-tooltip                       ((t (:foreground ,(hex dark-fg light-fg) :background ,(hex (+ dark-bg 2) (- light-bg 2))))))
     `(company-tooltip-annotation            ((t (:foreground ,(hex dark-fg light-fg) :background ,(hex (+ dark-bg 2) (- light-bg 2))))))
     `(company-tooltip-annotation-selection  ((t (:foreground ,(hex dark-fg light-fg) :background ,(hex (+ dark-bg 2) (- light-bg 2))))))
     `(company-tooltip-common                ((t (:foreground ,(hex 226 32)))))
     `(company-tooltip-common-selection      ((t (:inherit company-tooltip-common))))
     `(company-tooltip-mouse                 ((t (:foreground ,(hex 226 32)))))
     `(company-tooltip-search                ((t (:foreground ,(hex dark-fg light-fg) :background ,(hex dark-bg light-bg)))))
     `(company-tooltip-search-selection      ((t (:inherit company-tooltip-search))))
     `(company-tooltip-selection             ((t (:foreground ,(hex 226 32) :background ,(hex 23 152)))))

     ;; git-gutter
     `(git-gutter:added     ((t (:foreground ,(hex 108 65) :background ,(hex (+ dark-bg 1) (- light-bg 2)) :weight bold))))
     `(git-gutter:deleted   ((t (:foreground ,(hex 161 161) :background ,(hex (+ dark-bg 1) (- light-bg 2)) :weight bold))))
     `(git-gutter:modified  ((t (:foreground ,(hex 68 68) :background ,(hex (+ dark-bg 1) (- light-bg 2)) :weight bold))))

     ;; helm
     `(helm-buffer-directory    ((t (:foreground ,(hex 66 116)))))
     `(helm-buffer-file         ((t (:foreground ,(hex 108 66)))))
     `(helm-buffer-not-saved    ((t (:foreground ,(hex 16 16)))))
     `(helm-buffer-process      ((t (:foreground ,(hex (+ dark-bg 9) (- light-bg 9))))))
     `(helm-buffer-saved-out    ((t (:foreground ,(hex 16 16)))))
     `(helm-buffer-size         ((t (:inherit helm-buffer-process))))
     `(helm-candidate-number    ((t (:background unspecified))))
     `(helm-ff-directory        ((t (:foreground ,(hex 66 116)))))
     `(helm-ff-file             ((t (:foreground ,(hex 108 66)))))
     `(helm-ff-symlink          ((t (:foreground ,(hex 73 23)))))
     `(helm-ff-invalid-symlink  ((t (:inherit helm-ff-symlink :underline (:style wave)))))
     `(helm-ff-prefix           ((t (:foreground ,(hex 66 16)))))
     `(helm-match               ((t (:inherit match))))
     `(helm-source-header       ((t (:foreground ,(hex 168 168) :weight bold))))
     `(helm-selection           ((t (:foreground unspecified :background ,(hex (+ dark-bg 3) (- light-bg 3))))))
     `(helm-selection-line      ((t (:inherit helm-selection))))

     ;; highlight-indent-guides
     `(highlight-indent-guides-odd-face  ((t (:background ,(hex (- dark-bg 1) (+ light-bg 1))))))
     `(highlight-indent-guides-even-face ((t (:background ,(hex (+ dark-bg 1) (- light-bg 1))))))

     ;; ivy
     `(ivy-current-match            ((t (:foreground unspecified :background ,(hex (+ dark-bg 3) (- light-bg 3))))))
     `(ivy-minibuffer-match-face-1  ((t (:inherit ivy-current-match))))
     `(ivy-minibuffer-match-face-2  ((t (:inherit match))))
     `(ivy-minibuffer-match-face-3  ((t (:inherit ivy-minibuffer-match-face-2))))
     `(ivy-minibuffer-match-face-4  ((t (:inherit ivy-minibuffer-match-face-2))))

     ;; linum-relative
     `(linum-relative-current-face   ((t (:foreground ,(hex 131 131) :background ,(hex (- dark-bg 1) (- light-bg 1)) :weight bold))))

     ;; smart-mode-line
     `(sml/filename ((t (:foreground ,(hex 187 230) :weight bold))))

     ;; smart-parens
     `(sp-pair-overlay-face        ((t (:background ,(hex (+ dark-bg 3) (- light-bg 3))))))
     `(sp-wrap-overlay-face        ((t (:background ,(hex (+ dark-bg 3) (- light-bg 3))))))
     `(sp-wrap-tag-overlay-face    ((t (:background ,(hex (+ dark-bg 3) (- light-bg 3))))))
     `(sp-show-pair-match-face     ((t (:foreground ,(hex 226 200) :background ,(hex (+ dark-bg 1) (- light-bg 3)) :weight bold))))
     `(sp-show-pair-mismatch-face  ((t (:foreground ,(hex 226 226) :background ,(hex 196 196) :weight bold)))))))

(defun seoul256-create (theme background)
  "Create a seoul256 theme THEME using a given BACKGROUND."
  (let ((dark-bg 237)
        (light-bg 253)
        (dark-fg 252)
        (light-fg 239)
        (style "dark"))

    (when (and (>= background 233)
               (<= background 239))
      (setq style "dark"
            dark-bg background))

    (when (and (>= background 252)
               (<= background 256))
      (setq style "light"
            light-bg background))

    (if (string= style "dark")
        (setq seoul256-current-bg dark-bg)
      (setq seoul256-current-bg light-bg))

    (seoul256-apply theme style dark-fg light-fg dark-bg light-bg)))

(defun seoul256-switch-background ()
  "Switch the background of the current seoul256 theme."
  (interactive)
  (if (= seoul256-current-bg seoul256-alternate-background)
      (seoul256-create 'seoul256 seoul256-background)
    (seoul256-create 'seoul256 seoul256-alternate-background)))

(seoul256-create 'seoul256 seoul256-background)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'seoul256)
;; Local Variables:
;; fill-column: 99
;; End:

;;; seoul256-theme.el ends here
