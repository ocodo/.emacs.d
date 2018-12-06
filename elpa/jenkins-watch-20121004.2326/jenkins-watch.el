;;; jenkins-watch.el --- Watch continuous integration build status -*- indent-tabs-mode: t; tab-width: 8 -*-

;; Copyright (C) 2010, 2011, 2012 Andrew Taylor

;; Author: Andrew Taylor <ataylor@redtoad.ca>
;; URL: https://github.com/ataylor284/jenkins-watch
;; Package-Version: 20121004.2326
;; Version: 1.2

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; Displays an icon representing the current build status of
;; jenkins/hudson on the modeline.

;;; Code:

(require 'url)

(defgroup jenkins-watch nil
  "Jenkins watch."
  :prefix "jenkins-watch")

(defcustom jenkins-login-url nil
  "Jenkins login URL."
  :type 'string
  :group 'jenkins-watch)

(defcustom jenkins-api-url "http://SERVER/job/JOB/api/xml"
  "The jenkins job api URL.  Override this replacing SERVER and JOB with appropriate values."
  :type 'string
  :group 'jenkins-watch)

(defcustom jenkins-watch-timer-interval 90
  "The interval to poll jenkins."
  :type 'number
  :group 'jenkins-watch)

(defvar jenkins-watch-timer nil
  "Timer object for jenkins polling will be stored here.")

(defconst jenkins-watch-jenkins-status-name-alist
  '(("blue" . success)
    ("yellow" . unstable)
    ("red" . failure)
    ("grey" . error)))

(defun jenkins-watch-start ()
  "Start watching jenkins."
  (interactive)
  (unless jenkins-watch-timer
    (setq jenkins-watch-timer
	  (run-at-time "0 sec"
		       jenkins-watch-timer-interval
		       #'jenkins-watch-timer-action))
    (jenkins-watch-status-indicator-add-to-mode-line)))

(defun jenkins-watch-stop ()
  "Stop watching jenkins."
  (interactive)
  (when jenkins-watch-timer
    (cancel-timer jenkins-watch-timer)
    (setq jenkins-watch-timer nil)
    (jenkins-watch-status-indicator-remove-from-mode-line)))

(defun jenkins-auth (callback)
  (let ((url-request-method "POST")
	(url-request-data "j_username="))
    (url-retrieve jenkins-login-url callback)))

(defun jenkins-fetch-data (&rest arg)
  (let ((url-request-method nil)
	(url-request-data nil))
    (url-retrieve jenkins-api-url #'jenkins-watch-update-status nil t)))

(defun jenkins-watch-timer-action ()
  (condition-case exception
      (if jenkins-login-url
	  (jenkins-auth #'jenkins-fetch-data)
	(jenkins-fetch-data))
    (error
     (jenkins-watch-log-error exception)
     (setq jenkins-watch-mode-line "X-("))))

(defun jenkins-watch-update-status (status)
  (goto-char (point-min))
  (search-forward "\n\n")
  (let ((status (jenkins-watch-extract-last-status)))
    (cond ((eq status 'success)
	   (setq jenkins-watch-mode-line (concat " " jenkins-watch-mode-line-success)))
	  ((eq status 'unstable)
	   (setq jenkins-watch-mode-line (concat " " jenkins-watch-mode-line-unstable)))
 	  ((eq status 'failure)
	   (setq jenkins-watch-mode-line (concat " " jenkins-watch-mode-line-failure)))
	  ((eq status 'error)
	   (setq jenkins-watch-mode-line "X-("))))
  (kill-buffer))

(defun jenkins-watch-extract-last-status ()
  (condition-case exception
      (let* ((xml (xml-parse-region (point) (point-max)))
	     (project (car xml))
	     (color (car (xml-get-children project 'color))))
	(cdr (assoc (nth 2 color) jenkins-watch-jenkins-status-name-alist)))
    (error
     (jenkins-watch-log-error exception)
     "ERROR")))

(defconst jenkins-watch-success-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
	    "/* XPM */
static char * blue_xpm[] = {
\"16 16 48 1\",
\" 	c None\",
\".	c #2F65A7\",
\"+	c #3E69A6\",
\"@	c #4E6D99\",
\"#	c #456FAC\",
\"$	c #3F74B0\",
\"%	c #577097\",
\"&	c #54769C\",
\"*	c #4B77AE\",
\"=	c #5A78A5\",
\"-	c #517CB4\",
\";	c #617EA5\",
\">	c #607EAB\",
\",	c #5681B9\",
\"'	c #5D81B4\",
\")	c #6083B7\",
\"!	c #5987B8\",
\"~	c #6785B3\",
\"{	c #6087B4\",
\"]	c #558AC1\",
\"^	c #598DC4\",
\"/	c #688AB8\",
\"(	c #6490C3\",
\"_	c #6A91BE\",
\":	c #7798C0\",
\"<	c #6E9ACD\",
\"[	c #739AC7\",
\"}	c #7AA0CE\",
\"|	c #74A4D0\",
\"1	c #87A2C6\",
\"2	c #83ACD3\",
\"3	c #89AECF\",
\"4	c #96AECC\",
\"5	c #8EB2D4\",
\"6	c #94B4D0\",
\"7	c #99B9D6\",
\"8	c #9FBBD2\",
\"9	c #A5C1D8\",
\"0	c #ACC4D5\",
\"a	c #B6C9D6\",
\"b	c #B1CDD8\",
\"c	c #BACEDA\",
\"d	c #C3D3D9\",
\"e	c #C9D6D7\",
\"f	c #D5DFDA\",
\"g	c #DDE4D9\",
\"h	c #E6E9D8\",
\"i	c #F6F7E0\",
\"                \",
\"                \",
\"     18972:     \",
\"    80cc93<[    \",
\"   40egfc7|]_   \",
\"  _5afihd8|^$/  \",
\"  :29egfc7|]#~  \",
\"  :}60ba82<,+)  \",
\"  _(|3652<]$.)  \",
\"  /-(<<<(]$..{  \",
\"   !$-,,-#..-   \",
\"   >*..+...$~   \",
\"    @)+..+'=    \",
\"      %;;&      \",
\"                \",
\"                \"};
")) "Image for successful build.")

(defconst jenkins-watch-mode-line-success
  (if jenkins-watch-success-image
      (propertize ":)"
		  'display jenkins-watch-success-image
		  'help-echo "Build succeeded")
    ":)"))

(defconst jenkins-watch-unstable-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
           :ascent center
           :data
           "/* XPM */
static char *yellow[] = {
/* columns rows colors chars-per-pixel */
\"16 16 52 1 \",
\"     c None\",
\"N    c #7B6819\",
\".    c #8D7824\",
\"X    c #C3B446\",
\"o    c #CDB14B\",
\"O    c #BCA144\",
\"+    c #BA9F4D\",
\"@    c #CFB14B\",
\"#    c #FFB839\",
\"$    c #CEAF5B\",
\"%    c #BDA351\",
\"&    c #C7AB51\",
\"*    c #CDAE5B\",
\"=    c #DCBC62\",
\"-    c #DCBF64\",
\";    c #DFBE63\",
\":    c #DCCB6E\",
\">    c #DACF7A\",
\",    c #E0C565\",
\"<    c #E8BE5D\",
\"1    c #FEBC47\",
\"2    c #EFC05E\",
\"3    c #F8C15B\",
\"4    c #DFCC71\",
\"5    c #E3BF60\",
\"6    c #FCC547\",
\"7    c #FACA54\",
\"8    c #F9D659\",
\"9    c #E3CE6C\",
\"0    c #E5DB88\",
\"q    c #F2C96C\",
\"w    c #F6C360\",
\"e    c #F8DE60\",
\"r    c #F2D372\",
\"t    c #F6E569\",
\"y    c #F7EA79\",
\"u    c #D1B553\",
\"i    c #D4C567\",
\"p    c #EBE191\",
\"a    c #EDE393\",
\"s    c #EFE382\",
\"d    c #F8EC87\",
\"f    c #F9EE95\",
\"g    c #FAF09D\",
\"h    c #FBF2A6\",
\"j    c #FCF5B9\",
\"k    c #FDF8C5\",
\"l    c #FEFBD7\",
\"c    c #A68F0F\",
\"n    c #BDAF3D\",
\"M    c #C2B54A\",
\"C    c #AB9516\",
\"                \",
\"                \",
\"     >pghd:     \",
\"   Cphhghdtt    \",
\"   shjkkhdt8y   \",
\"  odfkkkhfte6%  \",
\"  -dfhkkhdt86<  \",
\"  rydfggfye86y  \",
\"  -8tyddyt86#7  \",
\"  %68ette86##u  \",
\"   w668766##t   \",
\"   u7######6*   \",
\"    o86166w%    \",
\"     NO&&&      \",
\"                \",
\"                \"};
")) "Image for unstable build.")

(defconst jenkins-watch-mode-line-unstable
  (if jenkins-watch-unstable-image
      (propertize ":/"
                 'display jenkins-watch-unstable-image
                 'help-echo "Build unstable")
    ":/"))

(defconst jenkins-watch-failure-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
	    "/* XPM */
static char * red_xpm[] = {
\"16 16 48 1\",
\" 	c None\",
\".	c #EF2724\",
\"+	c #F0292C\",
\"@	c #A33F3E\",
\"#	c #F03534\",
\"$	c #A8484C\",
\"%	c #B0484A\",
\"&	c #F2393C\",
\"*	c #B95352\",
\"=	c #F34444\",
\"-	c #D45252\",
\";	c #EB4E4F\",
\">	c #F34D4D\",
\",	c #DE5354\",
\"'	c #C55A5A\",
\")	c #CC5957\",
\"!	c #CC5A5D\",
\"~	c #E75455\",
\"{	c #BE6261\",
\"]	c #F35656\",
\"^	c #F7595B\",
\"/	c #E46165\",
\"(	c #D06868\",
\"_	c #CB6D6C\",
\":	c #F76261\",
\"<	c #E86B6A\",
\"[	c #F6696A\",
\"}	c #C77876\",
\"|	c #F66F6C\",
\"1	c #F87274\",
\"2	c #F87978\",
\"3	c #F97B7F\",
\"4	c #D78684\",
\"5	c #F08483\",
\"6	c #FA8383\",
\"7	c #F98989\",
\"8	c #E5908F\",
\"9	c #FA9090\",
\"0	c #F99694\",
\"a	c #FC9A9D\",
\"b	c #FA9F9F\",
\"c	c #FAA5A3\",
\"d	c #FDAAAD\",
\"e	c #FBAFAF\",
\"f	c #FFB9B7\",
\"g	c #FDC0C1\",
\"h	c #FEC7C7\",
\"i	c #FDD7D7\",
\"                \",
\"                \",
\"     }8b95_     \",
\"    8bcb93:<    \",
\"   40egfc7|>/   \",
\"  {7afihd91]&!  \",
\"  (20egfb7|>#-  \",
\"  ([60bb92:=+~  \",
\"  !]|3662[>&.,  \",
\"  '=]:[:^>&..)  \",
\"   ~&====#..>   \",
\"   %=.++...#'   \",
\"    @,#..+;%    \",
\"      @**$      \",
\"                \",
\"                \"};
")) "Image for failed build.")

(defconst jenkins-watch-mode-line-failure
  (if jenkins-watch-success-image
      (propertize ":("
		  'display jenkins-watch-failure-image
		  'help-echo "Build failed")
    ":("))

(defvar jenkins-watch-mode-line ":|"
  "What gets displayed on the mode line.")
(put 'jenkins-watch-mode-line 'risky-local-variable t)

(defun jenkins-watch-status-indicator-add-to-mode-line ()
  ""
  (if (boundp 'mode-line-modes)
      (add-to-list 'mode-line-modes '(t jenkins-watch-mode-line) t)))

(defun jenkins-watch-status-indicator-remove-from-mode-line ()
  ""
  (if (boundp 'mode-line-modes)
      (delete '(t jenkins-watch-mode-line) mode-line-modes)))

(defun jenkins-watch-log-error (exception)
  ""
  (message "%s" (concat "jenkins-watch error: "
			(eval (cons 'format (cdr exception))))))

(provide 'jenkins-watch)

;;; jenkins-watch.el ends here
