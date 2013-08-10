;;;; Commentary:
;;;;
;;;; This library provides the 'columnify' command, which arranges the
;;;; lines in a region into columns.  It chooses a column width based
;;;; on the length of the longest line, and tries to fit as many
;;;; columns as possible into the current line width (as established
;;;; by `fill-column'), with some reasonable amount of space between
;;;; columns.

