;;; ox-hugo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ox-blackfriday" "ox-blackfriday.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ox-blackfriday.el

(autoload 'org-blackfriday-export-as-markdown "ox-blackfriday" "\
Export current buffer to a Github Flavored Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org BLACKFRIDAY Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-blackfriday-convert-region-to-md "ox-blackfriday" "\
Convert text in the current region to Blackfriday Markdown.
The text is assumed to be in Org mode format.

This can be used in any buffer.  For example, you can write an
itemized list in Org mode syntax in a Markdown buffer and use
this command to convert it.

\(fn)" t nil)

(autoload 'org-blackfriday-export-to-markdown "ox-blackfriday" "\
Export current buffer to a Github Flavored Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-blackfriday-publish-to-blackfriday "ox-blackfriday" "\
Publish an Org file to Blackfriday Markdown file.

PLIST is the property list for the given project.  FILENAME is
the filename of the Org file to be published.  PUB-DIR is the
publishing directory.

Return output file name.

\(fn PLIST FILENAME PUB-DIR)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-blackfriday" '("org-blackfriday-")))

;;;***

;;;### (autoloads nil "ox-hugo" "ox-hugo.el" (0 0 0 0))
;;; Generated autoloads from ox-hugo.el
 (put 'org-hugo-section 'safe-local-variable 'stringp)
 (put 'org-hugo-front-matter-format 'safe-local-variable 'stringp)
 (put 'org-hugo-footer 'safe-local-variable 'stringp)
 (put 'org-hugo-preserve-filling 'safe-local-variable 'booleanp)
 (put 'org-hugo-delete-trailing-ws 'safe-local-variable 'booleanp)
 (put 'org-hugo-use-code-for-kbd 'safe-local-variable 'booleanp)
 (put 'org-hugo-allow-spaces-in-tags 'safe-local-variable 'booleanp)
 (put 'org-hugo-prefer-hyphen-in-tags 'safe-local-variable 'booleanp)
 (put 'org-hugo-auto-set-lastmod 'safe-local-variable 'booleanp)
 (put 'org-hugo-suppress-lastmod-period 'safe-local-variable 'floatp)
 (put 'org-hugo-export-with-toc 'safe-local-variable (lambda (x) (or (booleanp x) (integerp x))))
 (put 'org-hugo-export-with-section-numbers 'safe-local-variable (lambda (x) (or (booleanp x) (equal 'onlytoc x) (integerp x))))
 (put 'org-hugo-default-static-subdirectory-for-externals 'safe-local-variable 'stringp)
 (put 'org-hugo-export-creator-string 'safe-local-variable 'stringp)
 (put 'org-hugo-date-format 'safe-local-variable 'stringp)
 (put 'org-hugo-paired-shortcodes 'safe-local-variable 'stringp)
 (put 'org-hugo-link-desc-insert-type 'safe-local-variable 'booleanp)

(autoload 'org-hugo-slug "ox-hugo" "\
Convert string STR to a `slug' and return that string.

A `slug' is the part of a URL which identifies a particular page
on a website in an easy to read form.

Example: If STR is \"My First Post\", it will be converted to a
slug \"my-first-post\", which can become part of an easy to read
URL like \"https://example.com/posts/my-first-post/\".

In general, STR is a string.  But it can also be a string with
Markdown markup as that string passed to this function is often
the sub-headings of a post (which can contain bold, italics,
link, etc markup).

The `slug' generated from that STR follows these rules:

- Contain only lower case alphabet, number and hyphen characters
  ([[:alnum:]-]).
- Not have *any* HTML tag like \"<code>..</code>\",
  \"<span class=..>..</span>\", etc.
- Not contain any URLs (if STR happens to be a Markdown link).
- Replace \".\" in STR with \"dot\", and \"&\" with \"and\".
- Replace parentheses with double-hyphens.  So \"foo (bar) baz\"
  becomes \"foo--bar--baz\".
- Replace non [[:alnum:]-] chars with spaces, and then one or
  more consecutive spaces with a single hyphen.
- At most two consecutive hyphens are allowed.
- No hyphens allowed at the leading or trailing end of the slug.

\(fn STR)" nil nil)

(autoload 'org-hugo-export-as-md "ox-hugo" "\
Export current buffer to a Hugo-compatible Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org Hugo Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil.

Return the buffer the export happened to.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-hugo-export-to-md "ox-hugo" "\
Export current buffer to a Hugo-compatible Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-hugo-export-wim-to-md "ox-hugo" "\
Export the current subtree/all subtrees/current file to a Hugo post.

This is an Export \"What I Mean\" function:

- If the current subtree has the \"EXPORT_FILE_NAME\" property, export
  that subtree.
- If the current subtree doesn't have that property, but one of its
  parent subtrees has, then export from that subtree's scope.
- If none of the subtrees have that property (or if there are no Org
  subtrees at all), but the Org #+title keyword is present,
  export the whole Org file as a post with that title (calls
  `org-hugo-export-to-md' with its SUBTREEP argument set to nil).

- If ALL-SUBTREES is non-nil, export all valid Hugo post subtrees
  (that have the \"EXPORT_FILE_NAME\" property) in the current file
  to multiple Markdown posts.
- If ALL-SUBTREES is non-nil, and again if none of the subtrees have
  that property (or if there are no Org subtrees), but the Org #+title
  keyword is present, export the whole Org file.

- If the file neither has valid Hugo post subtrees, nor has the
  #+title present, throw a user error.  If NOERROR is non-nil, use
  `message' to display the error message instead of signaling a user
  error.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

If ALL-SUBTREES is nil, return output file's name.
If ALL-SUBTREES is non-nil, and valid subtrees are found, return
a list of output files.
If ALL-SUBTREES is non-nil, and valid subtrees are not found,
return the output file's name (exported using file-based
approach).

\(fn &optional ALL-SUBTREES ASYNC VISIBLE-ONLY NOERROR)" t nil)

(autoload 'org-hugo-debug-info "ox-hugo" "\
Get Emacs, Org and Hugo version and ox-hugo customization info.
The information is converted to Markdown format and copied to the
kill ring.  The same information is displayed in the Messages
buffer and returned as a string in Org format.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-hugo" '("org-hugo-")))

;;;***

;;;### (autoloads nil "ox-hugo-auto-export" "ox-hugo-auto-export.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ox-hugo-auto-export.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-hugo-auto-export" '("org-hugo-")))

;;;***

;;;### (autoloads nil "ox-hugo-pandoc-cite" "ox-hugo-pandoc-cite.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ox-hugo-pandoc-cite.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-hugo-pandoc-cite" '("org-hugo-pandoc-cite-")))

;;;***

;;;### (autoloads nil nil ("ox-hugo-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ox-hugo-autoloads.el ends here
