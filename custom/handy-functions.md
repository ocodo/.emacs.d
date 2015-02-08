# Handy Functions

These functions are variously home-grown or pilfered from other
sources, they are all modified from their original form and may have
forked from thier origins up 5 or 6 years ago.

They all vary in usefulness.  Go ahead and browse below, you'll find
the source in `.emacs.d/custom/handy-functions.md`

All functions listed below are `(interactive)`.

### Toggle mode line on off

`(toggle-mode-line-on-off)`

Toggle the modeline off and on.  Uses `saved-mode-line` as a register.

Note this only affects the current buffer, and it doesn't seem to work
wth key bindings.

### Join line from below

`(join-line-from-below)`

Umm, I think this one joins the line from below, to the current line.

### Align number right

`(align-number-right)`

Align region to equal signs from BEGIN to END.

### Insert random in range

`(insert-random-in-range)`

Insert a random number within the range of START and END.

### Insert random radian

`(insert-random-radian)`

Insert a radian value from 0 to 6.28318 (ie. 2PI : 360 degrees)

### Fraction radian

`(fraction-radian)`

Fraction DENOMINATOR of circle to radians.

### Now is

`(now-is)`

Insert current time.

### Utc seconds

`(utc-seconds)`

Insert UTC seconds.

### Untabify buffer

`(untabify-buffer)`

Untabify the current buffer.

### Indent buffer

`(indent-buffer)`

Indent the current buffer.

### Cleanup buffer

`(cleanup-buffer)`

Perform cleanup operations on a buffer, tabs to spaces, re-indent, trim trailing whitespace.

### Toggle window split

`(toggle-window-split)`

Toggle the current window split.

### Open line below

`(open-line-below)`

Open a newline below the current point.

### Open line above

`(open-line-above)`

Open a newline above the current point.

### Duplicate current line or region

`(duplicate-current-line-or-region)`

Duplicates the current line or region ARG times.

### Shell command on region replace

`(shell-command-on-region-replace)`

Run `shell-command-on-region` replacing the selected region.  START END COMMAND.

### Directory of library

`(directory-of-library)`

Open directory with dired which contain the given LIBRARYNAME.

### Join line or lines in region

`(join-line-or-lines-in-region)`

Join this line or the lines in the selected region.

### Rename this buffer and file

`(rename-this-buffer-and-file)`

Renames current buffer and file it is visiting.

### Delete this buffer and file

`(delete-this-buffer-and-file)`

Delete the file connected to this buffer and kill it, FORCE is universal argument.

### Switch to scratch

`(switch-to-scratch)`

Switch to scratch, grabbing and pasting the region into scratch if selected.

### Eval and replace prin1

`(eval-and-replace-prin1)`

Replace the preceding sexp with its value using prin1.

### Eval and replace

`(eval-and-replace)`

Replace the preceding sexp with its value.

### Magit just amend

`(magit-just-amend)`

Just git commit --amend.

### Shell command on buffer file

`(shell-command-on-buffer-file)`

Run a shell command, using the file of current buffer as input.
Return an error if no buffer file.

## Text transformers

The following functions will convert between `dashed-text`,
`snake_case` (or `underscored_text`), `lowerCamel` and `UpperCamel`
(or `StudlyCaps`) or just regular old spaced words.

### Snake case at point or region

`(snake-case-at-point-or-region)`

Transform the current text at point or in selection to `snake_case`.

### Dasherise at point or region

`(dasherise-at-point-or-region)`

Transform the current text at point or in selection to `dashed-words`.

### Upper camelcase at point or region

`(upper-camelcase-at-point-or-region)`

Transform the current text at point or in selection to `UpperCamelCase`.

### Lower camelcase at point or region

`(lower-camelcase-at-point-or-region)`

Transform the current text at point or in selection to `lowerCamelCase`.

### Humanize at point or region

`(humanize-at-point-or-region)`

Humanize text selection.

The first word will be Capitalized, the rest lower case.

### Titleized at point or region

`(titleized-at-point-or-region)`

Titleize text selection, words will be spaced and Capitalized.

### Yank repeat

`(yank-repeat)`

Repeat yank `n` times.

### Smart beginning of line

`(smart-beginning-of-line)`

Move point to first non-whitespace character or `beginning-of-line`.

### Pretty print xml region

`(pretty-print-xml-region)`

Pretty format XML markup in region.

### Toggle fullscreen

`(toggle-fullscreen)`

Toggle full screen. DEPRECATED

### Copy region to osx clipboard

`(copy-region-to-osx-clipboard)`

Copy contents of the current region to the OS X Clipboard.

### Copy buffer to osx clipboard

`(copy-buffer-to-osx-clipboard)`

Copy contents of the current buffer to the OS X Clipboard.

### Ruby toggle symbol at point

`(ruby-toggle-symbol-at-point)`

Dirt simple, just prefix/unprefix current word with/without a colon.

### Ruby make interpolated string at point or region

`(ruby-make-interpolated-string-at-point-or-region)`

Simple conversion of string/region to ruby interpolated string.

### Pcre regexp from list of words

`(pcre-regexp-from-list-of-words)`

Insert a PCRE regexp to match a list of WORDS.

### Kill whole word

`(kill-whole-word)`

Kill the current word at point. (Note it will kill the word left of the cursor.)

### Sass hex color to var

`(sass-hex-color-to-var)`

Search for the next hex color, replace it with a new variable
name. (based on the current css selector.)

Place the new variable at the top of the file set to the hex color we
just found.

### Markdown codefence region

`(markdown-codefence-region)`

Enclose the region in a Github Flavored Markdown code-fence, (ie. enclose in three <code>```</code> backticks.)

### Reload current chrome tab osx

`(reload-current-chrome-tab-osx)`

Run a simple AppleScript to reload the current Google Chrome tab.

Use of AppleScript makes this OSX specific of course.

### Open opsmanager

`(open-opsmanager)`

Open ~/workspace/OpsManager in a dired buffer.

### Increase default font height

`(increase-default-font-height)`

Adjust the default font `:height` by 10, universal argument is `M` (to
set by multiples, negative values will reduce `:height`).

### Set default font height

`(set-default-font-height)`

Set the default font `:height` to the value of `P` (prefix arg) or
when `P` is unset, enter via minibuffer.
