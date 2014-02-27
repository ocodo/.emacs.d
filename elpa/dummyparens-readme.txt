Dummyparens is a simple utility providing parentheses auto-pairing and
wrapping.

When you press an opening parenthesis key it inserts the closing one
for you. If a region is selected when you press the key, the region
will be wrapped. If you wrap with { it will also indent the wrapped
region (convenient for C and the like).

This is similar to electric-pair minor mode, but with wrapping.

It's intended to be minimalistic and non-invasive as you would expect
such simple functionality to be.

For more sophisticated features, like HTML tags or LaTeX environments
handling, consider https://github.com/Fuco1/smartparens

