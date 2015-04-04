This package implements a 1D selection model.  It provides methods
to add or remove intervals and merge or split the overlapping
portions automatically.  The intervals are modeled as a list of
cons (beg . end) ordered in ascending order.  An example interval
list is:

((1 . 4) (7 . 12) (15 . 17))

Each interval represents all the integers inside it, including the
boundaries.  Therefore, a list like ((1 . 2) (3 . 4)) is invalid
and should be represented as ((1 . 4)) instead.  To model a single
point selection, you can use interval of zero length, for example
(5 . 5).  Two helper functions are provided to add or remove points
instead of intervals.

This data structure is not meant to be used for querying, it is only
useful to represent the selection as a whole.  A helper macro is
provided to iterate over the selected indices.  Lookup is of course
possible, but inefficient.  For data structure designed for
interval and point lookup see package `interval-tree' instead.

See github readme at https://github.com/Fuco1/interval-list
