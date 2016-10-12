;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'gnu-apl-util)

(defvar gnu-apl--symbol-doc
  '(("+"
     (("Monadic" "Conjugate" "Returns the conjugate of R"
       "If R is a real number, return R. If R is complex, return
R with the imaginary part negated.")
      ("Dyadic" "Addition" "Returns the sum of L and R")))

    ;; ========================================

    ("−"
     (("Monadic" "Negation" "Negate R")
      ("Dyadic" "Subtraction" "Subtract R from L")))

    ;; ========================================

    ("×"
     (("Monadic" "Signum" "¯1 if R<0; 0 if R=0; 1 if R>0")
      ("Dyadic" "Multiply" "L multiplied by R")))

    ;; ========================================

    ("÷"
     (("Monadic" "Reciprocal" "1 divided by R")
      ("Dyadic" "Division (mathematics)" "L divided by R")))

    ;; ========================================

    ("⋆"
     (("Monadic" "Exponential" "e to the R power")
      ("Dyadic" "Exponentiation]]" "L raised to the R power")))

    ;; ========================================

    ("○"
     (("Monadic" "Pi times" "Multiply by π")
      ("Dyadic" "Circle" "Trigonometric functions of R selected by L"
       "The operation is chosen by the value of L from the
following list of available operations:

  0   (1-R⋆2)⋆0.5
 ¯1   arcsin R               1   sin R
 ¯2   arccos R               2   cosin R
 ¯3   arctan R               3   tan R
 ¯4   (R+1)×((R-1)÷R+1)⋆0.5  4   (1+R⋆2)⋆0.5
 ¯5   arcsinh R              5   sinh R
 ¯6   arccosh R              6   cosh R
 ¯7   arctanh R              7   tanh R
 ¯8   -(¯1-R×2)⋆0.5          8   (¯1-R⋆2)⋆0.5
 ¯9   R                      9   Real part of R
¯10   +R                    10   |R
¯11   0J1×R                 11   Imaginary part of R
¯12   ⋆0J1×R                12   Arc R")))

    ;; ========================================

    ("?"
     (("Monadic" "Roll" "One integer selected randomly from the first R integers")
      ("Dyadic" "Deal" "L distinct integers selected randomly from the first R integers")))

    ;; ========================================

    ("∊"
     (("Monadic" "Enlist" "Create a vector containing all scalars in R")
      ("Dyadic" "Membership" "1 for elements of L present in R; 0 where not.")))

    ;; ========================================

    ("⍷"
     (("Find" "Find subsequence in array" "1 for position each position that contains the array L in R")))

    ;; ========================================

    ("⌈"
     (("Monadic" "Ceiling" "Least integer greater than or equal to R")
      ("Dyadic" "Sample maximum and minimum" "The greater value of L or R")))

    ;; ========================================

    ("⌊"
     (("Monadic" "Floor" "Greatest integer less than or equal to R")
      ("Dyadic" "Sample maximum and minimum" "The smaller value of L or R")))

    ;; ========================================

    ("⍴"
     (("Monadic" "Shape" "Number of components in each dimension of R")
      ("Dyadic" "Reshape" "Array of shape L with data R")))

    ;; ========================================

    ("↑"
     (("Monadic" "Take" "Select the first element of R")
      ("Dyadic" "Take" "Select the first (or last) L elements of R according to ×L")))

    ;; ========================================

    ("↓"
     (("Dyadic" "Drop " "Remove the first (or last) L elements of R according to ×L")))

    ;; ========================================

    ("⊥"
     (("Dyadic" "Decode" "Value of a polynomial whose coefficients are R at L")))

    ;; ========================================

    ("⊤"
     (("Dyadic" "Encode" "Base-L representation of the value of R")))

    ;; ========================================

    ("∣"
     (("Monadic" "Absolute value" "Magnitude of R")
      ("Dyadic" "Modulo" "R modulo L")))

    ;; ========================================

    (","
     (("Monadic" "Ravel" "Reshapes R into a vector")
      ("Dyadic" "Catenation" "Elements of R appended to the elements of L")))

    ;; ========================================

    ("\\"
     (("Dyadic" "Expansion" "Insert zeros (or blanks) in R corresponding to zeros in L")))

    ;; ========================================

    ("/"
     (("Dyadic" "Compress" "Select elements in R corresponding to ones in L")))

    ;; ========================================

    ("⍳"
     (("Monadic" "Index generator" "Vector of the first R integers")
      ("Dyadic" "Index of" "The location (index) of R in L; 1+⌈/⍳⍴L if not found")))

    ;; ========================================

    ("⌹"
     (("Monadic" "Matrix inverse" "Inverse of matrix R")
      ("Dyadic" "Matrix divide" "Solution to system of linear equations Lx = R")))

    ;; ========================================

    ("⌽"
     (("Monadic" "Reversal" "Reverse elements of R along last axis")
      ("Dyadic" "Rotation" "The elements of R are rotated L positions")))

    ;; ========================================

    ("⊖"
     (("Monadic" "Reversal" "Reverse elements of R along first axis")
      ("Dyadic" "Rotation" "The elements of R are rotated L positions along the first axis")))

    ;; ========================================

    ("⍟"
     (("Monadic" "Logarithm" "Natural logarithm of R")
      ("Dyadic" "Logarithm" "Logarithm of R to base L")))

    ;; ========================================

    ("⍕"
     (("Monadic" "Format" "A character representation of R")
      ("Dyadic" "Format" "Format R into a character matrix according to L")))

    ;; ========================================

    ("⍉"
     (("Monadic" "Transpose" "Reverse the axes of R")
      ("Dyadic" "Transpose" "The axes of R are ordered by L")))

    ;; ========================================

    ("!"
     (("Monadic" "Factorial" "Product of integers 1 to R")
      ("Dyadic" "Combinations" "Number of combinations of R taken L at a time")))

    ;; ========================================

    ("<"
     (("Dyadic" "Less than" "Comparison: 1 if true, 0 if false")))

    ;; ========================================

    ("≤"
     (("Dyadic" "Less than or equal" "Comparison: 1 if true, 0 if false")))

    ;; ========================================

    ("="
     (("Dyadic" "Equality" "Comparison: 1 if true, 0 if false")))

    ;; ========================================

    ("≥"
     (("Dyadic" "Greater than or equal" "Comparison: 1 if true, 0 if false")))

    ;; ========================================

    (">"
     (("Dyadic" "Greater than" "Comparison: 1 if true, 0 if false")))

    ;; ========================================

    ("≠"
     (("Dyadic" "Not equal" "Comparison: 1 if true, 0 if false")))

    ;; ========================================

    ("∨"
     (("Dyadic" "Logical disjunction" "Logic: 0 if L and R are 0; 1 otherwise")))

    ;; ========================================

    ("∧"
     (("Dyadic" "Logical conjunction" "Logic: 1 if L and R are 1; 0 otherwise")))

    ;; ========================================

    ("⍱"
     (("Dyadic" "Logical Nor" "Logic: 1 if both L and R are 0; otherwise 0")))

    ;; ========================================

    ("⍲"
     (("Dyadic" "Logical Nand" "Logic: 0 if both L and R are 1; otherwise 1")))

    ;; ========================================

    (("∼" "~")
     (("Monadic" "Not" "Negates the binary values in R"
       "Given a set of binary values in R, return a new array of the
same dimension where each value has been negated. If any values
are not of the value 0 or 1, a DOMAIN ERROR will be raised.")
      ("Dyadic" "Without" "Returns L with the values from R removed.")))

    ;; ========================================

    ("⍋"
     (("Monadic" "Grade up" "Indices of R which will arrange R in ascending order")
      ("Dyadic" "Grade up with collation sequence" "Indices of R which will arrange R in ascending order based on collating sequence L")))

    ;; ========================================

    ("⍒"
     (("Monadic" "Grade down" "Indices of R which will arrange R in descending order")
      ("Dyadic" "Grade down with collation sequence" "Indices of R which will arrange R in descending order based on collating sequence L")))

    ;; ========================================

    ("⍎"
     (("Monadic" "Execute" "Execute an APL expression")))

    ;; ========================================

    ("←"
     (("Dyadic" "Assignment" "Assign the value of R to L")))

    ;; ========================================

    ("→"
     (("Monadic" "Goto" "Go to line R")))

    ;; ========================================

    ("∇"
     (("Monadic" "Function definition" "Define or modify a function")))

    ;; ========================================

    ("⊂"
     (("Monadic" "Enclose" "Produce a scalar from R")
      ("Dyadic" "Partition" "Divide R into vectors based on L")))

    ;; ========================================

    ("⊃"
     (("Monadic" "Disclose" "Produce an array from R")
      ("Dyadic" "Pick" "Select a value from R based on L")))

    ;; ========================================

    ("∪"
     (("Monadic" "Unique" "Return an array of all unique elements in R")))

    ;; ========================================

    ("⍷"
     (("Dyadic" "Find" "Return a boolean array indicating the positions of the array L in R")))

    ;; ========================================

    ("≡"
     (("Monadic" "Depth" "Return the levels of nesting in R")
      ("Dyadic" "Match" "Returns true if L has the same structure as well as data as R")))

    ;; ========================================

    ("≢"
     (("Monadic" "Returns the number of elements in the first dimension of R.")
      ("Dyadic" "Not match" "Returns true if L has different structure or data as R.")))

    ;; ========================================

    ("⊥"
     (("Dyadic" "Decode" "Yields the values of array L evaluated in a number system with radices R")))

    ;; ========================================

    ("⊤"
     (("Dyadic" "Encode" "Yields the representation of L in the number system whose radices are R")))

    ;; ========================================

    ("⊢"
     (("Monadic" "Identity" "Return R.")
      ("Dyadic" "Right" "Return R.")))

    ;; ========================================

    ("⊣"
     (("Monadic" "Identity computed" "Returns R, marked as being computed.")
      ("Dyadic" "Left" "Return L."))))

  "Documentation for APL symbols. Each element is a list of six
elements: The APL symbol, name of monadic operator, description
of the monadic operator, name of the dyadic operator, description
of the dyadic operator, extra documentation.")

(provide 'gnu-apl-refdocs-bsd-license)
