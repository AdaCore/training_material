=================================
Exercise: Rewriting with Result
=================================

---------------------------------
Rewriting with Result Problem
---------------------------------

In this exercise we're revisiting the expression evaluator exercise that
we did earlier. Our initial solution ignores a possible error case:
Dividing by zero! Rewrite :rust:`eval` to instead use idiomatic error
handling to handle this error case and return an error when it occurs.
We provide a simple :rust:`DivideByZeroError` type to use as the error type
for :rust:`eval`.

.. container:: source_include 200_error_handling/src/200_error_handling.rs :start-after://ANCHOR-types :end-before://ANCHOR-types_end :code:rust

.. container:: source_include 200_error_handling/src/200_error_handling.rs :start-after://ANCHOR-eval :end-before://ANCHOR-eval_end :code:rust

---------------------------------
Rewriting with Result Tests
---------------------------------

.. container:: source_include 200_error_handling/src/200_error_handling.rs :start-after://ANCHOR-tests :code:rust

---------------------------------
Hint
---------------------------------

The starting code here isn't exactly the same as the previous
exercise's solution: We've added in an explicit panic to show
where the error case is.

---------------------------------
Rewriting with Result Solution
---------------------------------

.. container:: source_include 200_error_handling/src/200_error_handling.rs :start-after://ANCHOR-solution :end-before://ANCHOR-tests :code:rust
