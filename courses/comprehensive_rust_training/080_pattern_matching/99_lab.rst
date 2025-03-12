=================================
Exercise: Expression Evaluation
=================================

----------------------------------
Expression Evaluation Background
----------------------------------

Let's write a simple recursive evaluator for arithmetic expressions.

An example of a small arithmetic expression could be :rust:`10 + 20`, which
evaluates to :rust:`30`. We can represent the expression as a tree:

.. image:: comprehensive_rust_training/pattern_matching_exercise_1.svg
   :width: 40%

A bigger and more complex expression would be
:rust:`(10 * 9) + ((3 - 4) * 5)`, which evaluate to :rust:`85`. We represent
this as a much bigger tree:

.. image:: comprehensive_rust_training/pattern_matching_exercise_2.svg

---------------------------------
Expression Evaluation Problem
---------------------------------

In code, we will represent the tree with two types:

.. container:: source_include 080_pattern_matching/src/080_pattern_matching.rs :start-after://ANCHOR-operation :end-before://ANCHOR-expression :code:rust

.. container:: source_include 080_pattern_matching/src/080_pattern_matching.rs :start-after://ANCHOR-expression :end-before://ANCHOR-eval :code:rust

And then create an evaluator:

.. code:: rust

   fn eval(e: Expression) -> i64 {
       todo!()
   }

.. container:: source_include 080_pattern_matching/src/080_pattern_matching.rs :start-after://ANCHOR-main :code:rust

.. note:: The :rust:`Box` type here is a smart pointer, and will be covered in detail later in the course. To evaluate a boxed expression, use the deref operator (:rust:`*`) to "unbox" it: :rust:`eval(*boxed_expr)`.

Copy and paste the code into the Rust playground, and begin implementing
:rust:`eval`.

---------------------------------
Expression Evaluation Solution
---------------------------------

.. container:: source_include 080_pattern_matching/src/080_pattern_matching.rs :start-after://ANCHOR-eval :end-before://ANCHOR-tests :code:rust
