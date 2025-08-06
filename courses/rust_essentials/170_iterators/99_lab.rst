====================================
Exercise: Iterator Method Chaining
====================================

------------------------------------
Iterator Method Chaining Problem
------------------------------------

In this exercise, you will need to find and use some of the provided
methods in the
:url:`Iterator <https://doc.rust-lang.org/std/iter/trait.Iterator.html>`
trait to implement a complex calculation.

Copy the following code to https://play.rust-lang.org/ and make the
tests pass. Use an iterator expression and :rust:`collect` the result to
construct the return value.

.. code:: rust

   /// Calculate the differences between elements of 'values' offset by 'offset',
   /// wrapping around from the end of 'values' to the beginning.
   ///
   /// Element 'n' of the result is 'values[(n+offset)%len] - values[n]'.
   fn offset_differences(offset: usize, values: Vec<i32>) -> Vec<i32> {
       todo!()
   }

.. container:: source_include 170_iterators/src/170_iterators.rs :start-after://ANCHOR-unit_tests :code:rust

------------------------------------
Iterator Method Chaining Solution
------------------------------------

.. raw:: latex

   \begin{scriptsize}

.. container:: source_include 170_iterators/src/170_iterators.rs :start-after://ANCHOR-solution :end-before://ANCHOR-unit_tests :code:rust

.. raw:: latex

   \end{scriptsize}

