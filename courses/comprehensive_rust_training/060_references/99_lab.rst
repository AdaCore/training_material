====================
Exercise: Geometry
====================

--------------------
Geometry Problem
--------------------

We will create a few utility functions for 3-dimensional geometry,
representing a point as :rust:`[f64;3]`. It is up to you to determine the
function signatures.

.. code:: rust

   // Calculate the magnitude of a vector by summing the squares of its coordinates
   // and taking the square root. Use the `sqrt()` method to calculate the square
   // root, like `v.sqrt()`.

   fn magnitude(...) -> f64 {
       todo!()
   }

   // Normalize a vector by calculating its magnitude and dividing all of its
   // coordinates by that magnitude.

   fn normalize(...) {
       todo!()
   }

Use the following :rust:`main` to test your work.

.. container:: source_include 060_references/src/060_references.rs :start-after://ANCHOR-main :code:rust

--------------------
Geometry Solution
--------------------

.. container:: source_include 060_references/src/060_references.rs :start-after://ANCHOR-solution :end-before://ANCHOR-main :code:rust

------------------------
Additional Information
------------------------

Note that in :rust:`normalize` we wrote :rust:`*item /= mag` to modify each element.

This is because we’re iterating using a mutable reference to an array, which causes the :rust:`for` loop to give mutable references to each element.
