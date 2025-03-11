====================
Exercise: Geometry
====================

--------------------
Geometry Problem
--------------------

We will create a few utility functions for 3-dimensional geometry,
representing a point as :rust:`[f64;3]`. It is up to you to determine the
function signatures.

::

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

   // Use the following `main` to test your work.

   fn main() {
       println!("Magnitude of a unit vector: {}", magnitude(&[0.0, 1.0, 0.0]));

       let mut v = [1.0, 2.0, 9.0];
       println!("Magnitude of {v:?}: {}", magnitude(&v));
       normalize(&mut v);
       println!("Magnitude of {v:?} after normalization: {}", magnitude(&v));
   }

--------------------
Geometry Solution
--------------------

.. code:: rust

   /// Calculate the magnitude of the given vector.
   fn magnitude(vector: &[f64; 3]) -> f64 {
       let mut mag_squared = 0.0;
       for coord in vector {
           mag_squared += coord * coord;
       }
       mag_squared.sqrt()
   }

   /// Change the magnitude of the vector to 1.0 without changing its direction.
   fn normalize(vector: &mut [f64; 3]) {
       let mag = magnitude(vector);
       for item in vector {
           *item /= mag;
       }
   }

   fn main() {
       println!("Magnitude of a unit vector: {}", magnitude(&[0.0, 1.0, 0.0]));

       let mut v = [1.0, 2.0, 9.0];
       println!("Magnitude of {v:?}: {}", magnitude(&v));
       normalize(&mut v);
       println!("Magnitude of {v:?} after normalization: {}", magnitude(&v));
   }

------------------------
Additional Information
------------------------

Note that in :rust:`normalize` we wrote :rust:`*item /= mag` to modify each element.

This is because we’re iterating using a mutable reference to an array, which causes the :rust:`for` loop to give mutable references to each element.
