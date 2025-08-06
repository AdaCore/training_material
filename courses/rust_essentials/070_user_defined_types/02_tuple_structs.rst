===============
Tuple Structs
===============

---------------
Tuple Structs
---------------

If the field names are unimportant, you can use a tuple struct:

.. code:: rust

   struct Point(i32, i32);

   fn main() {
       let p = Point(17, 23);
       println!("({}, {})", p.0, p.1);
   }

This is often used for single-field wrappers (called newtypes):

.. code:: rust

   struct PoundsOfForce(f64);
   struct Newtons(f64);

   fn compute_thruster_force() -> PoundsOfForce {
       todo!("Ask a rocket scientist at NASA")
   }

   fn set_thruster_force(force: Newtons) {
       // ...
   }

   fn main() {
       let force = compute_thruster_force();
       set_thruster_force(force);
   }

---------
Details
---------

-  Newtypes are a great way to encode additional information about the
   value in a primitive type, for example:

   -  The number is measured in some units: :rust:`Newtons` in the example
      above.
   -  The value passed some validation when it was created, so you no
      longer have to validate it again at every use:
      :rust:`PhoneNumber(String)` or :rust:`OddNumber(u32)`.

-  Demonstrate how to add a :rust:`f64` value to a :rust:`Newtons` type by
   accessing the single field in the newtype.

   -  Rust generally doesn't like inexplicit things, like automatic
      unwrapping or for instance using booleans as integers.
   -  Operator overloading is discussed on Day 3 (generics).

-  The example is a subtle reference to the
   :url:`Mars Climate Orbiter <https://en.wikipedia.org/wiki/Mars_Climate_Orbiter>`
   failure.
