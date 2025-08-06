=========
Structs
=========

---------
Structs
---------

Like tuples, Struct can also be destructured by matching:

.. container:: latex_environment scriptsize

   .. code:: rust

      struct Foo {
          x: (u32, u32),
          y: u32,
      }

      #[rustfmt::skip]
      fn main() {
          let foo = Foo { x: (1, 2), y: 3 };
          match foo {
              Foo { x: (1, b), y } => println!("x.0 = 1, b = {b}, y = {y}"),
              Foo { y: 2, x: i }   => println!("y = 2, x = {i:?}"),
              Foo { y, .. }        => println!("y = {y}, other fields were ignored"),
          }
      }

---------
Details
---------

-  Change the literal values in :rust:`foo` to match with the other
   patterns.
-  Add a new field to :rust:`Foo` and make changes to the pattern as needed.
-  The distinction between a capture and a constant expression can be
   hard to spot. Try changing the :rust:`2` in the second arm to a variable,
   and see that it subtly doesn't work. Change it to a :rust:`const` and see
   it working again.

-----------------
More to Explore
-----------------

* Try :rust:`match &foo` and check the type of captures. The pattern syntax remains the same, but the captures become shared references. This is :dfn:`match ergonomics` and is often useful with match self when implementing methods on an enum.

  * The same effect occurs with :rust:`match &mut foo` - the captures become exclusive references.

* The distinction between a capture and a constant expression can be hard to spot. Try changing the **2** in the second arm to a variable, and see that it subtly doesn’t work. Change it to a const and see it working again.

