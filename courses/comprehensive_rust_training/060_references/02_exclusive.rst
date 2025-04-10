======================
Exclusive References
======================

----------------------
Exclusive References
----------------------

Exclusive references, also known as mutable references, allow changing
the value they refer to. They have type :rust:`&mut T`.

.. code:: rust

   fn main() {
       let mut point = (1, 2);
       let x_coord = &mut point.0;
       *x_coord = 20;
       println!("point: {point:?}");
   }

---------------------------------
More About Exclusive References
---------------------------------

- :dfn:`Exclusive` means that only this reference can be used to access the value

  - No other references (shared or exclusive) can exist at the same time
  - Referenced value cannot be accessed while the exclusive reference exists

- Note difference between these two statements

  .. code:: Rust
     :number-lines: 1

     let mut x_coord: &i32
     let y_coord: &mut i32

  - Line 1 represents a shared reference which can be bound to different values
  - Line 2 represents an exclusive reference to a mutable value.
