========
Slices
========

--------
Slices
--------

A slice gives you a view into a larger collection:

.. code:: rust

   fn main() {
       let a: [i32; 6] = [10, 20, 30, 40, 50, 60];
       println!("a: {a:?}");

       let s: &[i32] = &a[2..4];

       println!("s: {s:?}");
   }

-  Slices borrow data from the sliced type.

---------
Details
---------

-  We create a slice by borrowing :rust:`a` and specifying the starting and
   ending indexes in brackets.

-  If the slice starts at index 0, Rust's range syntax allows us to drop
   the starting index, meaning that :rust:`&a[0..a.len()]` and
   :rust:`&a[..a.len()]` are identical.

-  The same is true for the last index, so :rust:`&a[2..a.len()]` and
   :rust:`&a[2..]` are identical.

-  To easily create a slice of the full array, we can therefore use
   :rust:`&a[..]`.

-  :rust:`s` is a reference to a slice of :rust:`i32`\ s. Notice that the type
   of :rust:`s` (:rust:`&[i32]`) no longer mentions the array length. This
   allows us to perform computation on slices of different sizes.

-  Slices always borrow from another object. In this example, :rust:`a` has
   to remain 'alive' (in scope) for at least as long as our slice.
