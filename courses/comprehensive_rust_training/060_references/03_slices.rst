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

-------------------
More About Slices
-------------------

- Create a slice by borrowing :rust:`a` and specifying starting and ending indices in brackets.

- Indexing shortcuts

  - If first index is 0, range syntax allows us to drop the starting index

    - :rust:`&a[0..a.len()]` and :rust:`&a[..a.len()]` are identical.

  - Same is true for the last index

    - :rust:`&a[2..a.len()]` and :rust:`&a[2..]` are identical.

  - So a slice of the full array can be specified as :rust:`&a[..]`.

- :rust:`s` is a reference to a slice of :rust:`i32`

  - Type of :rust:`s` does not mention array length

    - Allows us to perform computation on slices of different sizes.

- Slices always borrow from another object

  - In the example, :rust:`a` has to remain 'alive' (in scope) for at least as long as our slice.
