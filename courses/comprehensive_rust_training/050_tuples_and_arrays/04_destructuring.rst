============================
Patterns and Destructuring
============================

----------------------------
Patterns and Destructuring
----------------------------

When working with tuples and other structured values it's common to want
to extract the inner values into local variables. This can be done
manually by directly accessing the inner values:

.. code:: rust

   fn print_tuple(tuple: (i32, i32)) {
       let left = tuple.0;
       let right = tuple.1;
       println!("left: {left}, right: {right}");
   }

However, Rust also supports using pattern matching to destructure a
larger value into its constituent parts:

.. code:: rust

   fn print_tuple(tuple: (i32, i32)) {
       let (left, right) = tuple;
       println!("left: {left}, right: {right}");
   }

---------
Details
---------

-  The patterns used here are :dfn:`irrefutable`, meaning that the compiler
   can statically verify that the value on the right of :rust:`=` has the
   same structure as the pattern.
-  A variable name is an irrefutable pattern that always matches any
   value, hence why we can also use :rust:`let` to declare a single
   variable.
-  Rust also supports using patterns in conditionals, allowing for
   equality comparison and destructuring to happen at the same time.
   This form of pattern matching will be discussed in more detail later.
-  Edit the examples above to show the compiler error when the pattern
   doesn't match the value being matched on.
