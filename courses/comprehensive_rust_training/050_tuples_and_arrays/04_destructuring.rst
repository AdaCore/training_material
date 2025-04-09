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

----------
Patterns
----------

- Patterns used here are :dfn:`irrefutable` 

  - Compiler can statically verify value on right of :rust:`=` has same structure as pattern.

- Variable name is an irrefutable pattern that always matches any value

  - That is why we can also use :rust:`let` to declare a single variable.

- Rust also supports using patterns in conditionals

  - Allows equality comparison and destructuring to happen at the same time.
