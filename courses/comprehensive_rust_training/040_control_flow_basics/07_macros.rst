========
Macros
========

--------
Macros
--------

Macros are expanded into Rust code during compilation, and can take a
variable number of arguments. They are distinguished by a :rust:`!` at the
end. The Rust standard library includes an assortment of useful macros.

-  :rust:`println!(format, ..)` prints a line to standard output, applying
   formatting described in
   `std::fmt <https://doc.rust-lang.org/std/fmt/index.html>`__.
-  :rust:`format!(format, ..)` works just like :rust:`println!` but returns the
   result as a string.
-  :rust:`dbg!(expression)` logs the value of the expression and returns it.
-  :rust:`todo!()` marks a bit of code as not-yet-implemented. If executed,
   it will panic.
-  :rust:`unreachable!()` marks a bit of code as unreachable. If executed,
   it will panic.

.. code:: rust

   fn factorial(n: u32) -> u32 {
       let mut product = 1;
       for i in 1..=n {
           product *= dbg!(i);
       }
       product
   }

   fn fizzbuzz(n: u32) -> u32 {
       todo!()
   }

   fn main() {
       let n = 4;
       println!("{n}! = {}", factorial(n));
   }

---------
Details
---------

The takeaway from this section is that these common conveniences exist,
and how to use them. Why they are defined as macros, and what they
expand to, is not especially critical.

The course does not cover defining macros, but a later section will
describe use of derive macros.
