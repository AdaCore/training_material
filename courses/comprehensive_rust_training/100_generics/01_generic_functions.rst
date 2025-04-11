===================
Generic Functions
===================

-------------------
Generic Functions
-------------------

Rust supports generics, which lets you abstract algorithms or data
structures (such as sorting or a binary tree) over the types used or
stored.

.. code:: rust

   /// Pick `even` or `odd` depending on the value of `n`.
   fn pick<T>(n: i32, even: T, odd: T) -> T {
       if n % 2 == 0 {
           even
       } else {
           odd
       }
   }

   fn main() {
       println!("picked a number: {:?}", pick(97, 222, 333));
       println!("picked a string: {:?}", pick(28, "dog", "cat"));
   }

-------------------------
Using Generic Functions
-------------------------

-  Rust infers a type for T based on the types of the arguments and
   return value.

-  In this example we only use the primitive types :rust:`i32` and :rust:`&str`
   for :rust:`T`, but we can use any type here, including user-defined
   types:

   .. code:: rust

      struct Foo {
          val: u8,
      }

      pick(123, Foo { val: 7 }, Foo { val: 456 });

-  Similar to C++ templates, but Rust does a partial compilation

   - Function must be valid for all types matching the constraints

     - Example: modify :rust:`pick` to return :rust:`even + odd` if :rust:`n == 0`
     - Compiler complains because there is no guarantee type used supports :rust:`+`
     - A C++ template would not generate a compile error

-  Generic code is turned into non-generic code based on the call sites.
   This is a zero-cost abstraction: you get exactly the same result as
   if you had hand-coded the data structures without the abstraction.
