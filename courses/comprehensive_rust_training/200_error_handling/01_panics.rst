========
Panics
========

--------
Panics
--------

Rust handles fatal errors with a "panic".

Rust will trigger a panic if a fatal error happens at runtime:

.. code:: rust

   fn main() {
       let v = vec![10, 20, 30];
       println!("v[100]: {}", v[100]);
   }

-  Panics are for unrecoverable and unexpected errors.

   -  Panics are symptoms of bugs in the program.
   -  Runtime failures like failed bounds checks can panic
   -  Assertions (such as :rust:`assert!`) panic on failure
   -  Purpose-specific panics can use the :rust:`panic!` macro.

-  A panic will "unwind" the stack, dropping values just as if the
   functions had returned.
-  Use non-panicking APIs (such as :rust:`Vec::get`) if crashing is not
   acceptable.

---------
Details
---------

By default, a panic will cause the stack to unwind. The unwinding can be
caught:

.. code:: rust

   use std::panic;

   fn main() {
       let result = panic::catch_unwind(|| "No problem here!");
       println!("{result:?}");

       let result = panic::catch_unwind(|| {
           panic!("oh no!");
       });
       println!("{result:?}");
   }

-  Catching is unusual; do not attempt to implement exceptions with
   :rust:`catch_unwind`!
-  This can be useful in servers which should keep running even if a
   single request crashes.
-  This does not work if :rust:`panic = 'abort'` is set in your
   :rust:`Cargo.toml`.
