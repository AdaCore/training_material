==========================
Mutable Static Variables
==========================

--------------------------
Mutable Static Variables
--------------------------

It is safe to read an immutable static variable:

.. code:: rust

   static HELLO_WORLD: &str = "Hello, world!";

   fn main() {
       println!("HELLO_WORLD: {HELLO_WORLD}");
   }

However, since data races can occur, it is unsafe to read and write
mutable static variables:

.. code:: rust

   static mut COUNTER: u32 = 0;

   fn add_to_counter(inc: u32) {
       // SAFETY: There are no other threads which could be accessing `COUNTER`.
       unsafe {
           COUNTER += inc;
       }
   }

   fn main() {
       add_to_counter(42);

       // SAFETY: There are no other threads which could be accessing `COUNTER`.
       unsafe {
           println!("COUNTER: {COUNTER}");
       }
   }

------------------------------
Dealing With Race Conditions
------------------------------

-  The program here is safe because it is single-threaded. However, the
   Rust compiler is conservative and will assume the worst.

.. container:: latex_environment footnotesize

   ::

     error: creating a shared reference to mutable static is discouraged


-  If you remove the :rust:`unsafe` compiler complains:

.. container:: latex_environment footnotesize

   ::

      error[E0133]: use of mutable static is unsafe and requires unsafe block

-  Using a mutable static is generally a bad idea, but there are some
   cases where it might make sense in low-level :rust:`no_std` code, such as
   implementing a heap allocator or working with some C APIs.
