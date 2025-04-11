==========================
Review of Program Memory
==========================

--------------------------
Review of Program Memory
--------------------------

Programs allocate memory in two ways:

-  Stack: Continuous area of memory for local variables.

   -  Values have fixed sizes known at compile time.
   -  Extremely fast: just move a stack pointer.
   -  Easy to manage: follows function calls.
   -  Great memory locality.

-  Heap: Storage of values outside of function calls.

   -  Values have dynamic sizes determined at runtime.
   -  Slightly slower than the stack: some book-keeping needed.
   -  No guarantee of memory locality.

---------
Example
---------

Creating a :rust:`String` puts fixed-sized metadata on the stack and
dynamically sized data, the actual string, on the heap:

.. code:: rust

   fn main() {
       let s1 = String::from("Hello");
   }

.. image:: comprehensive_rust_training/review_of_program_memory.svg

.. note::

   :rust:`String` is backed by a :rust:`Vec`, so it has a
   capacity and length and can grow if mutable via reallocation on the
   heap.

-----------------
More to Explore
-----------------

We can inspect the memory layout with :rust:`unsafe` Rust. However, you
should point out that this is rightfully unsafe!

.. container:: latex_environment scriptsize

   .. code:: rust

      fn main() {
          let mut s1 = String::from("Hello");
          s1.push(' ');
          s1.push_str("world");
          // DON'T DO THIS AT HOME! For educational purposes only.
          // String provides no guarantees about its layout, so this could lead to
          // undefined behavior.
          unsafe {
             let (capacity, ptr, len): (usize, usize, usize) = std::mem::transmute(s1);
             println!("capacity = {capacity}, ptr = {ptr:#x}, len = {len}");
          }
      }
