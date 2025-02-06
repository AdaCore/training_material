========
Unions
========

--------
Unions
--------

Unions are like enums, but you need to track the active field yourself:

.. code:: rust

   #[repr(C)]
   union MyUnion {
       i: u8,
       b: bool,
   }

   fn main() {
       let u = MyUnion { i: 42 };
       println!("int: {}", unsafe { u.i });
       println!("bool: {}", unsafe { u.b }); // Undefined behavior!
   }

.. raw:: html

---------
Details
---------

Unions are very rarely needed in Rust as you can usually use an enum.
They are occasionally needed for interacting with C library APIs.

If you just want to reinterpret bytes as a different type, you probably
want
`std::mem::transmute <https://doc.rust-lang.org/stable/std/mem/fn.transmute.html>`__
or a safe wrapper such as the
`zerocopy <https://crates.io/crates/zerocopy>`__ crate.

.. raw:: html

