===============
Documentation
===============

---------------
Documentation
---------------

Rust comes with extensive documentation. For example:

-  All of the details about
   :url:`loops <https://doc.rust-lang.org/stable/reference/expressions/loop-expr.html>`.
-  Primitive types like
   :url:`u8 <https://doc.rust-lang.org/stable/std/primitive.u8.html>`.
-  Standard library types like
   :url:`Option <https://doc.rust-lang.org/stable/std/option/enum.Option.html>`
   or
   :url:`BinaryHeap <https://doc.rust-lang.org/stable/std/collections/struct.BinaryHeap.html>`.

Use :filename:`rustup doc --std` or https://std.rs to view the documentation.

In fact, you can document your own code:

.. code:: rust

   /// Determine whether the first argument is divisible by the second argument.
   ///
   /// If the second argument is zero, the result is false.
   fn is_divisible_by(lhs: u32, rhs: u32) -> bool {
       if rhs == 0 {
           return false;
       }
       lhs % rhs == 0
   }

The contents are treated as Markdown. All published Rust library crates
are automatically documented at :url:`docs.rs <https://docs.rs>` using
the :url:`rustdoc <https://doc.rust-lang.org/rustdoc/what-is-rustdoc.html>`
tool. It is idiomatic to document all public items in an API using this
pattern.

To document an item from inside the item (such as inside a module), use
:rust:`//!` or :rust:`/*! .. */`, called "inner doc comments":

.. code:: rust

   //! This module contains functionality relating to divisibility of integers.

.. raw:: html

---------
Details
---------

-  Show students the generated docs for the :rust:`rand` crate at
   https://docs.rs/rand.

.. raw:: html

