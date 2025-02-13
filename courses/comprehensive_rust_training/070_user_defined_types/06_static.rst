============
"static"
============

------------
"static"
------------

Static variables will live during the whole execution of the program,
and therefore will not move:

.. code:: rust

   static BANNER: &str = "Welcome to RustOS 3.14";

   fn main() {
       println!("{BANNER}");
   }

As noted in the
:url:`Rust RFC Book <https://rust-lang.github.io/rfcs/0246-const-vs-static.html>`,
these are not inlined upon use and have an actual associated memory
location. This is useful for unsafe and embedded code, and the variable
lives through the entirety of the program execution. When a
globally-scoped value does not have a reason to need object identity,
:rust:`const` is generally preferred.

.. raw:: html

---------
Details
---------

-  :rust:`static` is similar to mutable global variables in C++.
-  :rust:`static` provides object identity: an address in memory and state
   as required by types with interior mutability such as :rust:`Mutex<T>`.

=================
More to Explore
=================

-----------------
More to Explore
-----------------

Because :rust:`static` variables are accessible from any thread, they must
be :rust:`Sync`. Interior mutability is possible through a
:url:`Mutex <https://doc.rust-lang.org/std/sync/struct.Mutex.html>`,
atomic or similar.

It is common to use :rust:`OnceLock` in a static as a way to support
initialization on first use. :rust:`OnceCell` is not :rust:`Sync` and thus
cannot be used in this context.

Thread-local data can be created with the macro :rust:`std::thread_local`.

.. raw:: html

