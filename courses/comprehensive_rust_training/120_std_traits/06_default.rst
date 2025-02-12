=======================
The ``Default`` Trait
=======================

-----------------------
The ``Default`` Trait
-----------------------

:url:`Default <https://doc.rust-lang.org/std/default/trait.Default.html>`
trait produces a default value for a type.

.. code:: rust,editable

   #[derive(Debug, Default)]
   struct Derived {
       x: u32,
       y: String,
       z: Implemented,
   }

   #[derive(Debug)]
   struct Implemented(String);

   impl Default for Implemented {
       fn default() -> Self {
           Self("John Smith".into())
       }
   }

   fn main() {
       let default_struct = Derived::default();
       println!("{default_struct:#?}");

       let almost_default_struct =
           Derived { y: "Y is set!".into(), ..Derived::default() };
       println!("{almost_default_struct:#?}");

       let nothing: Option<Derived> = None;
       println!("{:#?}", nothing.unwrap_or_default());
   }

.. raw:: html

---------
Details
---------

-  It can be implemented directly or it can be derived via
   ``#[derive(Default)]``.
-  A derived implementation will produce a value where all fields are
   set to their default values.

   -  This means all types in the struct must implement ``Default`` too.

-  Standard Rust types often implement ``Default`` with reasonable
   values (e.g. ``0``, ``""``, etc).
-  The partial struct initialization works nicely with default.
-  The Rust standard library is aware that types can implement
   ``Default`` and provides convenience methods that use it.
-  The ``..`` syntax is called
  :url:`struct update syntax <https://doc.rust-lang.org/book/ch05-01-defining-structs.html#creating-instances-from-other-instances-with-struct-update-syntax>`.

.. raw:: html

