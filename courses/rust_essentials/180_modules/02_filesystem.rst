======================
Filesystem Hierarchy
======================

----------------------
Filesystem Hierarchy
----------------------

Omitting the module content will tell Rust to look for it in another
file:

.. code:: rust

   mod garden;

This tells Rust that the :rust:`garden` module content is found at
:rust:`src/garden.rs`. Similarly, a :rust:`garden::vegetables` module can be
found at :rust:`src/garden/vegetables.rs`.

The :rust:`crate` root is in:

-  :rust:`src/lib.rs` (for a library crate)
-  :rust:`src/main.rs` (for a binary crate)

Modules defined in files can be documented, too, using :dfn:`inner doc comments`.
These document the item that contains them - in this case, a
module.

.. code:: rust

   //! This module implements the garden, including a highly performant germination
   //! implementation.

   // Re-export types from this module.
   pub use garden::Garden;
   pub use seeds::SeedPacket;

   /// Sow the given seed packets.
   pub fn sow(seeds: Vec<SeedPacket>) {
       todo!()
   }

   /// Harvest the produce in the garden that is ready.
   pub fn harvest(garden: &mut Garden) {
       todo!()
   }

---------
Details
---------

-  Before Rust 2018, modules needed to be located at :rust:`module/mod.rs`
   instead of :rust:`module.rs`, and this is still a working alternative for
   editions after 2018.

-  The main reason to introduce :rust:`filename.rs` as alternative to
   :rust:`filename/mod.rs` was because many files named :rust:`mod.rs` can be
   hard to distinguish in IDEs.

-  Deeper nesting can use folders, even if the main module is a file:

   .. code:: ignore

      src/
      |-- main.rs
      |-- top_module.rs
      |-- top_module/
          |-- sub_module.rs

-  The place rust will look for modules can be changed with a compiler
   directive:

   .. code:: rust

      #[path = "some/path.rs"]
      mod some_module;

   This is useful, for example, if you would like to place tests for a
   module in a file named :rust:`some_module_test.rs`, similar to the
   convention in Go.
