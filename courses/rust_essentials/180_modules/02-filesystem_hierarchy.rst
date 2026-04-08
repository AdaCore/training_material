======================
Filesystem Hierarchy
======================

--------------------------------
Using Modules From Other Files
--------------------------------

* Each **file** is considered a **module**

  * Functions, structs, etc. are *potentially* visible to other files
  * File by definition is a module

    * Do not put :rust:`mod` at the top

  :filename:`supplier.rs`

    .. code:: rust

      pub fn perform_cleanup() {
          println!("Whistle while you work");
      }

* Calling file specifies module name

  :filename:`client.rs`

  .. code:: rust

    mod supplier;

    fn main() {
        supplier::perform_cleanup();
    }

--------------------------
Mapping Modules to Files
--------------------------

* How does Rust find :rust:`cleaner` module?

  * Module name must match filename

* Compiler looks for

  * :filename:`cleaner.rs`
  * :filename:`cleaner/mod.rs`

    * Legacy style but still common

.. note::

  Filenames consist of module name and :filename:`.rs` extension

-------------------------
Directory-Based Modules
-------------------------

* For complex modules with their own sub-modules

  * If :filename:`cleaner.rs` contains :rust:`mod sweep;`

    * ... then :rust:`sweep` module can be in :filename:`cleaner/sweep.rs`

* This creates a clean tree structure that mirrors your file system

.. image:: rust_essentials/modules_hierarchy.svg
