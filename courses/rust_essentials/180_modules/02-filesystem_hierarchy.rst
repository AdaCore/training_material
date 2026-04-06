======================
Filesystem Hierarchy
======================

--------------------------------
Using Modules From Other Files
--------------------------------

* Each file is considered a module

  * Functions, structs, etc. are *potentially* visible to other files

    * More on how that's controlled in the next chapter

  * Do not need :rust:`mod` in the file

    * If you have it, that's a nested module!

  **File cleaner.rs**

    .. code:: rust

      pub fn perform_cleanup() {
          println!("Whistle while you work")
      }

* Calling file specifies module name

  **File caller.rs**

  .. code:: rust

    mod cleaner;

    fn main() {
        cleaner::perform_cleanup();
    }

--------------------------
Mapping Modules to Files
--------------------------

* How does Rust find :rust:`cleaner` module?

  * Module name must match filename (plus **rs** extension)

* Rust looks for

  * :filename:`cleaner.rs`
  * :filename:`cleaner/mod.rs`

.. note::

  :filename:`cleaner/mod.rs` is legacy style but still common

-------------------------
Directory-Based Modules
-------------------------

* For complex modules with their own sub-modules

  * If :filename:`cleaner.rs` contains :rust:`mod sweep;`

    * ... then :rust:`sweep` module can be in :filename:`cleaner/sweep.rs`

* This creates a clean tree structure that mirrors your file system
