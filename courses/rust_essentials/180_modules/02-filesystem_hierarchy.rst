======================
Filesystem Hierarchy
======================

--------------------------------
Using Modules from Other Files
--------------------------------

* Caller specifies just the module name

  .. code:: rust

    mod cleaner;

    fn main() {
        cleaner::perform_cleanup();
    }

* Module file just contains the items

  .. code:: rust

    fn perform_cleanup() {
        println!("Whistle while you work")
    }

  *Note you don't need* :rust:`mod` *in this file*

--------------------------
Mapping Modules to Files
--------------------------

* How does Rust find :rust:`cleaner` module?

  * Module name must match filename (plus **rs** extension)

* Rust looks for

  * :filename:`cleaner.rs`

    * Preferred modern style

  * :filename:`cleaner/mod.rs`

    * Older style

-------------------------
Directory-based Modules
-------------------------

* For complex modules with their own sub-modules

  * If :filename:`cleaner.rs` contains :rust:`mod sweep;`

  * Then :rust:`sweep` module can be in :filename:`cleaner/sweep.rs`

* This creates a clean tree structure that mirrors your file system
