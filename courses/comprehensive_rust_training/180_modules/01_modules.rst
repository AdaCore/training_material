=========
Modules
=========

---------
Modules
---------

We have seen how :rust:`impl` blocks let us namespace functions to a type.

Similarly, :rust:`mod` lets us namespace types and functions:

.. code:: rust

   mod foo {
       pub fn do_something() {
           println!("In the foo module");
       }
   }

   mod bar {
       pub fn do_something() {
           println!("In the bar module");
       }
   }

   fn main() {
       foo::do_something();
       bar::do_something();
   }

.. raw:: html

---------
Details
---------

-  Packages provide functionality and include a :filename:`Cargo.toml` file that
   describes how to build a bundle of 1+ crates.
-  Crates are a tree of modules, where a binary crate creates an
   executable and a library crate compiles to a library.
-  Modules define organization, scope, and are the focus of this
   section.

.. raw:: html

