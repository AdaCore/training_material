=========================
Packages and Modularity
=========================

----------------
Modularity (1)
----------------

* Rust's compilation model is different from C/C++
* Also very different from Ada
* Rust's compilation unit is the crate
* A crate can span several files, and is usually much bigger than an Ada or C compilation unit (C++ is different because of templates)

Consequence is that parallel compilation is hampered in Rust.

* Rust compiler is incremental on a sub-file level

----------------
Modularity (2)
----------------

* Two types of crates: Binary crates and library crates

    - Entry point for binary crates: main.rs
    - Entry point for library crates: lib.rs
    - Both can be redefined

* Generally, a library = a crate (but a Cargo package can contain one or more crates)

* A crate can be subdivided in modules

---------
Modules
---------

A crate can be further subdivided into modules

* Modules provide scoping, organization, and encapsulation
* A module can be defined:

    - Inline
    - In a file corresponding to the module name

* By default, a module is private
* By default, items in a module are private

.. code:: Rust

   // Inline module
   pub mod ExprEval {
       pub struct Expr {
       }

       ...
   }

---------
Modules
---------

.. code:: Rust

   // Module in a separate file

   // main.rs

   pub mod ExprEval

   // expreval.rs

   pub struct Expr {
   }

---------
Modules
---------

.. code:: Rust

   // Module in a separate file, in a nested dir

   // main.rs

   pub mod ExprEval

   // expreval.rs

   pub mod Eval;

   pub struct Expr {
   }

   // expreval/eval.rs

   pub fn eval(...)

