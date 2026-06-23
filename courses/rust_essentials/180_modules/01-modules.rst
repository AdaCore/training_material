=========
Modules
=========

-------------
Big Picture
-------------

* Most applications reside in more than one file

  * *Modules* are how Rust organizes code

* Encapsulation

  * Groups related code together

    * Functions, structs, traits

  * Hides implementation details from programmer

* Namespacing

  * Prevents "name collisions"

* Unit of organization

  * Modules are the "folders" of your logic

------------------
Complete Picture
------------------

**Rust code is made up of**

.. list-table::
  :header-rows: 1

  * - **Element**
    - **Description**
    -

  * - *Item*
    - Function, struct, enum
    - (Smallest unit)

  * - *Module*
    - Folder for items
    - (Privacy boundary)

  * - *Crate*
    - Collection of modules
    - (Binary or library)

  * - *Package*
    - One or more crates
    - (Managed by :filename:`Cargo.toml`)

---------------
"mod" Keyword
---------------

* :rust:`mod` - foundation of module system

  * Container for functions, structs, traits, modules
  * Like a "namespace" - helps prevent naming conflicts

.. code:: rust

  mod cleaner {
      pub fn perform_cleanup() {
          println!("Cleaning up...");
      }
  }

  fn main() {
      cleaner::perform_cleanup();
  }

.. code:: output

  Cleaning up...

.. note::

  Written as :rust:`module_name::function_name`
