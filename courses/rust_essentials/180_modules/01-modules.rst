=========
Modules
=========

--------------------------
Modules: The Big Picture
--------------------------

* All application does not reside in one file!

  * *Modules* are how Rust organizes code

* Encapsulation

  * Group related code together

    * Functions, structs, traits

  * Hide implementation details from user

* Namespacing

  * Prevent "name collisions"

    * :rust:`draw()` in module :rust:`Circle` and :rust:`draw()` in :rust:`Square` do not conflict

* Unit of Organization

  * Modules are the "folders" of your logic

    * Crates are the "packages" of your project

---------------
"mod" Keyword
---------------

.. code:: rust

  mod cleaner {
      pub fn perform_cleanup() {
          println!("Cleaning up...");
      }
  }

  fn main() {
      cleaner::perform_cleanup();
  }

.. note::

  Written as :rust:`module_name::function_name`

