=========
Modules
=========

----------------------------
Organization "Big Picture"
----------------------------

* Encapsulation

  * Modules allow you to group related code together

    * Functions, structs, traits

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

  Use the :rust:`::` to specify the module containing the item
