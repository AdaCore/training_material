========================
"use", "super", "self"
========================

-------------------------
Dealing With Long Paths
-------------------------

.. code:: rust

  mod greenhouse {
      pub mod shelf {
          pub mod cactus {
              pub fn water_cactus() {
                  println!("Watering the cactus");
              }
              pub fn touch_spine() {
                  println!("Touching the prickly thing");
              }
          }
      }
  }

  fn main() {
      // This is repetitive and hard to read
      greenhouse::shelf::cactus::water_cactus();
      greenhouse::shelf::cactus::touch_spine();
      greenhouse::shelf::cactus::water_cactus();
      greenhouse::shelf::cactus::touch_spine();
  }

.. note::

  Wouldn't it be nice to shorten these paths?

--------------------
The "use" Shortcut
--------------------

* :rust:`use` helps avoid typing long paths

  * E.g., :rust:`std::collections::HashMap`

* :rust:`use std::collections::HashMap;`

  * Allows you to just type :rust:`HashMap`
  * ... as if it was in your own module

.. tip::

  Rather than :rust:`use`, try :rust:`as` to rename item

  .. code:: rust

    use std::io::Result as IoResult;

-----------------------
"use" With a Wildcard
-----------------------

* Use the wildcard "*" (:dfn:`glob import`) to get everything

  * All public items in module get added to current scope

* Benefits

  * Common in :rust:`mod tests` to test private items easily
  * Used in preludes to load essential traits
  * Speeds up prototyping

* Risks

  * Makes it hard to find things for coder and autocomplete
  * Globbed modules with the same name cause compilation errors

.. note:: 

  To reduce risks of *globbing*, use **nested imports**

  .. code:: rust

    use std::io::(self, Read, Write);

----------------
Relative Paths
----------------

* When referring to the current module, you can use :rust:`self`

  * Often used in :rust:`use` statements

* Use :rust:`super` to refer to the enclosing module 

  * Useful for reaching "outside" the current module

    * Especially in unit tests

  .. code:: rust

    mod parent {
        pub fn hello() {}
        mod child {
            fn call_parent() {
                super::hello(); // Reaches up to 'parent'
            }
        }
    }

----------------
Absolute Paths
----------------

* Use :rust:`crate` to refer to something from the base directory of the filesystem

  * Always starts from the root of the current crate
  * Path stays valid even if you move the file to a different module

* Example

  .. code:: rust

    use crate::network::server::start;

  * Works from anywhere in the project
