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


**Wouldn't it be nice to shorten these paths?**

.. code:: rust

  mod greenhouse;
  fn main() {
      // This is repetitive and hard to read
      greenhouse::shelf::cactus::water_cactus();
      greenhouse::shelf::cactus::touch_spine();
      greenhouse::shelf::cactus::water_cactus();
      greenhouse::shelf::cactus::touch_spine();
  }

--------------------
The "use" Shortcut
--------------------

* :rust:`use` helps avoid typing long paths

  .. code:: rust

    mod greenhouse;
    use greenhouse::shelf::cactus;
    fn main() {
        cactus::water_cactus();
        cactus::touch_spine();
        cactus::water_cactus();
        cactus::touch_spine();
    }

* Code just needs to use :rust:`cactus`

  * ... as if it was in scope

-----------------------
"use" With a Wildcard
-----------------------

* Use wildcard "*" (:dfn:`glob import`) to get everything

  * All public items in module get added to current scope

  .. code:: rust

    mod math_utils {
        pub fn add(a: i32, b: i32) -> i32 { a + b }
        pub fn subtract(a: i32, b: i32) -> i32 { a - b }
        pub fn multiply(a: i32, b: i32) -> i32 { a * b }
        pub fn divide(a: i32, b: i32) -> i32 { a / b }
    }

    use math_utils::*
    fn main() {
        let sum = add(10, 5);
    }

* Benefits

  * Used in preludes to load essential traits
  * Speeds up prototyping

* Risks

  * Makes it hard to find things for programmer and autocomplete
  * Globbed modules with the same name cause compilation errors

.. note:: 

  To reduce risks of *globbing*, use **nested imports**

  .. code:: rust

    use std::io::(self, Read, Write);

-----------------
Name Collisions
-----------------

.. code:: rust

  mod two_dee_graphics {
      pub fn render() { todo!() }
  }

  mod three_dee_graphics {
      pub fn render() { todo!() }
  }

  // COLLISION!
  use two_dee_graphics::render;
  use three_dee_graphics::render;

  fn main() {
      render(); // Error - which one did you mean?
  }

:error:`error[E0252]: the name 'render' is defined multiple times`

**Solution: Rename with** :rust:`as`

  .. code:: rust

    use two_dee_graphics::render as render2d;
    use three_dee_graphics::render as render3d;
    fn main() {
        render3d();
    }

----------------
Relative Paths
----------------

* Use :rust:`self` when referring to current module

  .. code:: rust

    mod shelf {
        pub mod cactus {
            pub struct Pot;
            pub fn water() {}
        }
    }

    use shelf::cactus::{self, Pot};

    fn main() {
        let my_pot = Pot;  // Imported directly via 'Pot'
        cactus::water();   // Imported via 'self' (cactus module)
    }

* Use :rust:`super` to refer to the enclosing module 

  * Useful for reaching "outside" the current module

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

* Use :rust:`crate` to refer to something from base directory of filesystem

  * Always starts from the root of the current crate
  * Path stays valid even if you move the code to a different module

* Example

  .. code:: rust

    use crate::network::server::start;

  * Works from anywhere in the project
