========================
"use", "super", "self"
========================

--------------------
The "use" Shortcut
--------------------

* :rust:`use` helps avoid typing long paths

  * E.g., :rust:`std::collections::HashMap`

* :rust:`use std::collections::HashMap;` allows you to just type :rust:`HashMap`

  * As if it was in your own module

* Use the wildcard "*" to get everything

  * :rust:`use std::io::*;` gets all items from :rust:`std::io`
  * But it makes name clashes more likely

* Simplify (and reduce clashes) with *renaming*

  .. code:: rust

    use std::io::Result as IoResult;

----------------
Relative Paths
----------------

* When referring to the current module, you can use :rust:`self`

  * Often used in use statements

* To refer to the enclosing module, you can use :rust:`super`

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

* To refer to something from the base directory of the filesystem, use :rust:`crate`

  * Always starts from the root of the current crate
  * Path stays valid even if you move the file to a different module

* Example

  .. code:: rust

    use crate::network::server::start;

  * Works from anywhere in the project
