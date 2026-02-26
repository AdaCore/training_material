===============
Encapsulation
===============

------------------
Why Encapsulate?
------------------

* Protect internal state of a data structure

  * Ensure data is always valid via getter/setter API's

* Decoupling - hiding implementation details

  * Change your code without breaking client

    * E.g., swapping an array for a :rust:`Vec`

--------------------------
Encapsulation in Structs
--------------------------

* *Gatekeeper* pattern

  * Keep fields private

    * Only your code can modify fields

  * Provide :rust:`pub` getter and setter methods

    * Control how data is read or modified

.. container:: latex_environment tiny

  .. code:: rust

    pub struct Students {
        names: Vec<String>, // Private!
    }

    impl Students {
        pub fn new() -> Self {
            Self { names: Vec::new() }
        }
        pub fn add(&mut self, name: String) {
            if !name.is_empty() { self.names.push(name); }
        }
    }

------------------------
Breaking Encapsulation
------------------------

*Or, When to* :rust:`pub`

* Transparency vs. Control

  * Use :rust:`pub` fields when struct is a simple "data bag"

    * No internal rules to protect
    * E.g., :rust:`Point { pub x: i32, pub y: i32 }`

* "Crate-Internal" Compromise

  * Use :rust:`pub(crate)` for items to be shared across project
  * But remain hidden from external users

.. note::

  Encapsulation hides code to help guarantee correctness

  Private fields cannot have "illegal" values
