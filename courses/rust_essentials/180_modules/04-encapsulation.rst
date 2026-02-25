===============
Encapsulation
===============

------------------
Why Encapsulate?
------------------

* Invariants - protect internal state of a data structure

  * Ensure data is always valid

    * Can only be changed via approved logic

* Decoupling - hiding implementation details

  * Can change your internal code without breaking client

    * E.g., swapping an array for a :rust:`Vec`

* API Surface - smaller public API

  * Easier to document
  * Easier to learn
  * Harder to misuse

--------------------------
Encapsulation in Structs
--------------------------

* *Gatekeeper* pattern

  * Keep fields private (default behavior)
  * Provide a :rust:`pub fn new(...)` constructor

    * Ensure struct starts in valid state

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

  Encapsulation is not just "hiding" code; it is about **guaranteeing correctness**

  If a field is private, you guarantee no outside code can put an "illegal" value in
