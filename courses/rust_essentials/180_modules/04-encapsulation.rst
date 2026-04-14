===============
Encapsulation
===============

------------------
Why Encapsulate?
------------------

* Protect internal state of a data structure

  * Ensure data is always valid
  * Typically implemented via getter/setter API's

* Decoupling

  * Hides implementation details
  * Can change code without breaking client

    * E.g., swapping an array for a :rust:`Vec`

--------------------------
Encapsulation in Structs
--------------------------

*Gatekeeper* **pattern**

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

**Or, When to** :rust:`pub`

**Transparency vs. Control**

  * Use :rust:`pub` fields when struct is a simple "data collection"

    * No internal rules to protect
    * E.g., :rust:`Point { pub x: i32, pub y: i32 }`

**"Crate-Internal" Compromise**

  * Use :rust:`pub(crate)` for items to be shared across project
  * But remain hidden from external users

.. note::

  Encapsulation hides code to help guarantee correctness

  Private fields are only modifiable by "designer"

-----------------------
"pub" vs "pub(crate)"
-----------------------

.. code:: rust

  // Some connection data
  pub struct Client {
      pub url: String,
  }

  // Only this crate can see content
  pub(crate) struct Connection {
      pub(crate) socket_id: u32,
  }

  // Users of this crate can connect
  impl Client {
      pub fn connect(&self) -> Connection {
          // Logic to create a connection...
          Connection { socket_id: 101 }
      }
  }

**Why** :rust:`pub(crate)`**?**

  * You don't want clients checking the content
  * Once published, changing it breaks users code
