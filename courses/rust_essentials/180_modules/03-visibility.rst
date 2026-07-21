============
Visibility
============

--------------------
Private by Default
--------------------

* All items of a module are private (hidden)

  * Unless otherwise specified

* :rust:`pub` keyword makes item public (visible)

* Child module can see everything in its parent

  * Parent can only see :rust:`pub` items in its child

* Enforced by compiler

-------------------
Module Visibility
-------------------

:filename:`parent.rs`

  .. code:: rust

    pub mod child_pub;
    mod child_priv;

    fn parent_secret_handshake() {
        println!("Hello from the parent's private function!");
    }

    pub mod control {
        pub fn run_private_child() {
            // Use 'super' to refer to enclosing module ('parent')
            super::child_priv::secret_mission();
            // The following will generate an error
            super::child_priv::deeply_hidden();
        }
    }

:filename:`child_priv.rs`

  .. code:: rust

    pub fn secret_mission() {
        println!("I am the private child module executing a task!");
    
        // Use 'super' to refer to enclosing module ('parent')
        super::parent_secret_handshake();
    }

    fn deeply_hidden() {
        println!("Not even the parent can see this.");
    }

:filename:`child_pub.rs`

  .. code:: rust

    pub fn speak() {
        println!("I am the public child module!");
    
        // Accessing the parent's private function using `super`
        super::parent_secret_handshake();
    }

:error:`error[E0603]: function 'deeply_hidden' is private`

---------------------------
Visibility at Every Level
---------------------------

* Making a struct :rust:`pub` allows client to know it exists

  * But does not make its fields public

* Each field needs its own :rust:`pub`

  * Client knows :rust:`pub` fields exist

  .. code:: rust

    pub struct MyType {
        pub value: i32,     // Public
        initialized: bool,  // Private
    }

* Making an enum :rust:`pub` makes **all** variants public

--------------------------
Example: Security Module
--------------------------

:filename:`security.rs`

.. code:: rust

  // 'MasterKey` only visible in this file
  struct MasterKey { level: u8, }

  // 'KeyCard' is visible to caller
  pub struct KeyCard { pub ident: u32,  // Caller can see 'ident'
                       key: MasterKey,  // But cannot see 'key'
  }

  // Caller can 'issue_card'
  pub fn issue_card(ident: u32) -> KeyCard {
      KeyCard { ident,
                key: generate_master_key(),
      }
  }

  // Caller cannot 'generate_master_key'
  fn generate_master_key() -> MasterKey {
      MasterKey { level: 255 }
  }

:filename:`main.rs`

.. code:: rust

  // Grant visibility to 'security' module
  mod security;
  fn main() {
      let mut my_card = security::issue_card(1234);
      my_card.ident = my_card.ident * 10;  // Can see this field
      println!("{}", my_card.ident);
      my_card.key = my_card.key * 10;      // Error - field not visible
  }

:error:`error[E0616]: field 'key' of struct 'KeyCard' is private`
