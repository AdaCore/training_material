============
Visibility
============

--------------------
Private by Default
--------------------

* All elements of a module are private (hidden)

  * Unless otherwise specified

* :rust:`pub` keyword makes item public (visible)

* Visibility is top-down

  * Parent can only see :rust:`pub` items in its child
  * But child module can see everything in its parent

* Visibility enforced by compiler

---------------------------
Visibility at Every Level
---------------------------

* Making a struct :rust:`pub` does not make its fields public

  * Each field needs its own :rust:`pub`

  .. code:: rust

    pub struct MyType {
        pub value: i32,     // Public
        initialized: bool,  // Private
    }

* Making an enum :rust:`pub` makes **all** variants public

--------------------------
Example: Security Module
--------------------------

**File** :filename:`security.rs`

.. code:: rust

  // 'MasterKey` only visible in this file
  struct MasterKey {
      level: u8,
  }

  // 'KeyCard' is visible to caller
  pub struct KeyCard {
      pub ident: u32,  // Caller can see 'ident'
      key: MasterKey,  // But cannot see 'key'
  }

  // Caller can 'issue_card'
  pub fn issue_card(ident: u32) -> KeyCard {
      KeyCard {
          ident,
          key: generate_master_key(),
      }
  }

  // Caller cannot 'generate_master_key'
  fn generate_master_key() -> MasterKey {
      MasterKey { level: 255 }
  }

--------------------------------
Example: Using Security Module
--------------------------------

**File:** :filename:`main.rs`

.. code:: rust
  ..line-numbers: 1

  mod security;
  fn main() {
      let mut my_card = security::issue_card(1234);
      my_card.ident = my_card.ident * 10;
      println!("{}", my_card.ident);
      my_card.key = my_card.key * 10;
  }

1. We need access to :rust:`security` module
2. Main program
3. Request a :rust:`KeyCard`
4. Examine/modify public field :rust:`ident`
5. Print the public field
6. Examine/modify private field is compile error

:error:`error[E0616]: field 'key' of struct 'KeyCard' is private`
