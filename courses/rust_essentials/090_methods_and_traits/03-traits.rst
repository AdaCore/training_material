=========
Traits
=========

----------------------------
Traits - Rust's Interfaces
----------------------------

* What is a :dfn:`trait?`

  * *Collection of method signatures* a type must implement
  * Similar to interfaces in other languages **but**

    * Typically compile-time resolution
    * Traits are *separate* from types
    * Traits can define associated types and constants

* Traits let you abstract over types that share behavior

----------------------
Simple Trait Example
----------------------

.. code:: rust

  trait Friend {
      fn response(&self) -> String;
      fn greet(&self);
  }

* Defines what methods types must provide
* Does **not** contain implementation

----------------------
Implementing a Trait
----------------------

**Syntax**

  .. code:: rust

    impl SomeTrait for TheType { ... }

**Example**

  .. code:: rust

    struct Dog { name: String, age: i8 }

    impl Friend for Dog {
        fn response(&self) -> String { String::from("Wag!") }
        fn greet(&self) { println!("Hello"); }
    }

**Behavior**

  * :rust:`Dog` has the :rust:`Friend` capability (or behavior)
  * :rust:`Dog` is **not** derived from :rust:`Friend`

-----------------------
Default Trait Methods
-----------------------

* Traits can provide **default implementations**
* Types implementing the trait can *use* or *override* them
* Defaults can call required methods

.. code:: rust

  trait Friend {
    fn response(&self) -> String;
    // default 'greet' will use 'response'
    fn greet(&self) {
        println!("Hello! {}", self.response());
    }
  }

  impl Friend for Dog {
    fn response(&self) -> String {
        format!("I'm a dog, my name is {}!", self.name)
    }
    // Without defining 'greet', it would print
    // Hello! I'm a dog my name is Fido
  }

-------------------
Traits vs Methods
-------------------

+----------------+----------------------------+-----------------------------------+
| **Feature**    | **Methods**                | **Traits**                        |
+================+============================+===================================+
| *Identity*     | What a :rust:`Dog` **is**  | What a :rust:`Dog` **can do**     |
+----------------+----------------------------+-----------------------------------+
| *Availablity*  | Only for :rust:`Dog`       | Any type that is a :rust:`Friend` |
+----------------+----------------------------+-----------------------------------+
| *Example*      | :rust:`fn wag_tail(&self)` | :rust:`fn response(&self)`        |
+----------------+----------------------------+-----------------------------------+

-----------------------
Real World Comparison
-----------------------

* Methods are *private skills*

  * A dog knows how to wag its tail (but a cat doesn't)
  * Put :rust:`wag_tail` in :rust:`impl Dog`

* Traits are *common ground*

  * Both dog and cat have a friendly response (but they're different)
  * Put the response in a trait which doesn't care *what* is responding
