=========
Traits
=========

----------------------------
Traits - Rust's Interfaces
----------------------------

* What is a :dfn:`trait?`

  * *Collection of method signatures* a type must implement
  * Similar to interfaces in other languages

* Traits let you abstract over types that share behavior

----------------------
Simple Trait Example
----------------------

.. code:: rust

  trait Pet {
      fn talk(&self) -> String;
      fn greet(&self);
  }

* Defines what methods types must provide
* Does **not** contain implementation

----------------------
Implementing a Trait
----------------------

.. code:: rust

  struct Dog { name: String, age: i8 }

  impl Pet for Dog {
      fn talk(&self) -> String { … }
      fn greet(&self) { … }
  }

* **Syntax:** :rust:`impl Trait for Type`
* :rust:`Dog` *acts* like :rust:`Pet` and can *use* trait methods :rust:`talk` and :rust:`greet`

-----------------------
Default Trait Methods
-----------------------

* Traits can provide **default implementations**
* Types implementing the trait can *use* or *override* them
* Defaults can call required methods

.. code:: rust

  trait Pet {
    fn talk(&self) -> String;
    // default 'greet' will use 'talk'
    fn greet(&self) {
        println!("Oh you're a cutie! What's your name? {}", self.talk());
    }
  }

  impl Pet for Dog {
    fn talk(&self) -> String {
        format!("Woof, my name is {}!", self.name)
    }
    // could define a 'greet' for Dog but not necessary
  }

-------------------
Traits vs Methods
-------------------

.. container:: latex_environment small

  .. list-table::
    :header-rows: 1

    * - Feature
      - Inherent Methods
      - Trait Methods

    * - Defined on type
      - :math:`\textcolor{green!65!black}{\checkmark}`
      - Via :rust:`impl Trait for Type`

    * - Shared abstract behavior
      - :color-red:`X`
      - :math:`\textcolor{green!65!black}{\checkmark}`

    * - Polymorphism
      - :color-red:`X`
      - :math:`\textcolor{green!65!black}{\checkmark}`

* Methods define behavior on a type
* Traits define shared behavior across multiple types
