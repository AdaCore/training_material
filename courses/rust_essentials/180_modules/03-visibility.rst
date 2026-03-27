============
Visibility
============

--------------------
Private by Default
--------------------

* Every item is private to the module it's in

  * Default behavior

* :rust:`pub` keyword makes item accessible to the caller

* Visibility is top-down

  * Child module can see everything in its parent
  * Parent can only see :rust:`pub` items in its child

----------------------
Private Code Example
----------------------

.. code:: rust

  mod outer {
      fn private() {
          println!("outer::private");
      }

      pub fn public() {
          println!("outer::public");
      }

  }

  fn main() {
    outer::public();
    outer::private();
  }

:error:`error[E0603]: function 'private' is private`

.. note::

  Compiler prevents illegal usage

----------------------------
Struct and Enum Visibility
----------------------------

* Making a struct :rust:`pub` does not make its fields public

  * Each field needs its own :rust:`pub`

  .. code:: rust

    pub struct MyType {
        pub value: i32,     // Public
        initialized: bool,  // Private
    }

* Making an enum :rust:`pub` makes **all** variants public
