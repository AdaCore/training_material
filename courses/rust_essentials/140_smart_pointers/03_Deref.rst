==============================
Dereferencing Smart Pointers
==============================

---------------------
:rust:`Deref` Trait
---------------------

-  Writes code that works for both references and smart pointers

-  Returns a reference to the inner data to avoid moving ownership

-  Allows overriding dereference operator :rust:`*`

.. code:: rust 

  
------------------------
:rust:`Deref` Coercion
------------------------

-  converts a reference to one type into a reference to another type

-  Perform multiple "steps" of coercion at compile time

-  Incur zero runtime performance penalty

-  Access the inner value of a smart pointer transparently

.. code:: rust 

  fn hello(name: &str) {
    println!("Hello, {name}!");
  }

  let m = MyBox::new(String::from("Rust"));
    
  // &m is &MyBox<String>
  // Rust coerces: &MyBox<String> -> &String -> &str
  hello(&m); // No need to do hello(&m.0)

-------------------------
Mutability and Coercion
-------------------------

*Prohibit &T to &mut T (Never coerce Immutable to Mutable)*

.. list-table::
   :header-rows: 1

   * - From
     - To
     - Result

   * - :rust:`&T`
     - :rust:`&U`
     - :color-green:`V`


   * - :rust:`&mut T`
     - :rust:`&mut U`
     - :color-green:`V`


   * - :rust:`&mut T`
     - :rust:`&U`
     - :color-green:`V`

   * - :rust:`&T`
     - :rust:`&mut U`
     - :color-red:`X`


-----------------------------
User Defined Smart Pointers
-----------------------------

-  Can define custom smart pointers

.. code:: rust 

  use std::ops::Deref;
  struct MyBox<T>(T);
  impl<T> MyBox<T> {
    fn new(x: T) -> MyBox<T> {
        MyBox(x)
    }
  }
  
  let x = MyBox::new(5);  
  *x = 10;
  
error[E0614]: type `MyBox<{integer}>` cannot be dereferenced  

-  Neef to implement :rust:`Deref`

.. code:: rust 

  impl<T> Deref for MyBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0 // Return reference to inner value
    }
  }
  *x = 10;
  
-  To be a smart pointer, :rust:`Drop` has to be implemented too