=============================
User-Defined Smart Pointers
=============================

-----------------------------
Implementing Smart Pointers
-----------------------------

**Can define custom smart pointers**

.. code:: rust 

  use std::ops::Deref;
  struct MyBox<T>(T);
  impl<T> MyBox<T> {
    fn new(x: T) -> MyBox<T> {
        MyBox(x)
    }
  }
  
  let name = MyBox::new(5);  
  println!("Hello, 00{}!", *name );
  
:error:`error[E0614]: type 'MyBox<{integer}>' cannot be dereferenced`

------------------------------------
Working with Custom Smart Pointers
------------------------------------

- Need to implement :rust:`Deref`

  - Can define custom logic inside :rust:`deref()` method

.. code:: rust 

  impl<T> Deref for MyBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0 // Return reference to inner value
    }
  }
  println!("Hello, 00{}!", *name );
  
- :rust:`Drop` should be implemented too

  - To behave like other *smart pointers* 

--------------
"Drop" Trait
--------------

- Most standard *smart pointers* implement :rust:`Drop`


- Allows :rust:`drop` method to define custom cleanup logic 

.. code:: rust


  impl<T> Drop for MyBox<T> {
    fn drop(&mut self) {
        println!("A MyBox is being cleared from memory");
    }
  }
  let name = MyBox::new(5);  
  println!("Hello, 00{}!", *name );

:command:`Hello, 007!`

:command:`A MyBox is being cleared from memory`
  