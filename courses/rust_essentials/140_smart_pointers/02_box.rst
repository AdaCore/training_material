================
:rust:`Box<T>`
================

--------------------
The Heap Allocator
--------------------

-  Allocates data on the heap 

-  Stores a fixed-size pointer on the stack 

-  Retains single ownership of the heap data 

-  Deallocates memory automatically when :rust:`Box` goes out of scope 

.. code:: rust

  pub struct Box<T>(Unique<T>);

  let b = Box::new(5);

  // Use 'b' as if it were a regular reference
  println!("b = {}", b);
  
:command:`b = 5`
 
-----------------------------------
Bypassing Static Size Constraints
-----------------------------------

-  Compiler requires size of every type at compile time

   -  Recursive types size is not known
  
-  :rust:`Box<T>` provides a fixed-width address

   -  Breaks direct recursion loop in memory
  
.. code:: rust

  // FAILS: How big is an infinite doll?
  enum Doll {
    Inside(Doll),
    Empty,
  }

  // WORKS: The Box is just a small "map" to the next doll
  enum Doll {
    Inside(Box<Doll>),
    Empty,
  }
  
-------------------------------
Automatic Resource Management
-------------------------------

-  :rust:`Box<T>` implements :rust:`Drop` to ensure memory safety 

   -  Invoke the :rust:`Drop` method automatically at end of scope
  
   -  Prevents memory leaks by ensuring deallocation