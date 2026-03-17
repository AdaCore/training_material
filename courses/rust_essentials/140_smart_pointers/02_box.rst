==========
"Box<T>"
==========

-----------------
Heap Allocation
-----------------

- :rust:`Box<T>` allocates data on the heap 

- Stores a fixed-size pointer on the stack 

- Retains single ownership of heap data 

- Deallocates memory automatically when :rust:`Box` goes out of scope 

.. code:: rust

  // 'Box::new()' is used to allocate data
  let my_box = Box::new(5);

  // Implicit dereference
  println!("Box value is {}", my_box);
  
:command:`Box value is 5`
 
----------------------------------
Using "Box<T>" for Recursive Type
----------------------------------

- Compiler requires each type to have a known size at compile time

  - Recursive types don't have a known size
  
- :rust:`Box<T>` provides a pointer with known size

  - Breaks direct recursion loop in memory
  
.. code:: rust

  // FAILS: How big is an infinite doll?
  enum Doll {
   Inside(Doll),
   Empty,
  }

  // WORKS: The "Box" is just a pointer to the next doll
  enum Doll {
   Inside(Box<Doll>),
   Empty,
  }
  
-------------------------------
Automatic Resource Management
-------------------------------

- :rust:`Box<T>` allows to move ownership of large data instead

  - Useful for function call rather than copying data passed on
  
  - Move is an O(1) operation regardless of what it points to

- :rust:`Box<T>` implements :rust:`Drop` to ensure memory safety 

  - Invokes :rust:`Drop` method automatically at end of scope
  
    - No need for manual intervention
  
  - Prevents memory leaks by ensuring deallocation
  
- :rust:`Box::new` has no runtime overhead just like C :c:`malloc`
  

  