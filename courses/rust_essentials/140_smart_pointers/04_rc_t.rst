=========
"Rc<T>"
=========

---------------------------------
Multiple Ownership With "Rc<T>"
---------------------------------

- Useful when *single* value is owned by *multiple* parts of a program
  
  - Tracks the number of owners

  - Prevents data cleanup until last owner finishes
  
- Single-threaded reference-counted smart pointer

  - included with :rust:`use std::rc::Rc`
  
---------------------------------
Reference Counting With "Rc<T>"
---------------------------------

**Shares ownership of the same heap allocation**

- :rust:`Rc::clone` creates another owner
- Increments the internal counter
 
.. code:: rust 

  // Both 'var_a' and 'var_b' share ownership of the value
  let var_a = Rc::new(5);
  println!("Count: {}", Rc::strong_count(&var_a)); 

  let var_b = Rc::clone(&var_a);
  println!("Count: {}", Rc::strong_count(&var_a)); 
  
.. code:: output

  Count: 1
  Count: 2

---------------
Shared Access
---------------

  
:rust:`Rc<T>` **does not implement** :rust:`DerefMut`

.. code:: rust 

  let tic = Rc::new(5);
  let tac = Rc::clone(&tic);
  let toe = Rc::clone(&tic);  

  *tic += 10; // Error: no mutable access
  
.. code:: error

  error[E0594]: cannot assign to data in an 'Rc'
