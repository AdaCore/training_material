=========
"Rc<T>"
=========

---------------------------------
Multiple Ownership With "Rc<T>"
---------------------------------

- Useful when *single* value is owned by *multiple* parts of a program

  - Tracks number of owners

  - Prevents data cleanup until last owner finishes

- Single-threaded reference-counted smart pointer

  - Brought into scope with :rust:`use std::rc::Rc`

---------------------------------
Reference Counting With "Rc<T>"
---------------------------------

**Shares ownership of the same heap allocation**

- :rust:`Rc::clone` creates another owner
- Increments internal counter

.. code:: rust

  // Mulder and Scully share ownership of the same value
  let mulder = Rc::new(51);
  println!("Count: {}", Rc::strong_count(&mulder));

  let scully = Rc::clone(&mulder);
  println!("Count: {}", Rc::strong_count(&mulder));

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

  *tic += 10; // Error

.. code:: error

  error[E0594]: cannot assign to data in an 'Rc'
