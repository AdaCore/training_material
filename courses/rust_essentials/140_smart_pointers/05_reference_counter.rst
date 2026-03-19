====================
Reference Counting
====================

----------------------------
"Rc<T>" Keeps the Light On
----------------------------

- Shares ownership of value on the heap using :rust:`Clone`
    
  - Creates a *shallow* copy not a deep copy

    - Only the *pointer* is copied
	
  - Increments the internal counter

- Called :dfn:`Reference Counted (Smart) Pointer`
  
- Useful when single value is owned by multiple parts of a program
  
  - Only provides *immutable access* to the data

  - Tracks number of active references

  - Prevents data cleanup until last owner finishes
 
.. code:: rust 

  // Both 'var_a' and 'var_b' share ownership of the value
  let var_a = Rc::new(5);
  println!("Count: {}", Rc::strong_count(&var_a)); 

  let var_b = Rc::clone(&var_a);
  println!("Count: {}", Rc::strong_count(&var_a)); 
  
:command:`Count: 1`

:command:`Count: 2`

------------------
Shared Ownership
------------------

- :rust:`Box<T>` enforces strict singular ownership

.. code:: rust 

  let var_a = Box::new(5);
  let var_b = var_a;
  let var_c = var_a;

:error:`error[E0382]: use of moved value: 'var_a'`
  
- :rust:`Rc<T>` allows shared ownership but no mutable access

.. code:: rust 

  let var_a = Rc::new(5);
  let var_b = Rc::clone(&var_a);
  let var_c = Rc::clone(&var_a);  
  
  *var_a += 10;
  
:error:`error[E0594]: cannot assign to data in an 'Rc'`

--------------
Fair Warning
--------------

- Previous example works for :rust:`Copy` types

.. code:: rust

  let var_a = 5;
  let var_b = Box::new(var_a);
  let var_c = Box::new(var_a);
  
- :rust:`Clone` works also with :rust:`Box<T>`

.. code:: rust

  let var_d = Box::new(5);
  let var_e = Box::clone(&var_d);
  let var_f = Box::clone(&var_d);

.. warning::  

  In **both** cases, data is **not shared**, it is **duplicated**

------------------------------------------
Why Use "Rc<T>" if "Box<T>" Can "Clone"?
------------------------------------------

- Avoids expensive data duplication

- Shares same state across multiple owners

- Saves memory by reusing same heap allocation

.. tip::  

  Copying an integer is fast, but not copying large data files!
  