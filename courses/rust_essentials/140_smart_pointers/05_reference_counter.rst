==============================
Reference Counter
==============================

----------------------------------
:rust:`Rc<T>` Keeps the Light On
----------------------------------

-  Useful when single value is owned by multiple parts of a program

  -  Share ownership of a value on the heap

  -  Track the number of active references

  -  Prevent data cleanup until the last owner finishes


-- JBE TO DO example tele

------------------
Shared Ownership
------------------

-  :rust:`Box<T>` enforces strict singular ownership.

.. code:: rust 

  let a = Box::new(5);
  let b = Box::new(a);
  let c = Box::new(a);

:error:`error[E0382]: use of moved value: 'a'`
  
-  :rust:`Rc<T>` allows shared ownership but no mutability

.. code:: rust 

  let mut a = Rc::new(5);
  let b = Rc::clone(&a);
  let c = Rc::clone(&a);  
  
  *a += 10;
  
:error:`error[E0594]: cannot assign to data in an 'Rc'`

--------------
Fair Warning
--------------

-  Previous example works for types with :rust:`Copy`

.. code:: rust

  let a = 5;
  let b = Box::new(a);
  let c = Box::new(a);
  
  let d = Box::new(5);
  let e = Box::clone(&d);
  let f = Box::clone(&a);
  
-  Data is **not shared**, it is **duplicated**


------------------------------------------------------------
Why Use :rust:`Rc<T>` if :rust:`Box<T>` Can :rust:`Clone`?
------------------------------------------------------------

-  Avoid expensive data duplication

-  Synchronize state across multiple owners

-  Save memory by reusing the same heap allocation

.. tip::  

  Copying an integer is fast, but not copying large data files!