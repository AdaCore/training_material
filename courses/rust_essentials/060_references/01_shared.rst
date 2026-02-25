===================
Shared References
===================

-------------------
Shared References
-------------------

- Created with the :rust:`&` operator
- Provide a mechanism to access a value without taking ownership
- Strictly **read-only**: referenced data **cannot change**
  - Even if the *original* variable was declared as :rust:`mut`
- A shared reference to a type :rust:`T` has type :rust:`&T`

.. code:: rust

    let first = 'A';
    let ref_1: &char = &first; // Refers to 'first'
    let ref_2: &char = &first; // Refers to 'first'

.. note::

    You can have unlimited shared references to the same data at the same time    

-----------------------------------
Using Reference (Accessing data)
-----------------------------------

- The :rust:`*` operator **dereferences** the reference to read the value

.. code:: rust

    let first = 'A';
    let reference: &char = &first; // Refers to 'first'
    println!("reference: {}", *reference);

* Generates the following output

:command:`reference: A`

------------------------------------------
Automatic Dereferencing for Field Access
------------------------------------------

- Use :rust:`.` for field access 
  - No C++ style :cpp:`->`

.. code:: rust

    let coordinates = (3, 5); 
    let reference = &coordinates;
    
    println!("x: {}, y: {}", coordinates.0, coordinates.1);
    println!("ref x: {}, ref y: {}", reference.0, reference.1);
 
* Generates the following output

:command:`x: 3, y: 5`

:command:`ref x: 3, ref y: 5`

-------------------------
Reference Reassignement
-------------------------

.. code:: rust

    let first = 'A';
    let second = 'B';
    let mut reference: &char = &first; // Refers to 'first'
    println!("reference: {}", *reference);
    reference = &second; // Now refers to 'second'
    println!("reference: {}", *reference);

* Generates the following output

:command:`reference: A`

:command:`reference: B`
