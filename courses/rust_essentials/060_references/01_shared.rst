===================
Shared References
===================

-------------------
Shared References
-------------------

- Created with the :rust:`&` operator
- Provide a mechanism to access a value without taking ownership
- Strictly **read-only**: the referenced data **cannot change**
  - Even if the original variable was declared as :rust:`mut`

.. code:: rust

    let first = 'A';
    let reference: &char = &first; // Refers to 'first'

.. note::

    A shared reference to a type :rust:`T` has type :rust:`&T`


-----------------------------------
Using Reference (Accessing data)
-----------------------------------

- The :rust:`*` operator **dereferences** the reference to read the value
  - Use :rust:`.` for field access (no C++ style :rust:`->`)

.. code:: rust

    let first = 'A';
    let reference: &char = &first; // Refers to 'first'
    println!("reference: {}", *reference);

* Generates the following output

:command:`reference: A`

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
