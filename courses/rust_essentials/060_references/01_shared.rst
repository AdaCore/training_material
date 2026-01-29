===========================
Shared References
===========================

------------------------------
Shared References
------------------------------

- Created with the :rust:`&` operator
- A shared reference to a type :rust:`T` has type :rust:`&T`
- Provide a mechanism to access another value
- Strictly **read-only**: the referenced data cannot change
- The :rust:`*` operator **dereferences** the reference to read the value
  - Use :rust:`.` for field access (no C++ style :rust:`->`)
- Prevent modification of the referenced value
  - Applies even if the referenced variable is :rust:`mut`

.. code:: rust

    let first = 'A';
    let second = 'B';
    let mut ref: &char = &first; // refers to first
    println!("ref: {}", *ref);
    ref = &second; // now refers to second
    println!("ref: {}", *ref);

* Generates the following output:

:command:`ref: A`

:command:`ref: B`
