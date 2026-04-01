===============
Borrow Errors
===============

-------------------
Reallocation Trap
-------------------

.. code:: rust

    let mut vec = vec![1, 2, 3, 4, 5];
    let elem = &vec[2];    // Immutable borrow
    vec.push(6);           // Mutable borrow - This won't compile
    println!("{elem}");    // Immutable borrow final use

-------------------------
Modifying While Looping
-------------------------

.. code:: rust

    let mut vec = vec![1, 2, 3, 4, 5];
        
    for elem in &vec {       // Immutable borrow of whole collection
        vec.push(elem * 2);  // Mutable borrow - This won't compile
    }                        // Immutable borrow ends when loop finishes
