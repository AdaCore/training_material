=======
Clone
=======

-------
Clone
-------

- Creates a deep copy of the underlying data
  - Typically duplicating heap-allocated resources
- Useful when original variable must remain valid after a function call
- Significantly more expensive than a move 
  - Requires new memory allocation and data migration
- Serves as a clear visual marker of intentional heap allocation
  - With :rust:`.clone()` syntax

.. code:: rust

    let name = String::from("Alice");
    
    // Explicitly clone the data
    say_hello(name.clone());

    // 'name' is still valid here
    say_hello(name);

.. note::

   It is common to prioritize progress over performance by "cloning away" ownership conflicts, deferring optimization until the logic is stable
