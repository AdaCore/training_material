================
Move Semantics
================

------------------------
Transferring Ownership
------------------------

Assigning a value to a new variable transfers ownership

.. code:: rust

    let s1 = String::from("Hello");
    let s2 = s1;

    // println!("{}", s1); // Error! s1 is no longer valid
    println!("s2: {}", s2);

Before move to :rust:`s2`:

.. image:: comprehensive_rust_training/review_of_program_memory.svg

After move to :rust:`s2`:

.. image:: comprehensive_rust_training/move_semantics_2.svg

