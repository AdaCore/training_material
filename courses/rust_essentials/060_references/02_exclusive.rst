======================
Mutable References
======================

-----------------------------------------------
Mutable References (aka Exclusive References)
-----------------------------------------------

- Created with :rust:`&mut` operator
- Allow modifying value they point to
- Cannot coexist with any other reference
- Mutable reference to a type :rust:`T` has type :rust:`&mut T`

.. code:: rust

    let mut two_plus_two = 4;
    let big_brother = &mut two_plus_two;
    *big_brother = 5; 
    println!("Truth: {two_plus_two}");

:command:`Truth: 5`

.. note::

  A :rust:`&mut` reference cannot be created from an **immutable** variable

