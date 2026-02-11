======================
Exclusive References
======================

--------------------
Mutable References
--------------------

- A mutable reference to a type :rust:`T` has type :rust:`&mut T`
  - Allows changing the value they refer to
  - Also known as exclusive references
- No other references (shared or exclusive) can exist simultaneously
- You cannot create a :rust:`&mut` reference to an immutable variable


.. code:: rust

    let mut two_plus_two = 4;
    let big_brother = &mut two_plus_two;
    *big_brother = 5; 
    println!("Truth: {two_plus_two}");

* Generates the following output

:command:`Truth: 5`

----------------------
Binding vs Reference
----------------------


Difference between a mutable binding and a mutable reference:

.. code:: rust

   // Shared reference
   // Can be reassigned to point to different items
   let mut reference: &i32;

   // Exclusive reference to a mutable value
   // Referenced value can be changed
   let reference: &mut i32;
