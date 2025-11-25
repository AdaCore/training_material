========
Tuples
========

--------------------
Tuples: The Basics
--------------------

  - Tuples groups together values of **different types**
  - Like arrays, tuples have a **fixed length**
  - Fields are accessed by a :rust:`.` followed by its index, starting from :rust:`0`

.. code:: rust

   // Tuple with an i8 and a bool
   let alien_report: (i8, bool) = (3, false);
   println!("Number of tentacles: {}", alien_report.0); 
   println!("...is the creature hostile: {}", alien_report.1);

-----------------------
Tuples: The Unit Type
-----------------------

  - The empty tuple :rust:`()` is referred to as the **unit type**
  - It signifies the absence of a return value
  - Unit type :rust:`()` is implicitly returned where there is no explicit return

.. code:: rust

   // This function implicitly returns the unit type ()
   fn do_something() { 
      // ...
   }
