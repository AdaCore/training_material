========
Tuples
========

--------------------
Basics
--------------------

  - Groups together values of **different types**
  - Like arrays, have a **fixed length**
  - Fields are accessed via dot notation by their index, starting from :rust:`0`

.. code:: rust

   // Tuple with an i8 and a bool
   let alien_report: (i8, bool) = (3, false);
   println!("Number of tentacles: {}", alien_report.0); 
   println!("...is the creature hostile: {}", alien_report.1);
