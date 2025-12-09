====================================
"break" and "continue"
====================================

------------------------------------
"break" and "continue"
------------------------------------

    - Start immediately start the next iteration with :rust:`continue`
    - Exit any kind of loop early with :rust:`break`
    - Both work with :rust:`while`, :rust:`for`, and :rust:`loop`

.. code:: rust

    let mut count = 0;
    loop {
        count += 1;
        if count < 3 { continue; } 
        if count > 5 { break; }
        println!("{}", count);
    }; 

* Generating the following output:

:command:`3`

:command:`4`

:command:`5`
  
-------------------------------------------------------
Returning a Value with "loop" and "break"
-------------------------------------------------------

    - :rust:`loop` is also an expression
    - Can return a non-trivial value

.. code:: rust

    let mut count = 0;
    let result = loop {
        count += 1;             
        if count > 5 { break count; }
        println!("{}", count);
    };
    println!("Result: {}", result);

--------
Labels
--------

  - **Optionally** attached to loops (:rust:`loop`, :rust:`while`, :rust:`for`)
  - Denoted by a single quote (:rust:`'`) followed by an identifier
  - Both :rust:`continue` and :rust:`break` can optionally take a label argument
  - Primarly used to break out of nested loops
    - Or to continue an outer loop from within an inner one

.. code:: rust

    let mut eaten = 0;
    'outer: for _box in 1..=5 {
        for _piece in 1..=5 {
            eaten += 1;
            if eaten == 13 {
                break 'outer; 
            }
        } // inner loop ends
    } // outer loop ends
    println!("Sugar crash at: {}", eaten);
    
--------------
Block Labels
--------------

-  Labeled break also works on arbitrary blocks

  .. code:: rust

      'label: {
          break 'label;
          println!("This line gets skipped");
      }
