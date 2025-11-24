====================================
:rust:`break` and :rust:`continue`
====================================

------------------------------------
:rust:`break` and :rust:`continue`
------------------------------------

    - Use :rust:`continue` to immediately start the next iteration
    - Use :rust:`break` to exit any kind of loop early
    - Both :rust:`continue` and :rust:`break` work with :rust:`while`, :rust:`for`, and :rust:`loop`
    
.. code:: rust

    fn main() {
        let mut count = 0;
        loop {
            count += 1;
            if count < 3 { continue; } 
            if count > 5 { break; }
            println!("{}", count);
        };        
    }

-------------------------------------------------------
Returning a value with :rust:`loop` and :rust:`break`
-------------------------------------------------------

    - Only :rust:`loop` is a looping construct and an expression
    - It is an expression that can return a non-trivial value

.. code:: rust

    fn main() {
        let mut count = 0;
        let result = loop {
            count += 1;             
            if count > 5 { break count; }
            println!("{}", count);
        };
        println!("Result: ", result);
    }


--------
Labels
--------

    - Both :rust:`continue` and :rust:`break` can optionally take a label argument
    - Used to break out of nested loops:

.. code:: rust

    fn main() {
        let mut chocolates_eaten = 0;
        'chocolate_boxes: for _box_number in 1..=5 {
            for _chocolate_piece in 1..=5 {
                chocolates_eaten += 1;
                if chocolates_eaten == 13 {
                    break 'chocolate_boxes; 
                }
            }
        }
    }

--------------
Block Labels
--------------

-  Labeled break also works on arbitrary blocks, e.g.

  .. code:: rust

      'label: {
          break 'label;
          println!("This line gets skipped");
      }
