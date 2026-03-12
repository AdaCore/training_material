===========
Ownership
===========

--------------------
Scope and Validity
--------------------

- Variable bindings are only accessible within their defined :dfn:`scope`
- Out-of-scope variables are strictly caught at compile-time

.. code:: rust

   struct Point(i32, i32);

   fn main() { // --- Outer scope starts
       { // --- Inner scope starts
           let pt = Point(3, 4); // 'pt' becomes valid
           println!("x: {}", pt.0);
       } // <--- Inner scope ends, 'pt' is dropped
       println!("y: {}", pt.1);
   } // <--- Outer scope ends

:error:`error[E0425]: cannot find value 'pt' in this scope`

----------------------
Ownership Principles
----------------------

- Variable **owns** the value
- Every value has precisely **one owner** at all times
- When the owner goes **out of scope**, the value is **dropped**

.. container:: latex_environment scriptsize

    .. code:: rust

        {
            let poodle = String::from("ball"); // Poodle owns the ball
            let yorkie = poodle; // Poodle lets go, Yorkie picked it up
            
            // println!("{}", poodle); // ERROR: Poodle's mouth is empty
            println!("{}", yorkie);
        } // Yorkie drops the ball, and leaves
          // Poodle leaves quietly