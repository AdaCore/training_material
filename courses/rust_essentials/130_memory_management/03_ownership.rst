===========
Ownership
===========

--------------------
Scope and Validity
--------------------

- Variable bindings are only accessible within their defined **scope**
- Out-of-scope variables are strictly caught at compile-time

.. code:: rust

   struct Point(i32, i32);

   { // Outer scope starts
       { // Inner scope starts
           let pt = Point(3, 4); // 'pt' becomes valid
           println!("x: {}", pt.0);
       } // Inner scope ends, 'pt' is dropped
       println!("y: {}", pt.1); // Error
   } // Outer scope ends

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
            let poodle = String::from("ball"); // 'poodle' owns the ball
            let yorkie = poodle; // 'poodle' lets go, 'yorkie' picked it up
            
            // println!("{}", poodle); // Error
            println!("{}", yorkie);
        } // 'yorkie' drops the ball, and leaves
          // 'poodle' leaves quietly
