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

   fn main() {
       {
           let pt = Point(3, 4);
           println!("x: {}", pt.0);
       }
       println!("y: {}", pt.1); // Error
   }

:error:`error[E0425]: cannot find value 'pt' in this scope`

----------------------
Ownership Principles
----------------------

- Variable **owns** the value
- Every value has precisely **one owner** at all times
- When the owner goes **out of scope**, the value is **dropped**
