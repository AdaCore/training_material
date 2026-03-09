===========
Ownership
===========

--------------------
Scope and validity
--------------------

- All variable bindings have a *scope* where they are valid
- It is an error to use a variable outside its scope

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
Ownership principles
----------------------

- Variable **owns** the value
- Every value has precisely **one owner** at all times
- At the end of the scope, the variable is *dropped* and data is freed