===========
Functions
===========

-----------
Functions
-----------

- Declaration parameters are followed by a type
  - Like Ada, unlike C/C+
- Last expression in a function body becomes the return value
- *Bare value* form is idiomatic at the end of a function
  - Omit the :rust:`;` at the end of the expression

.. code:: rust

   fn gcd(a: u32, b: u32) -> u32 {
       if b > 0 {
           gcd(b, a % b)
       } else {
           a
       }
   }

-------------------------------
Function With No Return Value
-------------------------------

  - Some functions have no return value
  - Return 'unit type' :rust:`()`
  - Compiler will infer this if return type is omitted
  - :rust:`return` keyword can be used for early return

.. code:: rust

   fn do_something() {
       println!("doing something!);
   }

---------------------------------
Function Features Not Supported 
---------------------------------

- Overloading is not supported
  - Each function has a single implementation
- Always takes a fixed number of parameters
- Default arguments are not supported
- Always takes a single set of parameter types



