===========
Functions
===========

-----------
Functions
-----------

.. code:: rust

   fn gcd(a: u32, b: u32) -> u32 {
       if b > 0 {
           gcd(b, a % b)
       } else {
           a
       }
   }

   fn main() {
       println!("gcd: {}", gcd(143, 52));
   }

-----------------
Function Syntax
-----------------

- Declaration parameters are followed by a type (like Ada, unlike C/C+) then a return type.
- Last expression in a function body (or any block) becomes the return value.

  - Omit the :rust:`;` at the end of the expression.

- :rust:`return` keyword can be used for early return

  - *Bare value* form is idiomatic at the end of a function

- Some functions have no return value

  - Return 'unit type' :rust:`()`

    - Compiler will infer this if return type is omitted.

- Overloading is not supported - each function has a single implementation.

  - Always takes a fixed number of parameters

    - Default arguments are not supported
    - Macros can be used to support variadic functions.

  - Always takes a single set of parameter types

    - These types can be generic, which will be covered later.
