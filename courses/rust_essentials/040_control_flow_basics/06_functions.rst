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

  fn get_goblin_bribe(mood_rating: u32, shiny_stone: bool) {
      let tribute = 50;
      let bonus = 100;
      if mood_rating >= 8 && shiny_stone {
          tribute
      } else {
          tribute + bonus
      }
  }

-------------------------------
Functions With No Return Value
-------------------------------

  - Some functions have no return value
  - Returns 'unit type' :rust:`()`
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
  - Always takes a single set of parameter types
- Always takes a fixed number of parameters
- Default arguments are not supported




