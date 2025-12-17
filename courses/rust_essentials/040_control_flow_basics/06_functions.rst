===========
Functions
===========

---------------------
What is a function?
---------------------

- Primary way to organize code into reusable blocks
- Take inputs, process them, and (often) return a value
- Declared using the :rust:`fn` keyword
  - Must be immediately followed by the body enclosed in :rust:`{ }` 
- Typically in **snake_case** (e.g., :rust:`calculate_area`)

.. code:: rust

  fn function_name(parameter_1:Type) -> ReturnType {
    // Function body (statements and expressions)
  }

--------------------------------
Parameters and Type Signatures
--------------------------------

- Function parameters must have their types **explicitly** declared
  - No inference, unlike variable bindings
- Function signature defines
  - Types of data the function accepts (parameters)
  - Type of data it produces (:rust:`-> ReturnType`)

.. code:: rust

  // We must tell the types of both 'first' and 'second'
  fn add(first: i32, second: i32) -> i32 {
    first + second
  }

-----------------------------------------
Return Values (Expression vs Statement)
-----------------------------------------

- Return Type is specified after an arrow (:rust:`->`)
- No :rust:`->` syntax means the function returns the unit type :rust:`()`
- Functions can return in two ways:
  - **Statement:** ends in a semicolon (:rust:`;`), returns :rust:`()`
  - **Expression:** does **not** end in a semicolon 
    - Last expression evaluated in the body is returned

.. code:: rust

  fn get_forty_two() -> i32 {
     // The automatically returned expression
     42
  }

  fn print_and_return_unit() {
     // A statement (ends in ;), returns ()
     println!("Hello!");
  }

-----------------------------
Explicit exit with "return"
-----------------------------

  - Forces the function to exit immediately
    - Bypassing the rest of the code
    - Essential for early exits based on control flow

.. code:: rust

  fn do_things(condition: bool) {
    if condition {
      println!("doing something else!");
      return; // Exits, returns ()
    }
    println!("doing something!");    
  }

- Returning a value for early exit

.. code:: rust

  fn check_age(age: i32) -> bool {
      if age < 0 {
          return false; // Exits immediately
      } 
      age >= 18 // Idiomatic way to return a value
  }

--------------------------------------------
Design Philosophy: Clarity and Precision 
--------------------------------------------

- Overloading is not supported
  - No multiple same-name functions with different arguments
  - You always know exactly which function is called
- No default arguments
  - Callers must provide a value for every parameter
  - You see all the data entering the function
- Fixed number of arguments
  - Take a strict number of inputs
  - *Macros* (like :rust:`(println!)`) can take variable arguments
    - But *functions* cannot
