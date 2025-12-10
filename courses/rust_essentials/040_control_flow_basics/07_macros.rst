========
Macros
========

--------
Macros
--------

- Macros are **not** functions
- Expanded into code during compilation
- Can take a **variable** number of arguments
- Distinguished by a :rust:`!` at the end  (e.g., println!, dbg!)
- You can write your own macros

----------
println!
----------
  - :rust:`println!(format, ..)` prints a line to standard output
    - Included in the standard library

.. code:: rust

    fn main() {
        let name = "World";
        let answer = 42;

        // Basic text output
        println!("Hello!");

        // Output with positional argument
        println!("Hello, {}!", name);

        // Output with named argument
        println!("The answer to life is {answer}.");
    }

------
dbg!
------
  - :rust:`dbg!(expression)` logs the value of the expression and returns it
    - Included in the standard library

  - Logs the value of an expression and returns the value itself
  - Often used for quick, temporary debugging 
    - Prints the file, line number, and the value
    - Can be used inside other expressions

.. code:: rust

    fn factorial(n: u32) -> u32 {
        let mut product = 1;
        for i in 1..=n {
            // Prints: [src/main.rs:5] i = 3
            product *= dbg!(i); 
        }
        product
    }

    let result = factorial(3);
    // result will be 6

-------
todo!
-------
  - :rust:`todo!()` marks a bit of code as not-yet-implemented
    - Included in the standard library
  - When executed, it immediately causes a **panic**
    - With a message indicating the un-implemented code 
  - Useful for sketching out function signatures during development

.. code:: rust

    fn fizzbuzz(n: u32) -> u32 {
        // Calling this function will panic with a message like:
        // "not yet implemented: Implement this"
        todo!("Implement this") 
    }

    fn main() {
        // This will cause the program to crash!
        fizzbuzz(10);
    }

--------------
unreachable!
--------------
  - :rust:`unreachable!()` marks a bit of code as unreachable
    - Included in the standard library
  - Marks a point in the code that should never be reached
  - When reached, immediately causes a panic with a message
  - Serves as a sanity check for the programmer

.. code:: rust

    let number = 1;
    match number {
        // The match is exhaustive for a u32, but in this context,
        // we logically know 'number' will only be 1 or 2.
        1 => println!("One"),
        2 => println!("Two"),
        
        // The underscore _ matches all other possible values.
        // If we assume 'number' is only ever 1 or 2, this arm 
        // should logically be unreachable.
        _ => unreachable!("Number is outside the expected range!"), 
    }
.. container:: speakernote

   The takeaway from this section is that these common conveniences exist,
   and how to use them. Why they are defined as macros, and what they
   expand to, is not especially critical.

   The course does not cover defining macros, but a later section will
   describe use of derive macros.
