========
Macros
========

------------------
What is a Macro?
------------------

- Macros are **not** functions
- Expanded into code during compilation
- Can take a **variable** number of arguments
- Distinguished by a :rust:`!` at the end  (e.g., :rust:`println!`, :rust:`dbg!`)
- You can write your own!

.. note::

   Writing custom macros is an advanced, non-trivial topic

----------
println!
----------

.. admonition:: Language Variant

   Standard Library

* :rust:`println!(format, ..)` prints a line to standard output

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

.. admonition:: Language Variant

   Standard Library

* :rust:`dbg!(expression)` logs the value of the expression
 
* Returns the value itself
* Often used for quick, temporary debugging 
  * Prints the file, line number, and the value
  * Can be used inside other expressions
* Works the same in debug and release modes

.. code:: rust

    fn factorial(n: u32) -> u32 {
        let mut product = 1;
        for i in 1..=n {
            product *= dbg!(i); 
        }
        product
    }
    let result = factorial(3); // result will be 6

* Generating the following output:

:command:`[src/main.rs:5:20] i = 1`

:command:`[src/main.rs:5:20] i = 2`

:command:`[src/main.rs:5:20] i = 3`

-------
todo!
-------

.. admonition:: Language Variant

   Standard Library

* :rust:`todo!()` marks a bit of code as not-yet-implemented
* When executed, it immediately causes a **panic**
  * Message indicates the un-implemented code
  * Useful for sketching out function signatures during development
* Works the same in debug and release modes

.. code:: rust

    fn fizzbuzz(n: u32) -> u32 {
        // Calling this will panic with a message like:
        // "not yet implemented: Implement this"
        todo!("Implement this") 
    }

    fn main() {
        fizzbuzz(10); // Will cause the program to crash!
    }

--------------
unreachable!
--------------

.. admonition:: Language Variant

   Standard Library

* :rust:`unreachable!()` marks a bit of code as unreachable
* Marks a point in the code that should never be reached
* When reached, immediately causes a **panic** with a message
* Serves as a sanity check for the programmer

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
