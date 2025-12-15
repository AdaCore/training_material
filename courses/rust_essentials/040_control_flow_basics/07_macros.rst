========
Macros
========

------------------
What is a Macro?
------------------

- Code that **generates** other code at **compile-time**
- Can take a **variable** number of arguments
- Is **not** a function
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

    let name = "World";
    let answer = 41;

    println!("Hello!");

    // Positional arguments
    println!("Hello, {}, the answer is {}.", name, answer + 1);

    // Named argument, improves readability and refactoring
    // Do not allow expressions inside the curly braces
    println!("Hello {name}!");
    println!("Hi {name}, the answer is {rv}.", rv = answer + 1);

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
* Works both the same in **debug** and **release** modes

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
* Works the same in both **debug** and **release** modes

.. code:: rust

    fn fizzbuzz(n: u32) -> u32 {
        todo!("Implement this") // Absence of ; is idiomatic here
    }

    fn main() {
        fizzbuzz(10);
    }

* Generating the following output:

:command:`thread 'main' (11) panicked at src/main.rs:4:5:`
:command:`not yet implemented: Implement this`

--------------
unreachable!
--------------

.. admonition:: Language Variant

   Standard Library

* :rust:`unreachable!()` marks a bit of code as unreachable
* Marks a point in the code that should never be reached
* When reached, immediately causes a **panic** with a message
* Works the same in both **debug** and **release** modes
* Serves as a sanity check for the programmer

.. code:: rust

    let number = 3;
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

* Generating the following output:

:command:`thread 'main' (41) panicked at src/main.rs:12:14:`
:command:`internal error: entered unreachable code: Number is outside the expected range!`

.. container:: speakernote

   The takeaway from this section is that these common conveniences exist,
   and how to use them. Why they are defined as macros, and what they
   expand to, is not especially critical.

   The course does not cover defining macros, but a later section will
   describe use of derive macros.
