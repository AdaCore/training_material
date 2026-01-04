=======================
Destructuring Structs
=======================

-------------------------------
Getting Exactly What You Need
-------------------------------

- **Unpacking** - patterns allow you to "break open" a struct and create variables from its fields in one step

- **Shorthand** - if you want a varaible name to match the field name, just list it

    .. code:: rust

        let Food { x, y } = foo;

- **Partial Matching** - use :rust:`..` to ignore remaining fields

    .. code:: rust

        // captures y and throws away the other fields
        Foo { y, ..}

.. note::

    This is cleaner than writing :rust:`let y = foo.y` for five different fields!

--------------------------
Advanced Struct Patterns
--------------------------

- **Renaming** - can decouple internal logic from struct's field names by assigning a field's value to a 
    new varaible name

- **Literal Constraints** - can write patterns that **only** match if a field contains a specific, hardcoded value

- **Nesting** - can destructure deeper structures (like a tuple in a struct) in a single pattern match

- **"Ignore" Operator** - use :rust:`..` to tell Rust you only care about a subset of the data

.. code:: rust

    struct Foo { x: (u32, u32), y: u32 }

    fn main() {
        let foo = Foo { x: (1, 100), y: 3 };

        match foo {
            // Literal + Nesting: Matches only if x.0 is 1. 
            // Binds x.1 to 'val' and y to 'y'.
            Foo { x: (1, val), y } => println!("Matched! val: {val}, y: {y}"),

            // Renaming: Binds the value of field 'y' to a new variable 'level'.
            // Ignores the 'x' field entirely using '..'.
            Foo { y: level, .. } => println!("Y is {level}"),
            
            // Catch-all: If none of the above specific structures match.
            _ => println!("No match found"),
        }
    }
