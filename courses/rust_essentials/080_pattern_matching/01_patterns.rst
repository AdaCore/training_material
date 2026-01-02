=================
Patterns
=================

---------------------------
The Language of Structure
---------------------------

- Patterns are a special syntax used to match against the **structure** of types

- First-class citizens of Rust, appearing in

    - Variable bindings
    - Function arguments
    - Loops

- A pattern either **matches** or **fails to match**

    - Compiler can ensure code is safe...
    - ...and exhaustive

----------------------------------
Every Binding is a Pattern Match
----------------------------------

- The syntax for creating a variable is actually

.. code:: rust

    let <PATTERN> = <VALUE>;

- **Irrefutable Patterns**: standard :rust:`let` statement requires a pattern that **cannot fail** to 
    match the right-hand side

.. code:: rust

    let x = 5; // variable name - pattern "x" matches any value and binds it

    let (x, y) = (1, 2); // tuples - pattern destructures tuple into 2 separate Variables

    let Foo { x, .. } = my_foo; // structs - pattern extracts specific fields directly

.. note::

    Because :rsut:`let` is a pattern match, it must be **guaranteed** to work
