===========
Variables
===========

---------------------
What is a Variable?
---------------------

- Think of a variable as a **labeled box** where you can store a single piece of info (a "value")

  - **Label** - the variable's name (e.g., :rust:`score`)
  - **Contents** - the value (e.g., :rust:`100`)

- In Rust, we create a variable with the :rust:`let` keyword
  
  - This **binds** a name to a value

.. code:: rust

  // We are binding the name "apples" to the value 5
  let apples = 5;

  // We are binding the name "person" to the value "Alice"
  let person = "Alice";

-------------------------------------
By Default, Variables are Immutable
-------------------------------------

- **Immutable** = **unchangeable**

- This is a core concept in Rust

  - The **compiler** will generate errors on assignment

  - Safety and reliability principles are built into the language

  - Prevents accidental data assignment (especially in large programs!)

  - :rust:`let` creates an **immutable** binding

.. code:: rust

  // This is OK!
  let my_var = 10;

  // This will cause an ERROR! We can't change the value
  my_var = 20;

--------------------------
Making Variables Mutable
--------------------------

- Sometimes, you *need* to change a value

- Rust requires **explicit** permission to do this

- :rust:`mut` - tells Rust the varaible is **mutable**

  - Add it to the declaration
  - The keyword follows :rust:`let`

.. code:: rust

  let mut change_me = 5;
  println!("change_me is: {change_me}");

  // This is now perfectly allowed!
  change_me = 6;
  println!("change_me is now: {change_me}");

.. note::

  In Rust, mutability is an **opt-in** choice
