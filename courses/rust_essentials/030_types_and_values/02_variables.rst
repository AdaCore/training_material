===========
Variables
===========

---------------------
What Is a Variable?
---------------------

- Like a **labeled box** that stores a single piece of information ("value")

  - **Label** - variable's name (e.g., :rust:`score`)
  - **Contents** - value (e.g., :rust:`100`)

- Create a variable with the :rust:`let` keyword
  
  - This **binds** a name to a value

.. code:: rust

  // Bind 'apples' to the value 5
  let apples = 5;

  // Bind 'person' to the value 'Alice'
  let person = "Alice";

-------------------------------------
By Default, Variables Are Immutable
-------------------------------------

- **Immutable** = **unchangeable**

- This is a core concept

  - The **compiler** will generate errors on assignment

  - Safety and reliability principles are built into the language

  - Prevents accidental data assignment (especially in large programs!)

  - :rust:`let` creates an **immutable** binding

.. code:: rust

  // This is OK!
  let my_var = 10;

  // This will cause an ERROR! We can't change the value
  my_var = 20;

.. code:: error
  :font-size: scriptsize

  error[E0384]: cannot assign twice to immutable variable 'my_var'

--------------------------
Making Variables Mutable
--------------------------

- Sometimes, a value *needs* to change

- Variables are immutable unless mutability is declared **explicitly**

- :rust:`mut` - specifies the variable is **mutable**

  - Add it to the declaration
  - The keyword follows :rust:`let`

.. code:: rust

  let mut change_me = 5;
  println!("change_me is: {change_me}");

  // This is now perfectly allowed!
  change_me = 6;
  println!("change_me is now: {change_me}");

.. code:: output

  change_me is: 5
  change_me is now: 6

.. note::

  Mutability is an **opt-in** choice
