============
Arithmetic
============

--------------------
Standard Operators
--------------------

- Rust standard arithmetic operators, in order of precedence (highest to lowest)

.. list-table::
   :widths: 20 5 5 5 65
   :header-rows: 0

   * - **Multiplicative**
     - **\***
     - **/**
     - **%**
     -
   * - **Additive**
     - **+**
     - **-**
     -
     -

.. note::

    Rust does not have the :rust:`++` or :rust:`--` increment/decrement operators

.. code:: rust

  let sum = 5 + 10;            // 15
  let difference = 95.5 - 4.3; // 91.2
  let product = 4 * 30;        // 120

  // Integer division truncates (rounds down)
  let quotient = 7 / 3;        // 2 (not 2.33...)

  let remainder = 7 % 3;       // 1

-----------------------------
Arithmetic Nuance: Division
-----------------------------

- The :rust:`/` operator behaves differents depending on the type

  - **Integer Division - Truncation**

    - When you divide two **integers**, the result is *always* an **integer**
    - The decimal is **truncated** (cut off), not rounded

.. code:: rust

  let a = 7 / 3;    // Result is 2 (not 2.33...)
  let b = 1 / 2;    // Result is 0 (not 0.5)

..

  - **Floating Point Division**

    - To get a **decimal** result, you *must* use **floating point** numbers
    - :rust:`f64`, :rust:`f32`

.. code:: rust

  let a = 7.0 / 3.0;  // Result is 2.333...

------------------
Integer Overflow
------------------

- What happens if a number gets too big for its type?

.. code:: rust

  // u8 can only hold values from 0 to 255
  let my_byte: u8 = 250;
  let new_byte = my_byte + 10; // 260? This won't fit!

- Rust's safe, defined behavior is to

  - **Debug Builds**

    - Rust *checks* for overflow
    - Your program will :rust:`panic!` (crash)
    - An error will tell you exactly what happened

  - **Release Builds**

    - Rust *does not* :rust:`panic!` (for speed)
    - It performs **two's complement wrapping**
    - **Example:** For :rust:`u8`, :rust:`255 + 1` "wraps around" to :rust:`0`

------------------------------
Handling Overflow Explicitly
------------------------------

- What if *you* want to control overflow behavior?

  - Rust provides explicit methods so your intent is clear

- You should **not** rely on wrapping if you expect a calculation overflow

- :rust:`wrapping_add()` - Performs wrapping in all Modules

  - :rust:`127_i8.wrapping_add(1)` results in :rust:`-128`

- :rust:`saturating_add()` - Clamps the value at the type's maximum or minimum

  - :rust:`120_i8.saturating_add(20)` results in :rust:`127` (the max :rust:`i8`)

- :rust:`overflowing_add()` - returns the value AND a :rust:`bool` indicating if overflow happened

  - :rust:`100_i8.overflowing_add(50)` results in :rust:`(-106, true)`
