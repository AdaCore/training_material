============
Arithmetic
============

--------------------
Standard Operators
--------------------

- Rust standard arithmetic operators, in order of precedence (highest to lowest)

.. list-table::
   :widths: 30 5 5 5 55
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

.. code:: rust

  let sum = 5 + 10;            // 15
  let difference = 95.5 - 4.3; // 91.2
  let product = 4 * 30;        // 120

  // Integer division truncates (rounds down)
  let quotient = 7 / 3;        // 2 (not 2.33...)

  let remainder = 7 % 3;       // 1

-------------------
The Exponent Trap
-------------------

- **No** operator for **exponent** (i.e., **power**)!

- **Common Mistake:** using :rust:`^`

  - In Rust, :rust:`^` is the **Bitwise XOR operator**
  - Code will compile, but your math will be wrong!

.. code:: rust

  let wrong = 5 ^ 2;  // Result is 7 (binary 101 XOR 010)

- **Correct Way:** use methods

  - Must use a method specific to your data type
  - :rust:`.pow(u32) ` - integers
  - :rust:`.powf(f64)` - floats

.. code:: rust

  5_i32.pow(2)      // result = 25
  5.0_f64.powf(2.5) // result ~55.9

.. note::

  Integer :rust:`pow()` requires a :rust:`u32` exponent to 
  prevent negative exponents returning non-integers

------------------------------
Modifying Variables In-Place
------------------------------

- Increment (:rust:`++`) and decrement (:rust:`--`) operators don't exist in Rust

  - *Why?* - they can lead to confusing code, and Rust prefers *clarity*

- **The Alternative:** Compound Assignment

  - Use the standard "shortcut" operators to do math AND update at once! 
  - This is the idiomatic way to increment counters in Rust

.. list-table::
   :widths: 15 30 35 20
   :header-rows: 1

   * - Operator
     - Expanded Meaning
     - Example
     - Result\*

   * - **+=**
     - :rust:`x = x + y`
     - :rust:`x += 1;`
     - :rust:`11`

   * - **-=**
     - :rust:`x = x - y`
     - :rust:`x -= 5;`
     - :rust:`5`

   * - **\*=**
     - :rust:`x = x * y`
     - :rust:`x *= 2;`
     - :rust:`20`

   * - **/=**
     - :rust:`x = x / y`
     - :rust:`x /= 2;`
     - :rust:`5`

   * - **%=**
     - :rust:`x = x % y`
     - :rust:`x %= 3;`
     - :rust:`1`

**\* Assume** :rust:`x` **starts at 10**

-----------------------------
Arithmetic Nuance: Division
-----------------------------

- The :rust:`/` operator behaves differents depending on the type

  - **Integer Division - Truncation**

    - When you divide two **integers**, the result is *always* an **integer**
    - The decimal is **truncated** (cut off), not rounded

.. code:: rust

  let truncated = 7 / 3;      // Result is 2 (not 2.33...)
  let also_truncated = 1 / 2; // Result is 0 (not 0.5)

..

  - **Floating Point Division**

    - To get a **decimal** result, you *must* use **floating point** numbers
    - :rust:`f64`, :rust:`f32`

.. code:: rust

  let precise = 7.0 / 3.0;  // Result is 2.333...

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

    - Rust *does not* :rust:`panic!`
    - It performs **two's complement wrapping**
    - **Example:** For :rust:`u8`, :rust:`255 + 1` "wraps around" to :rust:`0`

------------------------------
Handling Overflow Explicitly
------------------------------

- What if *you* want to control overflow behavior?

- :rust:`wrapping_add()` - Performs wrapping in all modules

- :rust:`saturating_add()` - Clamps the value at the type's *maximum* or *minimum*

- :rust:`overflowing_add()` - returns the value AND a :rust:`bool` indicating if overflow happened

.. code:: rust
  127_i8.wrapping_add(1)     // results in -128

  120_i8.saturating_add(20)  // results in 127 (max i8 value)

  100_i8.overflowing_add(50) // results in (-106, true)

.. warning::

  You should **not** rely on wrapping if you expect a calculation overflow