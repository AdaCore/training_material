=======
Types
=======

--------------------------
Rust is Statically Typed
--------------------------

- One of the most important features

- Compiler **must** know the exact **type** of every variable 
at compile time

- How Rust provides **type safety** (and prevents bugs!)

.. code:: rust

  // We are explicitly telling Rust:
  // "x is a 32-bit signed integer with the value 10"
  let x: i32 = 10;

----------------
Assigning Types
----------------

- Two ways to tell Rust what **type** a variable is

  - **Explicit Annotation**

    .. code:: rust

      let x: i32 = 10;

  - **Type Inference**

    .. code:: rust

      let y = 10;

--------------------------
Type Inference Explained 
--------------------------

- In most cases, you don't need to write the type

- Rust will **infer** it based on the value you give it

  - This is why :rust:`let apples = 5` worked in our earlier example!

- **Default Rules**

  - Integers (whole numbers) default to :rust:`i32`
  - Floating-point (decimals) default to :rust:`f64`

.. code:: rust

  fn main() {
      // Rust sees a whole number and infers i32
      let x = 10;
      // This is the same as writing:
      // let x: i32 = 10;

      // Rust sees a decimal and infers f64
      let y = 2.5;
      // This is the same as writing:
      // let y: f64 = 2.5;
  }

--------------------------
Inference is Smart
--------------------------

- Type inference isn't just about defaults...but also *how you use* a variable

- A variable's type might not be known until later in the function

.. code:: rust

--------------------------
Inference is Smart
--------------------------

- Type inference isn't just about defaults...but also *how you use* a variable

- A variable's type might not be known until later in the function

.. code:: rust

  fn main() {
      // Rust sees 10, but waits to decide the type...
      let x = 10;

      // We declare `y` as an explicit u32
      let y: u32;

      // Rust looks back and decides `x` MUST be u32
      y = x;
  }

--------------
Common Types
--------------

.. container:: latex_environment scriptsize

  .. list-table::
    :header-rows: 1

    * -
      - Types
      - Literals

    * - Signed integers
      - :rust:`i8`, :rust:`i16`, :rust:`i32`, 
        :rust:`i64`, :rust:`i128`, :rust:`isize`
      - :rust:`-10`, :rust:`0`, :rust:`1_000`, :rust:`123_i64`

    * - Unsigned integers
      - :rust:`u8`, :rust:`u16`, :rust:`u32`, :rust:`u64`, 
        :rust:`u128`, :rust:`usize`
      - :rust:`0`, :rust:`123`, :rust:`10_u16`

    * - Floating point numbers
      - :rust:`f32`, :rust:`f64`
      - :rust:`3.14`, :rust:`-10.0e20`, :rust:`2_f32`

    * - Unicode scalar values
      - :rust:`char`
      - :rust:`'a'`, ':math:`\alpha`', ':math:`\infty`'

    * - Booleans
      - :rust:`bool`
      - :rust:`true`, :rust:`false`

The types have widths as follows:

-  :rust:`iN`, :rust:`uN`, and :rust:`fN` are *N* bits wide
-  :rust:`isize` and :rust:`usize` are the width of a pointer
-  :rust:`char` is 32 bits wide
-  :rust:`bool` is 8 bits wide

-------------------------
Numeric Literal Formats 
-------------------------

- Rust provides a few ways to make values easier to read

- **Numeric Readability**

  - Underscores can be used in numbers for legibility
  - These are ignored by the compiler

  - :rust:`1_000` is the same as :rust:`1000` (or :rust:`10_00`)

- **Type Suffixes**

  - You can add the type *directly* to the numeric literal
  - This is another way to tell Rust the type without a full annotation

    .. code:: rust

      // These three bindings are identical: 
      let a: i64 = 123;  // Full annotation 
      let b = 123_i64;   // Type suffix 
      let c = 123i64;    // Suffix (no underscore)

---------------------------
Utilizing Different Bases 
---------------------------

- Integers can be expressed in different bases

- They all represent the same value to the computer 

.. list-table::
   :header-rows: 1

   * - Base
     - Syntax
     - Example

   * - Decimal
     - Standard
     - :rust:`98_222`

   * - Hex
     - :rust:`0x`
     - :rust:`0xff`

   * - Octal
     - :rust:`0o`
     - :rust:`0o77`

   * - Binary
     - :rust:`0b`
     - :rust:`0b1111_0000`

   * - Byte
     - :rust:`b` (:rust:`u8` only)
     - :rust:`b'A'`

--------------------
Numeric Strictness
--------------------

- Rust does **not** automatically convert types for you

.. code:: rust

  let my_int: i32 = 10;
  let my_float: f64 = 5.5;

  // will not compile!
  let sum = my_int + my_float;

  // "as" tells the compiler to interpret my_int as f64
  let sum = my_int as f64 + my_float;

.. tip::

  Rust forces you to be **intentional**. Applying
  :rust:`as` to a variable makes you think before doing.

----------------------------
The "char" Type is Special
----------------------------

- :rust:`char` is **4 bytes** in Rust (as opposed to 1 byte in other languages)

- It can hold almost any character from any language (including emojis!)

- Use single quotes for a :rust:`char`

.. code:: rust

  fn main() {
      let letter: char = 'a';
      let accented: char = 'é';
  }

-------------------------
Conditions Must be bool
-------------------------

- Some languages use numbers (0, 1) to express boolean values (false, true)

- Rust is strict: an :rust:`if` statement **must** be given a true :rust:`bool` value

  - Numbers are not allowed

- This code will **not** compile 

  .. code:: rust

    fn main() {
      let score = 10;

      // ERROR: expected `bool`, found integer
      // This is not valid Rust!
      if score {
          println!("You win!");
      }
    }

- This code is valid

  .. code:: rust

    fn main() {
      let score = 10;

      // CORRECT:
      // The expression `score > 0` evaluates to `true`
      if score > 0 {
          println!("You win!");
      }
    }

------------------------------
Recap: Anatomy of a Variable
------------------------------

.. code:: rust

  // This is a *mutable*, *explicitly typed* variable binding

  let mut x: i32 = 10;
  |   |   |  |    |
  |   |   |  |    +---  **Value** (a number literal)
  |   |   |  +--------  **Type** (a 32-bit signed integer)
  |   |   +------------ **Variable Name**
  |   +---------------- **Mutable** (makes it changeable)
  +-------------------- **Keyword** to declare a variable

**Key Takeaways:**

- :rust:`let`   - creates a variable
- :rust:`mut`   - *optional*, but makes it changeable
- :rust:`: i32` - *optional* because Rust can infer it