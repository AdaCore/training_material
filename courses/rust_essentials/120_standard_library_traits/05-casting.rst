=========
Casting
=========

-----------------------------------------
Conversion Between Numbers and Pointers
-----------------------------------------

* Numeric Casting

  * Used to convert between primitive types (e.g., :rust:`my_u8 as u32`)

  * **Safe:** When "widening" (moving to a larger memory space)

  * **Risky:** When "narrowing" (data may be lost)

* Pointer Casting

  * Allows converting between references and raw pointers
  * *Note: Usually reserved for low-level systems programming*

* Key Difference

  * Unlike :rust:`From`, this does not use traits

    * It is a built-in language "force-move"

.. note::

  Rust has no *implicit* conversion - explicit casts use :rust:`as`

------------------
Casting Examples
------------------

.. code:: rust

  fn main() {
      let value: i64 = 1000;
      println!("as u16: {}", value as u16);
      println!("as i16: {}", value as i16);
      println!("as u8: {}", value as u8);
  }

**produces**

  :command:`as u16: 1000`

  :command:`as i16: 1000`

  :command:`as u8: 232`

-------------------------
Be Careful with Casting
-------------------------

* Casting truncates using **bitmasking** - keeps the lower bits

* :rust:`From` and :rust:`Into` are generally safer

  * Also use :rust:`TryFrom` and :rust:`TryInto` when you're not sure about the result

* Truncation (generally) only works with primitive-like types

  * Casting :rust:`Struct` and :rust:`String` generate compiler errors

.. warning::

  Casting might not do what you expect!

  :rust:`enum` and pointers |rightarrow| keeps lower bits

  Slice (*fat pointer*) |rightarrow| length of slice is truncated!

-----------------------
Conversion vs Casting
-----------------------

.. container:: latex_environment footnotesize

  .. list-table::
     :header-rows: 1

    * - **Method**
      - **Safety Level**
      - **Best Use Case**

    * - :rust:`From` / :rust:`Into`
      - Guaranteed
      - Lossless conversion (e.g., :rust:`&str` to :rust:`String`)

    * - :rust:`TryFrom` / :rust:`TryInto`
      - Checked
      - Conversions that might fail (e.g., :rust:`u32` to :rust:`u8`)

    * - :rust:`as` (Widening)
      - Safe
      - Moving to a larger type (e.g., :rust:`u8` to :rust:`u32`)

    * - :rust:`as` (Narrowing)
      - Dangerous
      - Only when you **want** to truncate bits

.. tip::

  *The Developer's Rule*

  * **Prefer Traits:** Use :rust:`Into` or :rust:`TryInto` for clarity and safety
  * **Avoid "Magic Numbers":** Don't use :rust:`as` to force a conversion

    * It might hide a bug!
