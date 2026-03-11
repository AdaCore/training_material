==================
Type Conversions
==================

-----------------
Type Conversion
-----------------

* Conversion traits

  * :rust:`From<T>` - converts a value into :rust:`Self`
  * :rust:`Into<T>` - converts :rust:`Self` into a value
  * :rust:`Into` can be inferred when  :rust:`From` implemented

* Why it matters

  * Standardized way to convert between types with consistent syntax
  * Used extensively in APIs for ergonomic type transformations

* These methods are transformative

  * When you call :rust:`.into()`, the original target now "owns" the value

* What happens to the source value?

  * If source type implements :rust:`Copy`, source is still valid
  * Otherwise, the value is no longer valid

.. note::

  Conversion is *lossless* - it cannot fail (:dfn:`infallible conversion`)

---------------------
Conversion Examples
---------------------

* :rust:`From`

  * Uses source type for conversion

  .. code:: rust

    let a_string = String::from("hello");
    let addr = std::net::Ipv4Addr::from([127, 0, 0, 1]);
    let one = i16::from(true);
    let bigger = i32::from(123_i16);

* :rust:`Into`

  * Uses target type for conversion

  .. code:: rust

    let a_string: String = "hello".into();
    let addr: std::net::Ipv4Addr = [127, 0, 0, 1].into();
    let one: i16 = true.into();
    let bigger: i32 = 123_i16.into();

------------------
"From" vs "Into"
------------------

* If you implement :rust:`From`, you automatically get :rust:`Into`

  * So it is a good idea to always implement :rust:`From` for your types
  * Also referred to as the *Conversion Rule*

* :rust:`From` specifies both source and destination

  .. code:: rust

    let result = Feet::from(measure);

  * :rust:`result` is in :rust:`Feet`
      
* :rust:`Into` is called on an object

  * Compiler might not know the source
  * Need to supply hints

  .. code:: rust

    // What do we want 'target' to be?
    let target = source.into(); 

    // You have to help it:
    let target: Feet = source.into();

.. note::

  Conversions become very powerful when writing generic functions

------------------------------------
Conversion Between Primitive Types
------------------------------------

* Convert between primitive types with :rust:`as`

  :rust:`my_u8 as u32`

  * Rust has no *implicit* casting

* Unlike :rust:`From`, casting does not use traits

  * It is a built-in language "force-move"

* Casting truncates using **bitmasking** - keeps the lower bits

  * :rust:`enum` and pointers keeps lower bits
  * :rust:`From` and :rust:`Into` are generally safer

* Truncation (generally) only works with primitive-like types

  * Casting :rust:`Struct` and :rust:`String` generate compiler errors
  * Slice (*fat pointer*) truncate length of slice

------------------
Casting Examples
------------------

.. code:: rust

   let value: i64 = 1000;
   println!("value as i16: {}", value as i16);
   println!("value as u8: {}", value as u8);
   let signed_value = -136;
   println!("signed_value as u16: {}", signed_value as u16);

:command:`value as i16: 1000       `  *no lost bits*

:command:`value as u8: 232       `    *lost higher order bits*

:command:`signed_value as u16: 65400` *lost sign bit!*

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

  * :rust:`Into` or :rust:`TryInto` for clarity and safety
  * Don't use :rust:`as` to force a conversion
