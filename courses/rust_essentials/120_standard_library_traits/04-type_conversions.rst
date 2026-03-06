==================
Type Conversions
==================

----------------------
Safe Type Conversion
----------------------

* Conversion traits

  * :rust:`From<T>` - converts a value into :rust:`Self`
  * :rust:`Into<T>` - converts :rust:`Self` into a value
  * Automatically works both ways if :rust:`From` is implemented

* Why it matters

  * Standardized way to convert between types with consistent syntax
  * Used extensively in APIs for ergonomic type transformations

* The "Conversion" Rule

  * These methods are transformative
  * When you call :rust:`.into()`, the original variable is "spent"

    * This is a **transfer** of the value, not a copy

.. note::

  **Safe** means *lossless* - conversion cannot fail

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

.. note::

  **Conversion Rule:** You are "upgrading" your type.

  *Example:* from a string literal ("hello") to a :rust:`String`

------------------
"From" vs "Into"
------------------

* If you implement :rust:`From`, you automatically get :rust:`Into`

  * So it is a good idea to always implement :rust:`From` for your types

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
