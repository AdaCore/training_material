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

---------
Example
---------

* :rust:`From`

  * Uses source type for conversion

  .. code:: rust

    let s = String::from("hello");
    let addr = std::net::Ipv4Addr::from([127, 0, 0, 1]);
    let one = i16::from(true);
    let bigger = i32::from(123_i16);

* :rust:`Into`

  * Uses target type for conversion

  .. code:: rust

    let s: String = "hello".into();
    let addr: std::net::Ipv4Addr = [127, 0, 0, 1].into();
    let one: i16 = true.into();
    let bigger: i32 = 123_i16.into();

