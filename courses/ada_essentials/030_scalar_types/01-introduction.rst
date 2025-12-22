================
Introduction
================

----------------
Discrete Types
----------------

* **Individual** ("discrete") values

  * Can easily identify next/previous value

* Integer types

  - Signed integer types

    .. code:: Ada

      type Integer_T is range -1 .. 1000;
      Number : Integer_T := 123;

  - Modular integer types

    * No sign bit
    * **Wrap-around** semantics
    * Bitwise operations

    .. code:: Ada

      type Unsigned_T is mod 256;
      Unsigned : Unsigned_T := 123;
      Bitwise  : Unsigned_T := Unsigned and 16#55#;

* Enumeration types

  - Ordered list of **logical** values

  .. code:: Ada

    type Enumeration_T is (Red, Yellow, Green);
  
------------
Real Types
------------

* :dfn:`Floating-point` numbers have **variable** exponent portion

  * Allows for a very wide range of values

* :dfn:`Fixed-point` numbers have a **constant** exponent portion

  * Allows for simpler (integer-based) computer math

.. code:: Ada

  type Float_T is digits 6;
  type Fixed_T is delta 0.01;
