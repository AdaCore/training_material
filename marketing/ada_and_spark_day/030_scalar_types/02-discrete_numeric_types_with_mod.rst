========================
Discrete Numeric Types
========================

----------------------
Signed Integer Types
----------------------

**Syntax**

.. code:: bnf

  signed_integer_type_definition ::= 
     type identifier is
        range static_simple_expression .. static_simple_expression;

* Range of signed **whole** numbers

   - Symmetric about zero (-0 = +0)

* Implicit numeric operators

  .. code:: Ada

     -- 12-bit device
     type Analog_Conversions is range 0 .. 4095;
     Count : Analog_Conversions := 0;
     ...
     begin
        ...
        Count := Count + 1;
        ...
     end;

---------------
Modular Types
---------------

**Syntax**

.. code:: bnf

  modular_type_definition ::=
     type identifier is mod static_expression;

* Integer type
* **Unsigned** values
* Adds operations and attributes

.. note:: Typically **bit-wise** manipulation

* Modulus must be **static**
* Resulting range is  :ada:`0 .. modulus - 1`

  .. code:: Ada

     type Unsigned_Word is mod 2**16; -- 16 bits, 0..65535
     type Byte is mod 256;            -- 8 bits, 0..255
