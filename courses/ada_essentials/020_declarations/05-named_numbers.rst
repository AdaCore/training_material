===============
Named Numbers
===============

---------------
Named Numbers
---------------

**Syntax**

.. container:: source_include 020_declarations/syntax.bnf :start-after:named_numbers_begin :end-before:named_numbers_end :code:bnf

* Associate an **identifier** with a **mathematical expression**

   - Used as **constant**
   - Compatible with integer / real
   - Expression must be **static**

* Examples

   .. code:: Ada

      Pi : constant := 3.141592654;
      One_Third : constant := 1.0 / 3.0;
      Radians_In_Circle : constant := 2.0 * Pi;

----------------------
Named Number Benefit
----------------------

* Named numbers are exact — they’re not limited by a type’s range or precision

* Evaluation at **compile time**

.. code:: Ada

   Named_Number   : constant       := 1.0 / 3.0;
   Typed_Constant : constant Float := 1.0 / 3.0;

.. container:: latex_environment footnotesize

  .. list-table::
    :header-rows: 1

    * - Object
      - Named_Number
      - Typed_Constant

    * - :ada:`F32 : Float_32;`
      - 3.33333E-01
      - 3.33333E-01

    * - :ada:`F64 : Float_64;`
      - 3.33333333333333E-01
      - 3.333333_43267441E-01

    * - :ada:`F128 : Float_128;`
      - 3.33333333333333333E-01
      - 3.333333_43267440796E-01

