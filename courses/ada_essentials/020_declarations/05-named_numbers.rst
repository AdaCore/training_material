===============
Named Numbers
===============

---------------
Named Numbers
---------------

* Associate an **identifier** with an **expression**

   - Used as **constant**
   - `universal_integer`, or `universal_real`
   - Compatible with integer / real respectively
   - Expression must be **static**

* Syntax

   .. code:: Ada

     <constant_name> : constant := <static_expression>;

   where *<constant_name>* is an identifier

* Example

   .. code:: Ada

      Pi : constant := 3.141592654;
      One_Third : constant := 1.0 / 3.0;

----------------------
Named Number Benefit
----------------------

* Evaluation at **compile time**

    - As if **used directly** in the code

.. tip:: Useful due to their **perfect** accuracy

.. code:: Ada

   Named_Number   : constant :=       1.0 / 3.0;
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

