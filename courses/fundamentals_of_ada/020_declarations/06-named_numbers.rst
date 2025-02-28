===============
Named Numbers
===============

---------------
Named Numbers
---------------

* Associate a **name** with an **expression**

   - Used as **constant**
   - `universal_integer`, or `universal_real`
   - Compatible with integer / real respectively
   - Expression must be **static**

* Syntax

   .. code:: Ada

     <name> : constant := <static_expression>;

* Example

   .. code:: Ada

      Pi : constant := 3.141592654;
      One_Third : constant := 1.0 / 3.0;

--------------------------------------
A Sample Collection of Named Numbers
--------------------------------------

.. code:: Ada

   package Physical_Constants is
     Polar_Radius : constant := 20_856_010.51;
     Equatorial_Radius : constant := 20_926_469.20;
     Earth_Diameter : constant :=
       2.0 * ((Polar_Radius + Equatorial_Radius)/2.0);
     Gravity : constant := 32.1740_4855_6430_4;
     Sea_Level_Air_Density : constant :=
       0.002378;
     Altitude_Of_Tropopause : constant := 36089.0;
     Tropopause_Temperature : constant := -56.5;
   end Physical_Constants;

----------------------
Named Number Benefit
----------------------

* Evaluation at **compile time**

    - As if **used directly** in the code

.. tip:: They have **perfect** accuracy

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

