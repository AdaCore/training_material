**************
Record Types
**************

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

---------------------
Syntax and Examples
---------------------

* Syntax (simplified)

   .. code:: Ada

      type T is record
         Component_Name : Type [:= Default_Value];
         ...
      end record;

      type T_Empty is null record;

* Example

   .. code:: Ada

      type Record1_T is record
         Component1 : integer;
         Component2 : boolean;
      end record;

* Records can be **discriminated** as well

   .. code:: Ada

      type T (Size : Natural := 0) is record
         Text : String (1 .. Size);
      end record;

-----------------------------------------
"Dot" Notation for Components Reference
-----------------------------------------

.. code:: Ada

   type Months_T is (January, February, ..., December);
   type Date is record
      Day : Integer range 1 .. 31;
      Month : Months_T;
      Year : Integer range 0 .. 2099;
   end record;
   Arrival : Date;
   ...
   Arrival.Day := 27;  -- components referenced by name
   Arrival.Month := November;
   Arrival.Year := 1990;

* Can reference nested components

.. code:: Ada

   Employee
      .Birth_Date
        .Month := March;

============
Aggregates
============

------------
Aggregates
------------

* Literal values for composite types

   - As for arrays
   - Default value / selector: :ada:`<>`, :ada:`others`

* Can use both **named** and **positional**

    - Unambiguous

* Example:

   .. code:: Ada

      (Pos_1_Value,
       Pos_2_Value,
       Component_3 => Pos_3_Value,
       Component_4 => <>,
       others => Remaining_Value)

---------------------------
Record Aggregate Examples
---------------------------

.. code:: Ada

   type Color_T is (Red);
   type Car_T is record
      Color    : Color_T;
      Plate_No : String (1 .. 6);
      Year     : Natural;
   end record;
   type Complex_T is record
      Real      : Float;
      Imaginary : Float;
   end record;

.. code:: Ada

   declare
      Car   : Car_T     := (Red, "ABC123", Year => 2_022);
      Phase : Complex_T := (1.2, 3.4);
   begin
      Phase := (Real => 5.6, Imaginary => 7.8);
   end;

================
Default Values
================

--------------------------
Component Default Values
--------------------------

.. code:: Ada

   type Complex is
     record
       Real : Float := 0.0;
       Imaginary : Float := 0.0;
     end record;
   -- all components use defaults
   Phasor : Complex;
   -- all components must be specified
   I : constant Complex := (0.0, 1.0);

-----------------------------------
Defaults Within Record Aggregates
-----------------------------------

* Specified via the :dfn:`box` notation
* Value for the component is thus taken as for a stand-alone object declaration

   - So there may or may not be a defined default!

* Can only be used with "named association" form

   - But can mix forms, unlike array aggregates

.. code:: Ada

   type Complex is
     record
       Real : Float := 0.0;
       Imaginary : Float := 0.0;
     end record;
   Phase := (42.0, Imaginary => <>);
