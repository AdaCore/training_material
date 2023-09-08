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
         Field1 : integer;
         Field2 : boolean;
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

--------------------
Named Associations
--------------------

* **Any** order of associations
* Provides more information to the reader

   - Can mix with positional

* Restriction

   - Must stick with named associations **once started**

.. code:: Ada

   type Complex is record
       Real : Float;
       Imaginary : Float;
     end record;
   Phase : Complex := (0.0, 0.0);
   ...
   Phase := (10.0, Imaginary => 2.5);
   Phase := (Imaginary => 12.5, Real => 0.212);
   Phase := (Imaginary => 12.5, 0.212); -- illegal

.. container:: speakernote

   No positional notation after named notation

-------------------
Nested Aggregates
-------------------

.. code:: Ada

  type Months_T is (January, February, ..., December);
  type Date is record
     Day   : Integer range 1 .. 31;
     Month : Months_T;
     Year  : Integer range 0 .. 2099;
  end record;
  type Person is record
     Born : Date;
     Hair : Color;
  end record;
  John : Person    := ((21, November, 1990), Brown);
  Julius : Person  := ((2, August, 1995), Blond);
  Heather : Person := ((2, March, 1989), Hair => Blond);
  Megan : Person   := (Hair => Blond,
                       Born => (16, December, 2001));

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

------------------------------------
Default Component Value Evaluation
------------------------------------

* Occurs when object is elaborated

   - Not when the type is elaborated

* Not evaluated if explicitly overridden

.. code:: Ada

   type Structure is
     record
       A : Integer;
       R : Time := Clock;
     end record;
   -- Clock is called for S1
   S1 : Structure;
   -- Clock is not called for S2
   S2 : Structure := (A => 0, R => Yesterday);

-----------------------------------
Defaults Within Record Aggregates
-----------------------------------

.. admonition:: Language Variant

   Ada 2005

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

=======================
Discriminated Records
=======================

----------------------------
Discriminated Record Types
----------------------------

* :dfn:`Discriminated record` type

   + Different **objects** may have **different** components
   + All object **still** share the same type

* Kind of :dfn:`storage overlay`

   + Similar to :C:`union` in C
   + But preserves **type checking**
   + And object size **is related to** discriminant

* Aggregate assignment is allowed

---------------
Discriminants
---------------

.. code:: Ada

  type Person_Group is (Student, Faculty);
  type Person (Group : Person_Group) is record
     Name : String (1 .. 10);
     case Group is
        when Student => -- 1st variant
           Gpa  : Float range 0.0 .. 4.0;
        when Faculty => -- 2nd variant
           Pubs : Integer;
     end case;
  end record;

* :ada:`Group` is the :dfn:`discriminant`
* Run-time check for component **consistency**

   + eg :ada:`A_Person.Pubs := 1` checks :ada:`A_Person.Group = Faculty`
   + :ada:`Constraint_Error` if check fails

* Discriminant is **constant**

   + Unless object is **mutable**

-----------
Semantics
-----------

* :ada:`Person` objects are **constrained** by their discriminant

   + **Unless** mutable
   + Assignment from same variant **only**
   + **Representation** requirements

   .. code:: Ada

      Pat  : Person(Student); -- No Pat.Pubs
      Prof : Person(Faculty); -- No Prof.GPA
      Soph : Person := (Group  => Student,
                         Name => "John Jones",
                         GPA  => 3.2);
      X : Person;  -- Illegal: must specify discriminant

   .. code:: Ada

      Pat  := Soph; -- OK
      Soph := Prof; -- Constraint_Error at run time

