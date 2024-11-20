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
       Component_4 => <>, -- Default value (Ada 2005)
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

------------------------
Aggregate Completeness
------------------------

.. container:: columns

 .. container:: column

    * All component values must be accounted for

       - Including defaults via ``box``

    * Allows compiler to check for missed components
    * Type definition

       .. code:: Ada

          type Struct is record
              A : Integer;
              B : Integer;
              C : Integer;
              D : Integer;
            end record;
          S : Struct;

 .. container:: column

    * Compiler will not catch the missing component

       .. code:: Ada

          S.A := 10;
          S.B := 20;
          S.C := 12;
          Send (S);

    * Aggregate must be complete - compiler error

       .. code:: Ada

          S := (10, 20, 12);
          Send (S);

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

------------------------------------
Aggregates with Only One Component
------------------------------------

* **Must** use named form
* Same reason as array aggregates

.. code:: Ada

   type Singular is record
      A : Integer;
   end record;

   S : Singular := (3);          -- illegal
   S : Singular := (3 + 1);      -- illegal
   S : Singular := (A => 3 + 1); -- required

--------------------------
Aggregates with `others`
--------------------------

* Indicates all components not yet specified (like arrays)
* All :ada:`others` get the same value

  - They must be the **exact same** type

.. code:: Ada

   type Poly is record
      A : Float;
      B, C, D : Integer;
   end record;

   P : Poly := (2.5, 3, others => 0);

   type Homogeneous is record
      A, B, C : Integer;
   end record;

   Q : Homogeneous := (others => 10);

------
Quiz
------

What is the result of building and running this code?

.. code:: Ada

   procedure Main is
      type Record_T is record
         A, B, C : Integer;
      end record;

      V : Record_T := (A => 1);
   begin
      Put_Line (Integer'Image (V.A));
   end Main;

A. ``0``
B. ``1``
C. :answer:`Compilation error`
D. Run-time error

.. container:: animate

   The aggregate is incomplete. The aggregate must specify all components. You could use box notation :ada:`(A => 1, others => <>)`

------
Quiz
------

What is the result of building and running this code?

.. code:: Ada

   procedure Main is
      type My_Integer is new Integer;
      type Record_T is record
         A, B, C : Integer;
         D : My_Integer;
      end record;

      V : Record_T := (others => 1);
   begin
      Put_Line (Integer'Image (V.A));
   end Main;

A. ``0``
B. ``1``
C. :answer:`Compilation error`
D. Run-time error

.. container:: animate

   All components associated to a value using :ada:`others` must be of the same :ada:`type`.

------
Quiz
------

.. code:: Ada

   type Nested_T is record
      Field : Integer;
   end record;
   type Record_T is record
      One   : Integer;
      Two   : Character;
      Three  : Integer;
      Four  : Nested_T;
   end record;
   X, Y : Record_T;
   Z    : constant Nested_T := (others => -1);

Which assignment(s) is (are) legal?

A. ``X := (1, '2', Three => 3, Four => (6))``
B. :answermono:`X := (Two => '2', Four => Z, others => 5)`
C. :answermono:`X := Y`
D. :answermono:`X := (1, '2', 4, (others => 5))`

.. container:: animate

   A. :ada:`Four` **must** use named association
   B. :ada:`others` valid: :ada:`One` and :ada:`Three` are :ada:`Integer`
   C. Valid but :ada:`Two` is not initialized
   D. Positional for all components

------------------
Delta Aggregates
------------------

.. admonition:: Language Variant

   Ada 2022

* A Record can use a :dfn:`delta aggregate` just like an array

   .. code:: Ada

      type Coordinate_T is record
         X, Y, Z : Float;
      end record;
      Location : constant Coordinate_T := (1.0, 2.0, 3.0);

* Prior to Ada 2022, you would copy and then modify

   .. code:: Ada

      declare
         New_Location : Coordinate_T := Location;
      begin
         New_Location.Z := 0.0;
         -- OR
         New_Location := (Z => 0.0, others => <>);
      end;

* Now in Ada 2022 we can just specify the change during the copy

   .. code:: Ada

      New_Location : Coordinate_T := (Location with delta Z => 0.0);

   *Note for record delta aggregates you must use named notation*

