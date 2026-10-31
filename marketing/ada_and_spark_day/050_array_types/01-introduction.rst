==============
Introduction
==============

---------------
Arrays in Ada
---------------

* Traditional array concept supported to any dimension

.. code:: Ada

   declare
      type Hours is digits 6;
      type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
      type Schedule is array (Days) of Hours;
      Workdays : Schedule;
   begin
      ...
      Workdays (Mon) := 8.5;

------------------------------
Array Type Index Constraints
------------------------------

* Must be of an integer or enumeration type
* May be dynamic
* Default to predefined `Integer`
* Allowed to be null range

   - Defines an empty array
   - Meaningful when bounds are computed at run-time

* Used to define constrained array types

  .. code:: Ada

     type Schedule is array (Days range Mon .. Fri) of Float;
     type Flags_T is array (-10 .. 10) of Boolean;

* Or to constrain unconstrained array types

  .. code:: Ada

     subtype Line is String (1 .. 80);
     subtype Translation is Matrix (1..3, 1..3);

-------------------------
Run-Time Index Checking
-------------------------

* Array indexes are checked at run-time as needed
* Invalid index values result in :ada:`Constraint_Error`

.. code:: Ada

   procedure Test is
     type Int_Arr is array (1..10) of Integer;
     A : Int_Arr;
     K : Integer;
   begin
     A := (others => 0);
     K := FOO;
     A (K) := 42; -- run-time error if Foo returns < 1 or > 10
     Put_Line (A(K)'Image);
   end Test;
