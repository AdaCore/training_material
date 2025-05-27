=======
Proof
=======

----------------------
Functional Contracts
----------------------

* Precondition introduced by aspect :ada:`Pre`

  - Boolean expression stating **constraint on the caller**
  - Constraint on the value of inputs

* Postcondition introduced by aspect :ada:`Post`

  - Boolean expression stating **constraint on the subprogram**
  - Constraint on the value of inputs and outputs

* On the first declaration of a subprogram

  - This can be a spec or a body

* Optional, default is :ada:`True`

  - Precondition: subprogram can be called in any context
  - Postcondition: subprogram gives no information on its behavior

* Special attributes in postconditions

  - :ada:`X'Old` denotes the input value of :ada:`X`
  - :ada:`F'Result` denotes the result of function :ada:`F`

-----------------------------
Silver/Gold/Platinum Levels
-----------------------------

* Check absence of runtime errors (AoRTE)
* Check that assertions are always true
* Check that code respects functional contracts

  *basics.ads*

  .. code:: Ada
     :number-lines: 3

     procedure Swap (X, Y : in out Integer)
     with
       Post => X = Y'Old and Y = X'Old;

  *basics.adb*

  .. code:: Ada
     :number-lines: 5

     procedure Swap (X, Y : in out Integer) is
     begin
        Temp := Y;
        X := Y;
        Y := Temp;
     end Swap;

  :command:`basics.ads:3:20: warning: unused initial value of "X"`

  :command:`basics.ads:5:30: medium: postcondition might fail, cannot prove Y = X'Old`

-------------------------------
Run-Time Errors Are Pervasive
-------------------------------

.. container:: columns

 .. container:: column

    * A simple assignment statement

      .. code:: Ada

         A (I + J) := P / Q;

    * Which are the possible run-time errors for this example?

 .. container:: column

    * ``I+J`` might overflow the base type of the index range's subtype
    * ``I+J`` might be outside the index range's subtype
    * ``P/Q`` might overflow the base type of the component type
    * ``P/Q`` might be outside the component subtype
    * ``Q`` might be zero

-------------------------------
Categories of Run-Time Errors
-------------------------------

* Divide by zero

  - Arithmetic operations: division, :ada:`mod`, :ada:`rem`

* Index check

  - Read/write access in an array

* Overflow check

  - Most arithmetic operations
  - Checking that result is within bounds of the machine integer or float

* Range check

  - Type conversion, type qualification, assignment
  - Checking that the value satisfies range constraint of type

* Discriminant check

  - Read/write access in a discriminated record

* Length check

  - Assignment of an array or string

* Checks on pointer programs - Details in the course on Pointer Programs

-----------------------------------------
Quiz - Special Cases of Run-Time Errors
-----------------------------------------

Consider the following declarations:

.. code:: ada

   type Table is array (Natural range <>) of Integer;
   type Rec (Disc : Boolean) is record ...
   T : Table := ...;
   R : Rec := ...;
   X : Integer;

Which of the following *cannot* cause a runtime error:

   A. ``X := T (T'First)``
   B. ``X := X / (-1);``
   C. ``X := abs X;``
   D. ``X := T'Length;``
   E. ``R := (Disc => True, ...);``

.. container:: animate

   Explanations: **all** of then can cause a runtime error!

   A. Index check fails if :ada:`T` is empty.
   B. Overflow check fails if :ada:`X = Integer'First`
   C. Overflow check fails if :ada:`X = Integer'First`
   D. Range check fails if :ada:`T'Range` is :ada:`Natural`
   E. Discriminant check fails if :ada:`R.Disc /= True`

--------------------------
Categories of Assertions
--------------------------

* Pragma :ada:`Assert` and similar (:ada:`Assert_And_Cut`, :ada:`Assume`)

  - AoRTE is also proved for its expression

* Precondition on call

  - AoRTE is also proved for **any** calling context
  - This may require **guarding** the precondition

  .. code:: ada

     procedure Update (T : in out Table; X : Index; V : Value)
       with Pre => T (X) /= V; -- Index check might fail
       with Pre => X in T'Range and T (X) /= V; -- Same
       with Pre => X in T'Range and then T (X) /= V; -- OK

* Postcondition on subprogram

  - AoRTE is proved in the context of the subprogram **body**
  - Still better to include info for AoRTE in **caller**

  .. code:: ada

     procedure Find (T : Table; X : out Index; V : Value)
       with Post => T (X) = V; -- Not known that X in T'Range
       with Post => X in T'Range and then T (X) = V; -- OK

------------------------------
Levels of Software Assurance
------------------------------

* Silver level

  - Goal is **absence** of runtime errors
  - Functional contracts added to support that goal

    + Typically a few preconditions only

  .. code:: ada

     procedure Update (T : in out Table; X : Index; V : Value)
       with Pre => X in T'Range;

* Gold level

  - Builds on the Silver level
  - Functional contracts added to **express desired properties**

  .. code:: ada

     procedure Update (T : in out Table; X : Index; V : Value)
       with Pre  => X in T'Range,
            Post => T (X) = V;

* Platinum level

  - Same as Gold level
  - But the **full** functional specification is expressed as contracts

  .. code:: ada

     procedure Update (T : in out Table; X : Index; V : Value)
       with Pre  => X in T'Range,
            Post => T = (T'Old with delta X => V);

---------------
Preconditions
---------------

* Default precondition of :ada:`True` may **not** be sufficient

  .. code:: ada

     procedure Increment (X : in out Integer) is
     begin
        X := X + 1; -- Overflow check might fail
     end Increment;

* Precondition constrains **input context**

  .. code:: ada

     procedure Increment (X : in out Integer)
     with
       Pre => X < Integer'Last
     begin
        X := X + 1; -- Overflow check proved
     end Increment;

----------------
Postconditions
----------------

* Default postcondition of :ada:`True` may **not** be sufficient

  .. code:: ada

     procedure Add2 (X : in out Integer)
     with
       Pre => X < Integer'Last - 1
     is
     begin
        Increment (X);
        Increment (X); -- Precondition might fail
     end Add2;

* Postcondition constrains **output context**

  .. code:: ada

     procedure Increment (X : in out Integer)
     with
       Pre  => X < Integer'Last,
       Post => X = X'Old + 1;

     procedure Add2 (X : in out Integer)
     with
       Pre => X < Integer'Last - 1
     is
     begin
        Increment (X);
        Increment (X); -- Precondition proved
     end Add2;

------------------------------------------
Contextual Analysis of Local Subprograms
------------------------------------------

* Local subprograms without contracts are *inlined* in proof

  - Local: declared inside private part or body
  - Without contracts: no :ada:`Global`, :ada:`Pre`, :ada:`Post`, etc.
  - Additional conditions, details in the SPARK User's Guide

|

* Benefit: no need to add a contract

|

* Possible cost: proof of caller may become more complex

  - Add explicit contract like :ada:`Pre => True` to disable inlining of a
    subprogram
  - Use switch :command:`--no-inlining` to disable this feature globally
  - Use switch :command:`--info` to get more information about inlined
    subprograms

-------------------------
Overflow Checking (1/2)
-------------------------

* Remember: assertions might fail overflow checks

  .. code:: ada

     procedure Saturate_Add (X, Y : Natural; Z : out Natural)
       with Post => Z = Integer'Min (X + Y, Natural'Last);

* Sometimes property can be expressed to avoid overflows

  .. code:: ada

     procedure Saturate_Add (X, Y : Natural; Z : out Natural)
       with Post => Z =
         (if X <= Natural'Last - Y then X + Y else Natural'Last);

* Or a larger integer type can be used for computations

  .. code:: ada

     subtype LI is Long_Integer;

     procedure Saturate_Add (X, Y : Natural; Z : out Natural)
       with Post => LI(Z) =
         LI'Min (LI(X) + LI(Y), LI(Natural'Last));

-------------------------
Overflow Checking (2/2)
-------------------------

* Alternative: use a library of big integers

  - From SPARK Library :filename:`SPARK.Big_Integers`
  - Or Ada stdlib: :filename:`Ada.Numerics.Big_Numbers.Big_Integers`

  .. code:: Ada

     function Big (Arg : Integer) return Big_Integer is
       (To_Big_Integer (Arg)) with Ghost;
     procedure Saturate_Add (X, Y : Natural; Z : out Natural)
       with Post => Z =
         (if Big (X) + Big (Y) <= Big (Natural'Last)
          then X + Y else Natural'Last);

* Or use compiler switch :command:`-gnato13` to use big integers in all assertions

  - Implicit use
  - Should be used also when compiling assertions
  - Only applies to arithmetic operations (not :ada:`Integer'Min`)

  .. code:: ada

     procedure Saturate_Add (X, Y : Natural; Z : out Natural)
       with Post => Z =
         (if X + Y <= Natural'Last then X + Y else Natural'Last);

