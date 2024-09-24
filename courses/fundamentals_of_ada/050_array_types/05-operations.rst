============
Operations
============

-------------------------
Object-Level Operations
-------------------------

* Assignment of array objects

   .. code:: Ada

      A := B;

* Equality and inequality

   .. code:: Ada

      if A = B then

* Conversions

   .. code:: Ada

      C := Foo (B);

   - Component types must be the same type
   - Index types must be the same or convertible
   - Dimensionality must be the same
   - Bounds must be compatible (not necessarily equal)

-------------------------------
Extra Object-Level Operations
-------------------------------

* *Only for 1-dimensional arrays!*
* Concatenation

   .. code:: Ada

      type String_Type is array
        (Integer range <>) of Character;
      A : constant String_Type := "foo";
      B : constant String_Type := "bar";
      C : constant String_Type := A & B;
      -- C now contains "foobar"

* Comparison (for discrete component types)

   * Not for all scalars

* Logical (for :ada:`Boolean` component type)
* Slicing

   - Portion of array

---------
Slicing
---------

* Contiguous subsection of an array
* On any **one-dimensional** array type

  - Any component type

.. code:: Ada

   procedure Test is
     S1 : String (1 .. 9) := "Hi Adam!!";
     S2 : String := "We love    !";
   begin
     S2 (9..11) := S1 (4..6);
     Put_Line (S2);
   end Test;

Result: ``We love Ada!``

----------------------------------------
Example: Slicing with Explicit Indexes
----------------------------------------

* Imagine a requirement to have a ISO date

  - Year, month, and day with a specific format

.. code:: Ada

   declare
      Iso_Date : String (1 .. 10) := "2024-03-27";
   begin
      Put_Line (Iso_Date);
      Put_Line (Iso_Date (1 .. 4));  --  year
      Put_Line (Iso_Date (6 .. 7));  --  month
      Put_Line (Iso_Date (9 .. 10)); --  day

-----------------------------------
Idiom: Named Subtypes for Indexes
-----------------------------------

* Subtype name indicates the slice index range

   - Names for constraints, in this case index constraints

* Enhances readability and robustness

.. code:: Ada

   procedure Test is
     subtype Iso_Index is Positive range 1 .. 10;
     subtype Year is Iso_Index
        range Iso_Index'First .. Iso_Index'First + 3;
     subtype Month is Iso_Index
        range Year'Last + 2 .. Year'Last + 3;
     subtype Day is Iso_Index
        range Month'Last + 2 .. Month'Last + 3;
     Iso_Date : String (Iso_Index) := "2024-03-27";

   begin
     Put_Line (Iso_Date (Year));  --  2024
     Put_Line (Iso_Date (Month)); --  03
     Put_Line (Iso_Date (Day));   --  27

------------------------------------
Dynamic Subtype Constraint Example
------------------------------------

* Useful when constraints not known at compile-time
* Example: remove file name extension

.. code:: Ada

    File_Name
      (File_Name'First
      ..
      Index (File_Name, '.', Direction => Backward));

------
Quiz
------

.. code:: Ada

   type Index_T is range 1 .. 10;
   type OneD_T is array (Index_T) of Boolean;
   type ThreeD_T is array (Index_T, Index_T, Index_T) of OneD_T;
   A : ThreeD_T;
   B : OneD_T;

Which statement(s) is (are) legal?

   A. :answermono:`B(1) := A(1,2,3)(1) or A(4,3,2)(1);`
   B. :answermono:`B := A(2,3,4) and A(4,3,2);`
   C. ``A(1,2,3..4) := A(2,3,4..5);``
   D. :answermono:`B(3..4) := B(4..5)`

.. container:: animate

   Explanations

   A. All three objects are just Boolean values
   B. An element of :ada:`A` is the same type as :ada:`B`
   C. No slicing of multi-dimensional arrays
   D. Slicing allowed on single-dimension arrays

