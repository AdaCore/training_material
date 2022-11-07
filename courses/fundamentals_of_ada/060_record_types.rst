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

      type T ( Size : Natural := 0 ) is record
         Text : String (1 .. Size);
      end record;

==================
Components Rules
==================

----------
Examples
----------

.. include:: examples/060_record_types/components_rules.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/060_record_types.html#components-rules`

-------------------------------
Characteristics of Components
-------------------------------

* **Heterogeneous** types allowed
* Referenced **by name**
* May be no components, for **empty records**
* **No** anonymous types (e.g., arrays) allowed
* **No** constant components
* **No** recursive definitions

------------------------
Components Declarations
------------------------

* Multiple declarations are allowed (like objects)

   .. code:: Ada

      type Several is record
         A, B, C : Integer;
      end record;

* Recursive definitions are not allowed

   .. code:: Ada

      type Not_Legal is record
        A, B : Some_Type;
        C : Not_Legal;
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

------
Quiz
------

.. include:: quiz/record_component_decl/quiz.rst

------
Quiz
------

.. code:: Ada

    type Cell is record
       Val : Integer;
       Message : String;
    end record;

Is the definition legal?

A. Yes
B. :answer:`No`

.. container:: animate

    A :ada:`record` definition cannot have a component of an indefinite type. :ada:`String` is indefinite if you don't specify its size.

============
Operations
============

----------
Examples
----------

.. include:: examples/060_record_types/operations.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/060_record_types.html#operations`

----------------------
Available Operations
----------------------

* Predefined

   - Equality (and thus inequality)

      .. code:: Ada

         if A = B then

   - Assignment

      .. code:: Ada

         A := B;

   - Component-level operations

      + Based on components' types

         .. code:: Ada

            if A.component < B.component then

* User-defined

   - Subprograms

---------------------
Assignment Examples
---------------------

.. code:: Ada

   declare
     type Complex is record
         Real : Float;
         Imaginary : Float;
       end record;
     ...
     Phase1 : Complex;
     Phase2 : Complex;
   begin
     ...
       -- object reference
      Phase1 := Phase2;  -- entire object reference
      -- component references
      Phase1.Real := 2.5;
      Phase1.Real := Phase2.Real;
   end;

============
Aggregates
============

----------
Examples
----------

.. include:: examples/060_record_types/aggregates.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/060_record_types.html#aggregates`

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

  type Months_T is ( January, February, ..., December);
  type Date is record
     Day   : Integer range 1 .. 31;
     Month : Months_T;
     Year  : Integer range 0 .. 2099;
  end record;
  type Person is record
     Born : Date;
     Hair : Color;
  end record;
  John : Person    := ( (21, November, 1990), Brown );
  Julius : Person  := ( (2, August, 1995), Blond );
  Heather : Person := ( (2, March, 1989), Hair => Blond );
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
      A : Real;
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
         A, B, C : Integer := 0;
      end record;

      V : Record_T := (A => 1);
   begin
      Put_Line (Integer'Image (V.A));
   end Main;

A. ``0``
B. ``1``
C. :answer:`Compilation error`
D. Runtime error

.. container:: animate

   The aggregate is incomplete. The aggregate must specify all components, you could use box notation :ada:`(A => 1, others => <>)`

------
Quiz
------

What is the result of building and running this code?

.. code:: Ada

   procedure Main is
      type My_Integer is new Integer;
      type Record_T is record
         A, B, C : Integer := 0;
         D : My_Integer := 0;
      end record;

      V : Record_T := (others => 1);
   begin
      Put_Line (Integer'Image (V.A));
   end Main;

A. ``0``
B. ``1``
C. :answer:`Compilation error`
D. Runtime error

.. container:: animate

   All components associated to a value using :ada:`others` must be of the same :ada:`type`.

------
Quiz
------

What is the result of building and running this code?

.. code:: Ada

   procedure Main is
      type My_Integer is new Integer;
      type Record_T is record
         A, B, C : Integer := 0;
         D : My_Integer := 0;
      end record;

      V : Record_T := (others => <>);
   begin
      Put_Line (Integer'Image (V.A));
   end Main;

A. :answermono:`0`
B. ``1``
C. Compilation error
D. Runtime error

.. container:: animate

   :ada:`<>` is an exception to the rule for :ada:`others`, it can apply to several components of a different type.

------
Quiz
------

What is the result of building and running this code?

.. code:: Ada

   procedure Main is
      type My_Integer is new Integer;
      type Record_T is record
         A : Integer := 0;
      end record;

      V : Record_T := (1);
   begin
      Put_Line (Integer'Image (V.A));
   end Main;

A. ``0``
B. ``1``
C. :answer:`Compilation error`
D. Runtime error

.. container:: animate

    Single-valued aggregate must use named association.

------
Quiz
------

.. code:: Ada

   type Nested_T is record
      Field : Integer := 1_234;
   end record;
   type Record_T is record
      One   : Integer := 1;
      Two   : Character;
      Three  : Integer := -1;
      Four  : Nested_T;
   end record;
   X, Y : Record_T;
   Z    : constant Nested_T := (others => -1);

Which assignment(s) is(are) illegal?

A. :answermono:`X := (1, '2', Three => 3, Four => (6))`
B. ``X := (Two => '2', Four => Z, others => 5)``
C. ``X := Y``
D. ``X := (1, '2', 4, (others => 5))``

.. container:: animate

   A. :ada:`Four` **must** use named association
   B. :ada:`others` valid: :ada:`One` and :ada:`Three` are :ada:`Integer`
   C. Valid but :ada:`Two` is not initialized
   D. Positional for all components

================
Default Values
================

----------
Examples
----------

.. include:: examples/060_record_types/default_values.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/060_record_types.html#default-values`

--------------------------
Component Default Values
--------------------------

.. code:: Ada

   type Complex is
     record
       Real : Real := 0.0;
       Imaginary : Real := 0.0;
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

------------------------------------------
Default Initialization Via Aspect Clause
------------------------------------------

.. admonition:: Language Variant

   Ada 2012

* Not definable for entire record type
* Components of scalar types take type's default if no explicit default value specified by record type

.. code:: Ada

   type Toggle_Switch is (Off, On)
       with Default_Value => Off;
   type Controller is record
       -- Off unless specified during object initialization
       Override : Toggle_Switch;
       -- default for this component
       Enable : Toggle_Switch := On;
     end record;
   C : Controller; -- Override => off, Enable => On
   D : Controller := (On, Off); -- All defaults replaced

------
Quiz
------

.. admonition:: Language Variant

   Ada 2012

.. code:: Ada

   function Next return Natural; -- returns next number starting with 1

   type Record_T is record
      A, B : Integer := Next;
      C    : Integer := Next;
   end record;
   R : Record_T := (C => 100, others => <>);

What is the value of R?

A. (1, 2, 3)
B. (1, 1, 100)
C. :answer:`(1, 2, 100)`
D. (100, 101, 102)

.. container:: animate

 Explanations

 A. :ada:`C => 100`
 B. Multiple declaration calls :ada:`Next` twice
 C. Correct
 D. :ada:`C => 100` has no effect on :ada:`A` and :ada:`B`

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
   + And object size **depends** on discriminant

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
      Soph : Person := ( Group  => Student,
                         Name => "John Jones",
                         GPA  => 3.2);
      X : Person;  -- Illegal: must specify discriminant

   .. code:: Ada

      Pat  := Soph; -- OK
      Soph := Prof; -- Constraint_Error at run time

------------------------------
Mutable Discriminated Record
------------------------------

* When discriminant has a **default value**

   + Objects instantiated **using the default** are **mutable**
   + Objects specifying an **explicit** value are **not** mutable

* Mutable records have **variable** discriminants
* Use **same** storage for **several** variant

.. code:: Ada

  -- Potentially mutable
  type Person (Group : Person_Group := Student) is record

  --  Use default value: mutable
  S : Person;
  --  Explicit value: *not* mutable
  --  even if Student is also the default
  S2 : Person (Group => Student);
  ...
  S := (Group => Student, Gpa => 0.0);
  S := (Group => Faculty, Pubs => 10);

========
Lab
========

.. include:: labs/060_record_types.lab.rst

=========
Summary
=========

---------
Summary
---------

* Heterogeneous types allowed for components
* Default initial values allowed for components

   - Evaluated when each object elaborated, not the type
   - Not evaluated if explicit initial value specified

* Aggregates express literals for composite types

   - Can mix named and positional forms
