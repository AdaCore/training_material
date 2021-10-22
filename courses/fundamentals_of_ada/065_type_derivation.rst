***************
Type Derivation
***************

.. role:: cpp(code)
    :language: C++

.. role:: ada(code)
    :language: Ada

==============
Introduction
==============

-----------------
Type Derivation
-----------------

* Type derivation allows for reusing code
* Type can be **derived** from a **base type**
* Base type can be substituted by the derived type
* Subprograms defined on the base type are **inherited** on derived type
* This is **not** OOP in Ada

    - Tagged derivation **is** OOP in Ada

-------------------------------------
Ada Mechanisms for Type Inheritance
-------------------------------------

* :dfn:`Primitive` operations on types

   - Standard operations like **+** and **-**
   - Any operation that acts on the type

* Type derivation

   - Define types from other types that can add limitations
   - Can add operations to the type

* Tagged derivation

   - **This** is OOP in Ada
   - Seen in other chapter

============
Primitives
============

----------
Examples
----------

.. include:: examples/170_tagged_derivation/primitives.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/170_tagged_derivation.html#primitives`

--------------------
Primitive Operations
--------------------

* A type is characterized by two elements

   - Its data structure
   - The set of operations that applies to it

* The operations are called **primitive operations** in Ada

   .. code:: Ada

      type T is new Integer;
      procedure Attrib_Function(Value : T);

------------------------------
General Rule For a Primitive
------------------------------

* Primitives are subprograms
* `S` is a primitive of type `T` iff

   - `S` is declared in the scope of `T`
   - `S` "uses" type `T`

        + As a parameter
        + As its return type (for :ada:`function`)

   - The **freeze-point** is below `S` declaration

      .. code:: Ada

         package P is
            type T is range 1 .. 10;
            procedure P1 (V : T);
            procedure P2 (V1 : Integer; V2 : T);
            function F return T;
         end P;

------------------------------
Primitive of Multiple Types
------------------------------

* A subprogram can be a primitive of several types

      .. code:: Ada

         package P is
            type T1 is range 1 .. 10;
            type T2 is (A, B, C);

            procedure Proc (V1 : T1; V2 : T2);
            function "+" (V1 : T1; V2 : T2) return T1;
         end P;

--------------
Freeze Point
--------------

* Ada doesn't explicitly identify the end of members declaration
* This end is the implicit **freeze point** occurring whenever:

   - A **variable** of the type is **declared**
   - The type is **derived**
   - The **end of the scope** is reached

* Subprograms past this point are not primitive

.. code:: Ada

   type Root is Integer;
   procedure Prim (V : Root);
   type Child is new Root; -- freeze root
   procedure Prim2 (V : Root); -- Not a primitive

   V : Child; --  freeze child
   procedure Prim3 (V : Child); -- Not a primitive

-------------------------------
Implicit Primitive Operations
-------------------------------

* Type declaration implicitly creates primitives

    - Numerical and logical operations
    - Code can overload or remove them

   .. code:: Ada

      package P is
         type T1 is range 1 .. 10;
         -- implicit: function "+" (Left, Right : T1) return T1;
      end P;
      ...
      procedure Main is
         V1, V2 : T1;
      begin
         V1 := V1 + V2;
      end Main;

===================
Simple Derivation
===================

----------
Examples
----------

.. include:: examples/170_tagged_derivation/simple_derivation.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/170_tagged_derivation.html#simple-derivation`

------------------------
Simple Type Derivation
------------------------

* Any type (except :ada:`tagged`) can be derived

  .. code:: Ada

    type Child is new Parent;

* Child inherits from:

   - The data **representation** of the parent
   - The **primitives** of the parent

* Conversions are possible from child to parent

   .. code:: Ada

     type Parent is range 1 .. 10;
     procedure Prim (V : Parent);
     type Child is new Parent;  -- Freeze Parent
     procedure Not_A_Primitive (V : Parent);
     C : Child;
     ...
     Prim (C);  -- Implicitly declared
     Not_A_Primitive (Parent (C));

--------------------------------------
Simple Derivation and Type Structure
--------------------------------------

* The type "structure" can not change

   - :ada:`array` cannot become :ada:`record`
   - Integers cannot become floats

* But can be **constrained** further
* Scalar ranges can be reduced

   .. code:: Ada

      type Tiny_Int is range -100 .. 100;
      type Tiny_Positive is new Tiny_Int range 1 .. 100;

* Unconstrained types can be constrained

   .. code:: Ada

      type Arr is array (Integer range <>) of Integer;
      type Ten_Elem_Arr is new Arr (1 .. 10);
      type Rec (Size : Integer) is record
         Elem : Arr (1 .. Size);
      end record;
      type Ten_Elem_Rec is new Rec (10);

------------------------
Overriding Indications
------------------------

.. admonition:: Language Variant

   Ada 2005

* **Optional** indications
* Checked by compiler

   .. code:: Ada

      type Root is range 1 .. 100;
      procedure Prim (V : Root);
      type Child is new Root;

* **Replacing** a primitive: :ada:`overriding` indication

   .. code:: Ada

      overriding procedure Prim (V : Child);

* **Adding** a primitive: :ada:`not overriding` indication

   .. code:: Ada

      not overriding procedure Prim2 (V : Child);

* **Removing** a primitive: :ada:`overriding` as :ada:`abstract`

   .. code:: Ada

      overriding procedure Prim (V : Child) is abstract;

------
Quiz
------

.. code:: Ada

   type T1 is range 1 .. 100;
   procedure Proc_A (X : in out T1);

   type T2 is new T1 range 2 .. 99;
   procedure Proc_B (X : in out T1);
   procedure Proc_B (X : in out T2);

   -- Other scope
   procedure Proc_C (X : in out T2);

   type T3 is new T2 range 3 .. 98;

   procedure Proc_C (X : in out T3);

.. container:: columns

 .. container:: column

  Which are :ada:`T1`'s primitives

     A. :answermono:`Proc_A`
     B. ``Proc_B``
     C. ``Proc_C``
     D. No primitives of :ada:`T1`

 .. container:: column

  .. container:: animate

   Explanations

   A. Correct
   B. Freeze: :ada:`T1` has been derived
   C. Freeze: scope change
   D. Incorrect

.

=========
Summary
=========

---------
Summary
---------

* :dfn:`Primitive` of a type

   - Subprogram above **freeze-point** that takes or return the type
   - Can be a primitive for **multiple types**

* Freeze point rules can be tricky
* Simple type derivation

   - Types derived from other types can only **add limitations**

      + Constraints, ranges
      + Cannot change underlying structure

