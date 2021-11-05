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

---------------------------------------
Type Derivation
---------------------------------------

* Type derivation allows for reusing code
* Type can be **derived** from a **base type**
* Base type can be substituted by the derived type
* Subprograms defined on the base type are **inherited** on derived type
* This is **not** OOP in Ada

    - Tagged derivation **is** OOP in Ada

-------------------------------------
Ada Mechanisms for Type Inheritance
-------------------------------------

* *Primitive* operations on types

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

* In Ada, any (non-tagged) type can be derived

  .. code:: Ada

    type Child is new Parent;

* Child inherits from:

   - The data **representation** of the parent
   - The **primitives** of the parent

   .. code:: Ada

      package P is
        type Parent is range 1 .. 10;
        procedure Prim (V : Parent);
        type Child is new Parent;
        --  implicit Prim (V : Child);
      end P;

      with P; use P;
      procedure Main1 is
        V : Child;
      begin
        V := 5;
        Prim (V);
      end Main1;

* Conversions are possible for non-primitive operations

   .. code:: Ada

     with P; use P;
     procedure Main2 is
        procedure Not_A_Primitive (V : Parent)
           is null;
        V1 : Parent;
        V2 : Child;
     begin
        Not_A_Primitive (V1);
        Not_A_Primitive (Parent (V2));
     end Main2;

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

------------------------------------------
Simple Derivation and List of Operations
------------------------------------------

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

   package P1 is
      type T1 is range 1 .. 100;
      procedure Proc_A (X : in out T1);

      type T2 is new T1 range 2 .. 99;
      procedure Proc_B (X : in out T1);
      procedure Proc_B (X : in out T2);
   end P1;

   with P1; use P1;
   package P2 is
      procedure Proc_C (X : in out T2);

      type T3 is new T2 range 3 .. 98;

      procedure Proc_C (X : in out T3);
   end P2;

Which subprogram(s) is/are a primitive of T1

   A. :answermono:`Proc_A`
   B. ``Proc_A, Proc_B``
   C. ``Proc_A, Proc_B, Proc_C``
   D. No primitives of T1

.. container:: animate

   Explanations

   A. Correct
   B. :ada:`Proc_B` is defined *too late* - a new type has been derived from :ada:`T1`
   C. :ada:`Proc_B` is defined *too late* and :ada:`Proc_C` is in the wrong scope
   D. Incorrect

======================
Signed Integer Types
======================

----------------
Implicit Subtype
----------------

* The declaration

   .. code:: Ada

      type T is range L .. R;

* Is short-hand for

   .. code:: Ada

      type <Anon> is new Predefined_Integer_Type;
      subtype T is <Anon> range L .. R;

----------------------------
Implicit Subtype Explanation
----------------------------

.. code:: Ada

   type <Anon> is new Predefined-Integer-Type;
   subtype T is <Anon> range L .. R;

* Compiler choses a standard integer type that includes L .. R

   - :ada:`Integer`, :ada:`Short_Integer`, :ada:`Long_Integer`, etc.
   - **Implementation-defined** choice, non portable

* New anonymous type `Anon` is derived from the predefined type
* `Anon` inherits the type's operations (``+``, ``-`` ...)
* `T`, subtype of `Anon` is created with range L .. R
* :ada:`T'Base` will return the type `Anon`

--------------------------
Integer Types Base Warning
--------------------------

.. code:: Ada

   type <Anon> is new Predefined-Integer-Type;
   subtype T is <Anon> range L .. R;

* Runtime overflow conditions depend on :ada:`T'Base`
* Compiler will change base type depending on machine
* Take extra care when using two compilers

    - Multiple hosts (Windows, Linux), or architectures

* GNAT makes consistent and predictable choices on all major platforms.

-----------------------
Package **Interfaces**
-----------------------

* **Standard** package
* Integer types with **defined bit length**

   .. code:: Ada

      type My_Base_Integer is new Integer;
      pragma Assert (My_Base_Integer'First = -2**31);
      pragma Assert (My_Base_Integer'Last = 2**31-1);

    - Dealing with hardware registers

* Note: Shorter may not be faster for integer maths.

    - Modern 64-bit machines are not efficient at 8-bit maths

.. code:: Ada

   type Integer_8 is range -2**7 .. 2**7-1;
   for Integer_8'Size use 8;
   -- and so on for 16, 32, 64 bit types...

=========
Summary
=========

---------
Summary
---------

* *Primitive* of a type

   - Subprogram above **freeze-point** that takes or return the type
   - Can be a primitive for **multiple types**

* Freeze point rules can be tricky
* Simple type derivation

   - Types derived from other types can only **add limitations**

      + Constraints, ranges
      + Cannot change underlying structure

