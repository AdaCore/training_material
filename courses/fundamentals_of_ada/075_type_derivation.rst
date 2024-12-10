***************
Type Derivation
***************

.. container:: PRELUDE BEGIN

.. container:: PRELUDE ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. container:: PRELUDE SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. container:: PRELUDE REQUIRES

.. container:: PRELUDE PROVIDES

.. container:: PRELUDE END

==============
Introduction
==============

-----------------
Type Derivation
-----------------

* Type :dfn:`derivation` allows for reusing code
* Type can be **derived** from a **base type**
* Base type can be substituted by the derived type
* Subprograms defined on the base type are **inherited** on derived type
* This is **not** OOP in Ada

    - Tagged derivation **is** OOP in Ada

---------------------------
Reminder: What is a Type?
---------------------------

* A type is characterized by two elements

   - Its data structure
   - The set of operations that applies to it

* The operations are called **primitive operations** in Ada

.. container:: latex_environment small

   .. code:: Ada

      package Types is
         type Integer_T is range -(2**63) .. 2**63-1 with Size => 64; 
         procedure Increment_With_Truncation (Val : in out Integer_T);
         procedure Increment_With_Rounding (Val : in out Integer_T);
     end Types;

===================
Simple Derivation
===================

------------------------
Simple Type Derivation
------------------------

* Any type (except :ada:`tagged`) can be derived

  .. code:: Ada

    type Natural_T is new Integer_T range 0 .. Integer_T'Last;

* :ada:`Positive_T` inherits from:

   - The data **representation** of the parent

      * Integer based, 64 bits

   - The **primitives** of the parent

      * :ada:`Increment_With_Truncation` and :ada:`Increment_With_Rounding`

* The types are not the same

   .. code:: Ada

      I_Obj : Integer_T := 0;
      N_Obj : Natural_T := 0;

   * :ada:`I_Obj := N_Obj;` generates a compile error

      :color-red:`expected type "Integer_T" defined at line 2`

   * But a child can be converted to the parent

      * :ada:`I_Obj := Integer_T (N_Obj);`

--------------------------------------
Simple Derivation and Type Structure
--------------------------------------

* The type "structure" can not change

   - :ada:`array` cannot become :ada:`record`
   - Integers cannot become floats

* But can be **constrained** further
* Scalar ranges can be reduced

   .. code:: Ada

      type Positive_T is new Natural_T range 1 .. Natural_T'Last;

* Unconstrained types can be constrained

   .. code:: Ada

      type Arr_T is array (Integer range <>) of Integer;
      type Ten_Elem_Arr_T is new Arr_T (1 .. 10);
      type Rec_T (Size : Integer) is record
         Elem : Arr_T (1 .. Size);
      end record;
      type Ten_Elem_Rec_T is new Rec_T (10);

============
Primitives
============

--------------------
Primitive Operations
--------------------

* Primitive Operations are those subprograms associated with a type

.. code:: Ada

   type Integer_T is range -(2**63) .. 2**63-1 with Size => 64; 
   procedure Increment_With_Truncation (Val : in out Integer_T);
   procedure Increment_With_Rounding (Val : in out Integer_T);

* Note most scalars have some primitive operations defined by the language

   * e.g. :ada:`+` and :ada:`*` for numeric types, etc

* A primitive operation can be used on a child type with no conversion

   .. code:: Ada

      declare
         N_Obj : Natural_T := 1234;
      begin
         Increment_With_Truncation (N_Obj);
      end;

---------------------------------------
General Rule for Defining a Primitive
---------------------------------------

* Primitives are subprograms
* Subprogram :ada:`S` is a primitive of type :ada:`T` if and only if:

   - :ada:`S` is declared in the scope of :ada:`T`
   - :ada:`S` uses type :ada:`T`

        + As a parameter
        + As its return type (for a :ada:`function`)

   - :ada:`S` is above :dfn:`freeze-point` (see next section)

* Standard practice

    - Primitives should be declared **right after** the type itself
    - In a scope, declare at most a **single** type with primitives

      .. code:: Ada

         package P is
            type T is range 1 .. 10;
            procedure P1 (V : T);
            procedure P2 (V1 : Integer; V2 : T);
            function F return T;
         end P;

----------------------------------
Creating Primitives for Children
----------------------------------

* Just because we can inherit a primitive from out parent doesn't mean we want to

* Create a new primitive (with the same name as the parent) for the child

   * Very similar to overloaded subprograms
   * But added benefit of visibility to grandchildren

.. code:: Ada

   type Integer_T is range -(2**63) .. 2**63-1; 
   procedure Increment_With_Truncation (Val : in out Integer_T);
   procedure Increment_With_Rounding (Val : in out Integer_T);
   
   type Child_T is new Integer_T range -1000 .. 1000;
   procedure Increment_With_Truncation (Val : in out Child_T);
   
   type Grandchild_T is new Child_T range -100 .. 100;
   procedure Increment_With_Rounding (Val : in out Grandchild_T);

------------------------
Overriding Indications
------------------------

* **Optional** indications
* Checked by compiler

   .. container:: latex_environment footnotesize

      .. code:: Ada

         type Child_T is new Integer_T range -1000 .. 1000;
         procedure Increment_With_Truncation
            (Val : in out Child_T);
         procedure Just_For_Child
            (Val : in out Child_T);

* **Replacing** a primitive: :ada:`overriding` indication

   .. container:: latex_environment footnotesize

      .. code:: Ada

         overriding procedure Increment_With_Truncation
            (Val : in out Child_T);

* **Adding** a primitive: :ada:`not overriding` indication

   .. container:: latex_environment footnotesize

      .. code:: Ada

         not overriding procedure Just_For_Child
            (Val : in out Child_T);

* **Removing** a primitive: :ada:`overriding` as :ada:`abstract`

   .. container:: latex_environment footnotesize

      .. code:: Ada

         overriding procedure Just_For_Child
            (Val : in out Grandchild_T) is abstract;

..
  language_version 2005

==============
Freeze Point
==============

-----------------------------
What is the "Freeze Point"?
-----------------------------

* Ada doesn't explicitly identify the end of the "scope" of a type

   * The compiler needs to know it for determining primitive operations
   * Also needed for other situations (described elsewhere)

* This end is the implicit **freeze point** occurring whenever:

   - A **variable** of the type is **declared**
   - The type is **derived**
   - The **end of the scope** is reached

* Subprograms past this "freeze point" are not primitive operations

.. code:: Ada

   type Parent is Integer;
   procedure Prim (V : Parent);

   type Child is new Parent;

   -- Parent has been derived, so it is frozen.
   -- Prim2 is not a primitive
   procedure Prim2 (V : Parent);

   V : Child;

   -- Child used in an object declaration, so it is frozen
   -- Prim3 is not a primitive
   procedure Prim3 (V : Child);

-----------------------
Debugging Type Freeze
-----------------------

* Freeze |rightarrow| Type **completely** defined
* Compiler does **need** to determine the freeze point

    - To instantiate, derive, get info on the type (:ada:`'Size`)...
    - Freeze rules are a guide to place it
    - Actual choice is more technical

        + May contradict the standard

* :command:`-gnatDG` to get **expanded** source

    - **Pseudo-Ada** debug information

:filename:`pkg.ads`

   .. code:: Ada

       type Up_To_Eleven is range 0 .. 11;

:filename:`<obj>/pkg.ads.dg`

.. container:: latex_environment tiny
        
   ::

      type example__up_to_eleven_t is range 0 .. 11;              -- type declaration
      [type example__Tup_to_eleven_tB is new short_short_integer] -- representation
      freeze example__Tup_to_eleven_tB []                         -- freeze representation
      freeze example__up_to_eleven_t []                           -- freeze representation

------
Quiz
------

.. container:: latex_environment tiny

   .. code:: Ada

      type Parent is range 1 .. 100;
      procedure Proc_A (X : in out Parent);

      type Child is new Parent range 2 .. 99;
      procedure Proc_B (X : in out Parent);
      procedure Proc_B (X : in out Child);

      -- Other scope
      procedure Proc_C (X : in out Child);

      type Grandchild is new Child range 3 .. 98;

      procedure Proc_C (X : in out Grandchild);

.. container:: columns

 .. container:: column

  Which are :ada:`Parent`'s primitives

     A. :answermono:`Proc_A`
     B. ``Proc_B``
     C. ``Proc_C``
     D. No primitives of :ada:`Parent`

 .. container:: column

  .. container:: animate

   Explanations

   A. Correct
   B. Freeze: :ada:`Parent` has been derived
   C. Freeze: scope change
   D. Incorrect


=========
Summary
=========

---------
Summary
---------

* :dfn:`Primitive` of a type

   - Subprogram above **freeze-point** that takes or returns the type
   - Can be a primitive for **multiple types**

* Freeze point rules can be tricky
* Simple type derivation

   - Types derived from other types can only **add limitations**

      + Constraints, ranges
      + Cannot change underlying structure

