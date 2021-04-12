
***************
Type Derivation
***************


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

.. include:: examples/170_inheritance/primitives.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/170_inheritance.html#primitives`

---------------------------
The Notion of a Primitive
---------------------------

* A type is characterized by two sets of properties

   - Its data structure
   - The set of operations that applies to it

* These operations are called **methods** in C++, or **Primitive Operations** in Ada

   * In Ada

      -  the primitive relationship is implicit
      - The "hidden" parameter **this** is explicit (and can have any name)

      .. code:: Ada
       
          type T is record
             Attrib_Data : Integer;
          end record;
          procedure Attrib_Function(This : T);

   * In C++

       .. code:: C++
       
          class T {
            public:
              int Attrib_Data;
              void Attrib_Function (void);
          };

------------------------------
General Rule For a Primitive
------------------------------

* A subprogram `S` is a primitive of type `T` if

   - `S` is declared in the scope of `T`
   - `S` has at least one parameter of type `T` (of any mode, including access) or returns a value of type `T`
   * The **freeze-point** has not been reached prior to `S` declaration

      .. code:: Ada

         package P is
            type T is range 1 .. 10;
            procedure P1 (V : T);
            procedure P2 (V1 : Integer; V2 : T);
            function F return T;
         end P;
 
* A subprogram can be a primitive of several types

      .. code:: Ada

         package P is
            type T1 is range 1 .. 10;
            type T2 is (A, B, C);
            procedure Proc (V1 : T1; V2 : T2);
         end P;
 
--------------
Freeze Point
--------------

* Ada doesn't explicitly identify the end of the list of members
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

--------------------------
Beware of Access Types!
--------------------------

* Using a named access type in a subprogram creates a primitive of the access type, **not** the type of the accessed object!

   .. code:: Ada

      package P is
         type T is range 1 .. 10;
         type A_T is access all T;
         procedure Proc (V : A_T); -- Primitive of A_T
      end P;
 
* In order to create a primitive using an access type, the :ada:`access` mode should be used

   .. code:: Ada

      package P is
         type T is range 1 .. 10;
         procedure Proc (V : access T); -- Primitive of T
      end P;

-------------------------------
Implicit Primitive Operations
-------------------------------

* At type declaration, primitives are implicitly created if not explicitly given by the developer, depending on the kind of the type

      .. code:: Ada

         package P is
            type T1 is range 1 .. 10;
            -- implicit: function "+" (Left, Right : T1) return T1;
            -- implicit: function "-" (Left, Right : T1) return T1;
            -- ...
            type T2 is null record;
            -- implicit: function "=" (Left, Right : T2) return T2;
         end P;
 
* These primitives can be used just as any others

      .. code:: Ada

         procedure Main is
            V1, V2 : P.T1;
         begin
            V1 := P."+" (V1, V2);
         end Main;
 
===================
Simple Derivation
===================

----------
Examples
----------

.. include:: examples/170_inheritance/simple_derivation.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/170_inheritance.html#simple-derivation`

------------------------
Simple Type Derivation
------------------------

* In Ada, any (non-tagged) type can be derived
    
  .. code:: Ada
    
    type Child is new Parent;
     
* A child is a distinct type that inherits from:

   - The data representation of the parent
   - The primitives of the parent
    
   .. code:: Ada
    
      procedure Test is
        type Parent is range 1 .. 10;
        procedure Prim (V : Parent);
        type Child is new Parent;
        --  implicit Prim (V : Child);
        V : Child;
      begin
        V := 5;
        Prim (V);

* Conversions are possible for non-primitive operations

   .. code:: Ada
    
      package P is
        type Parent is range 1 .. 10;
        type Child is new Parent;
      end P;
       
      procedure Main is
         procedure Not_A_Primitive (V : Parent);
         V1 : Parent;
         V2 : Child;
      begin
         Not_A_Primitive (V1);
         Not_A_Primitive (Parent (V2));
      end Main;

--------------------------------------
Simple Derivation and Type Structure
--------------------------------------

* The structure of the type has to be kept

   - An array stays an array
   - A scalar stays a scalar

* Scalar ranges can be reduced

   .. code:: Ada

      type Int is range -100 .. 100;
      type Nat is new Int range 0 .. 100;
      type Pos is new Nat range 1 .. 100;
 
* Constraints on unconstrained types can be specified

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

* Operations can be overridden

   + Overriding can be checked by optional `overriding` reserved word

   .. code:: Ada

      type Root is range 1 .. 100;
      procedure Prim (V : Root);
      type Child is new Root;
      overriding procedure Prim (V : Child);
 
* Operations can be added

   + Addition can be checked by optional `not overriding` reserved word

   .. code:: Ada

      type Root is range 1 .. 100;
      procedure Prim (V : Root);
      type Child is new Root;
      not overriding procedure Prim2 (V : Child);
 
* Operations can be removed

   + Removal can be checked by optional `overriding` reserved word

   .. code:: Ada

      type Root is range 1 .. 100;
      procedure Prim (V : Root);
      type Child is new Root;
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

----------------------------------
Signed Integer Types (Revisited)
----------------------------------

* The *Basic Types* lecture introduced Ada's signed integer types, and the predefined integer types in package `Standard`.
* But ... we missed one important detail.
* A declaration like this:

   .. code:: Ada

      type T is range L .. R;
 
* Is actually a short-hand for:

   .. code:: Ada

      type <Anon> is new Predefined_Integer_Type;
      subtype T is <Anon> range L .. R;
 
----------------------------------
Signed Integer Types Explanation
----------------------------------

.. code:: Ada

   type <Anon> is new Predefined-Integer-Type;
   subtype T is <Anon> range L .. R;
 
* What's going on?

   - The compiler looks at L and R (which must be static) and chooses a predefined signed integer type from `Standard` (e.g. `Integer`, `Short_Integer`, `Long_Integer`, etc.) which at least includes the range L .. R.
   - This choice is implementation-defined.
   - An anonymous type `Anon` is created, derived from that predefined type. `Anon` inherits all of the predefined type's primitive operations, like ``+``, ``-``, ``*`` and so on.
   - A subtype `T` of `Anon` is created with range L .. R

      + `Anon` can be referred to as `T'Base` in your program.

------------------------------
Signed Integer Types Warning
------------------------------

.. code:: Ada

   type <Anon> is new Predefined-Integer-Type;
   subtype T is <Anon> range L .. R;
 
* Warning! The choice of `T'Base` affects whether runtime computations will overflow.

   - Example: on one machine, the compiler chooses `Integer`, which is 32-bit, and your code runs fine with no overflows.
   - On another machine, a compiler might choose `Short_Integer`, which is 16-bit, and your code will fail an *overflow check*
   - Extra care is needed if you have two compilers - e.g. for Host (like Windows or Linux) and Cross targets...

* Good news! GNAT makes consistent and predictable choices on all major platforms.

-------------------------------
Signed Integer Types Guidance
-------------------------------

* You can avoid the implementation-defined choice by deriving your own Base Types explicitly, and using `Assert` to enforce the expected range

   - Something like

   .. code:: Ada

      type My_Base_Integer is new Integer;
      pragma Assert (My_Base_Integer'First = -2**31);
      pragma Assert (My_Base_Integer'Last = 2**31-1);
 
* Then derive further types and subtypes from `My_Base_Integer`
* Don't assume that "Shorter = Faster" for integer maths. On some machines, 32-bit is more efficient than 8- or 16-bit maths!

--------------------------------------
Signed Integer Types Guidance (cont)
--------------------------------------

* If you want to derive from a base type that has a well-defined bit length (for example when dealing with hardware registers that must be a particular bit length), then package Interfaces declares types such as:

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

* *Primitive* operations on types

   - An operation is any subprogram that acts on a type
   - Single subprogram can be a primitive for multiple types

      + Any type referenced in the subprogram

* Freeze point rules can be tricky

    - Primitive of multiple types

* Simple type derivation

   - Types derived from other types can only add limitations

      + Constraints, ranges
      + Cannot change underyling structure

