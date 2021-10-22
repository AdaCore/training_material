**************
Polymorphism
**************

.. |rightarrow| replace:: :math:`\rightarrow`

.. role:: ada(code)
    :language: Ada

==============
Introduction
==============

--------------
Introduction
--------------

* `'Class` operator to categorize classes of types
* Type classes allow dispatching calls

   - Abstract types
   - Abstract subprograms

* Run-time call dispatch vs compile-time call dispatching

==================
Classes of Types
==================

----------
Classes
----------

* In Ada, a Class denotes an inheritance subtree
* Class of `T` is the class of `T` and all its children
* Type :ada:`T'Class` can designate any object typed after type of class of `T`

   .. code:: Ada

      type Root is tagged null record;
      type Child1 is new Root with null record;
      type Child2 is new Root with null record;
      type Grand_Child1 is new Child1 with null record;
      -- Root'Class = {Root, Child1, Child2, Grand_Child1}
      -- Child1'Class = {Child1, Grand_Child1}
      -- Child2'Class = {Child2}
      -- Grand_Child1'Class = {Grand_Child1}

* Objects of type :ada:`T'Class` have at least the properties of T

   - Fields of `T`
   - Primitives of `T`

-----------------
Indefinite type
-----------------

* A class wide type is an indefinite type

   - Just like an unconstrained array or a record with a discriminant

* Properties and constraints of indefinite types apply

   - Can be used for parameter declarations
   - Can be used for variable declaration with initialization

.. code:: Ada

   procedure Main is
      type T is tagged null record;
      type D is new T with null record;
      procedure P (X : in out T'Class) is null;
      Obj : D;
      Dc  : D'Class := Obj;
      Tc1 : T'Class := Dc;
      Tc2 : T'Class := Obj;
      -- initialization required in class-wide declaration
      Tc3 : T'Class; -- compile error
      Dc2 : D'Class; -- compile error
   begin
      P (Dc);
      P (Obj);
   end Main;

-------------------------------
Testing the type of an object
-------------------------------

* The tag of an object denotes its type
* It can be accessed through the `'Tag` attribute
* Applies to both objects and types
* Membership operator is available to check the type against a hierarchy

.. code:: Ada

   type Parent is tagged null record;
   type Child is new Parent with null record;
   Parent_Obj : Parent; -- Parent_Obj'Tag = Parent'Tag
   Child_Obj  : Child;  -- Child_Obj'Tag = Child'Tag
   Parent_Class_1 : Parent'Class := Parent_Obj;
                    -- Parent_Class_1'Tag = Parent'Tag
   Parent_Class_2 : Parent'Class := Child_Obj;
                    -- Parent_Class_2'Tag = Child'Tag
   Child_Class    : Child'Class := Child(Parent_Class_2);
                    -- Child_Class'Tag  = Child'Tag

   B1 : Boolean := Parent_Class_1 in Parent'Class;       -- True
   B2 : Boolean := Parent_Class_1'Tag = Child'Class'Tag; -- False
   B3 : Boolean := Child_Class'Tag = Parent'Class'Tag;   -- False
   B4 : Boolean := Child_Class in Child'Class;           -- True

=============
Dispatching
=============

---------------------------
Calls on class-wide types
---------------------------

* Any subprogram expecting a T object can be called with a :ada:`T'Class` object

.. code:: Ada

   type Root is null record;
   procedure P (V : Root);

   type Child is new Root with null record;
   procedure P (V : Child);

      V1 : Root'Class := [...]
      V2 : Child'Class := [...]
   begin
      P (V1);
      P (V2);

-------------------
Dispatching calls
-------------------

* The **actual** type of the object is not known at compile time
* The *right* type will be selected at runtime

    - Its primitive will be called

.. container:: columns

 .. container:: column

   *Ada*

      .. code:: Ada

         declare
           V1 : Root'Class :=
                Root'(others => <>);
           V2 : Root'Class :=
                Child'(others => <>);
         begin
           V1.P; -- calls P of Root
           V2.P; -- calls P of Child

 .. container:: column

   *C++*

      .. code:: C++

         Root * V1 = new Root ();
         Root * V2 = new Child ();
         V1->P ();
         V2->P ();

------
Quiz
------

.. code::Ada

   package P is
      type Root is tagged null record;
      function F1 (V : Root) return Integer is (101);
      type Child is new Root with null record;
      function F1 (V : Child) return Integer is (201);
      type Grandchild is new Child with null record;
      function F1 (V : Grandchild) return Integer is (301);
   end P;

   with P1; use P1;
   procedure Main is
      Z : Root'Class := Grandchild'(others => <>);

What is the value returned by :ada:`F1 (Child'Class (Z));`?

   A. :answer:`301`
   B. 201
   C. 101
   D. Compilation error

.. container:: animate

   Explanations

   A. Correct
   B. Would be correct if the cast was :ada:`Child` - :ada:`Child'Class` leaves the object as :ada:`Grandchild`
   C. Object is initialized to something in :ada:`Root'class`, but it doesn't have to be :ada:`Root`
   D. Would be correct if function parameter types were :ada:`'Class`

===============================
Exotic Dispatching Operations
===============================

-------------------------------
Multiple dispatching operands
-------------------------------

* Primitives with multiple dispatching operands are allowed if all operands are of the same type

   .. code:: Ada

      type Root is null tagged record;
      procedure P (Left : Root; Right : Root);
      type Child is new Root with null record;
      overriding procedure P (Left : Child; Right : Child);

* At call time, all actual parameters' tags have to match, either statically or dynamically

   .. code:: Ada

      R1, R2 : Root;
      C1, C2 : Child;
      Cl1 : Root'Class := R1;
      Cl2 : Root'Class := R2;
      Cl3 : Root'Class := C1;
      ...
      P (R1, R2);               -- static:  ok
      P (R1, C1);               -- static:  error
      P (Cl1, Cl2);             -- dynamic: ok
      P (Cl1, Cl3);             -- dynamic: error
      P (R1, Cl1);              -- static:  error
      P (Root'Class (R1), Cl1); -- dynamic: ok

---------------------------
Special case for equality
---------------------------

* Overriding the default equality for a :ada:`tagged` type involves the use of a function with multiple controlling operands
* As in general case, static types of operands have to be the same
* If dynamic types differ, equality returns false instead of raising exception

.. code:: Ada

   type Root is null tagged record;
   function "=" (L : Root; R : Root) return Boolean;
   type Child is new Root with null record;
   overriding function "=" (L : Child; R : Child) return Boolean;
   R1, R2 : Root;
   C1, C2 : Child;
   Cl1 : Root'Class := R1;
   Cl2 : Root'Class := R2;
   Cl3 : Root'Class := C1;
   ...
   -- overridden "=" called via dispatching
   if Cl1 = Cl2 then [...]
   if Cl1 = Cl3 then [...] -- returns false

--------------------------
Controlling result (1/2)
--------------------------

* The controlling operand may be the return type

   - This is known as the constructor pattern

      .. code:: Ada

         type Root is tagged null record;
         function F (V : Integer) return Root;

* If the child adds fields, all such subprograms have to be overridden

      .. code:: Ada

         type Root is tagged null record;
         function F (V : Integer) return Root;

         type Child is new Root with null record;
         --  OK, F is implicitly inherited

         type Child1 is new Root with record
            X : Integer;
         end record;
         --  ERROR no implicitly inherited function F

* Primitives returning abstract types have to be abstract

      .. code:: Ada

         type Root is abstract tagged null record;
         function F (V : Integer) return Root is abstract;

--------------------------
Controlling result (2/2)
--------------------------

* Primitives returning :ada:`tagged` types can be used in a static context

   .. code:: Ada

      type Root is tagged null record;
      function F return Root;
      type Child is new Root with null record;
      function F return Child;
      V : Root := F;

* In a dynamic context, the type has to be known to correctly dispatch

   .. code:: Ada

     V1 : Root'Class := Root'(F);  -- Static call to Root primitive
     V2 : Root'Class := V1;
     V3 : Root'Class := Child'(F); -- Static call to Child primitive
     V4 : Root'Class := F;         -- What is the tag of V4?
     ...
     V1 := F; -- Dispatching call to Root primitive
     V2 := F; -- Dispatching call to Root primitive
     V3 := F; -- Dispatching call to Child primitive

* No dispatching is possible when returning access types

=========
Summary
=========

---------
Summary
---------

* `'Class` operator

   - Allows subprograms to be used for multiple versions of a type

* Dispatching

   - Abstract types require concrete versions
   - Abstract subprograms allow template definitions

      + Need an implementation for each abstract type referenced

* Run-time call dispatch vs compile-time call dispatching

   - Compiler resolves appropriate call where it can
   - Run-time resolves appropriate call where it can
   - If not resolved, exception
