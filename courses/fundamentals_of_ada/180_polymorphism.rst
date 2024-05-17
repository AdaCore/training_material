**************
Polymorphism
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
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

--------------
Introduction
--------------

* :ada:`'Class` operator to categorize :dfn:`classes of types`
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
   B2 : Boolean := Parent_Class_1'Tag = Child'Tag; -- False
   B3 : Boolean := Child_Class'Tag = Parent'Tag;   -- False
   B4 : Boolean := Child_Class in Child'Class;           -- True

----------------
Abstract Types
----------------

* A tagged type can be declared :ada:`abstract`
* Then, :ada:`abstract tagged` types:

   - cannot be instantiated
   - can have abstract subprograms (with no implementation)
   - Non-abstract derivation of an abstract type must override and implement abstract subprograms

---------------------------
Abstract Types Ada vs C++
---------------------------

* Ada

    .. code:: Ada

       type Root is abstract tagged record
          F : Integer;
       end record;
       procedure P1 (V : Root) is abstract;
       procedure P2 (V : Root);
       type Child is abstract new Root with null record;
       type Grand_Child is new Child with null record;

       overriding  -- Ada 2005 and later
       procedure P1 (V : Grand_Child);

* C++

    .. code:: Ada

       class Root {
          public:
             int F;
             virtual void P1 (void) = 0;
             virtual void P2 (void);
       };
       class Child : public Root {
       };
       class Grand_Child {
          public:
             virtual void P1 (void);
       };

.. container:: speakernote

   "overriding" keyword is optional

------------------------
Relation to Primitives
------------------------

Warning: Subprograms with parameter of type `T'Class` are not primitives of `T`

      .. code:: Ada

         type Root is tagged null record;
         procedure P (V : Root'Class);
         type Child is new Root with null record;
         -- This does not override P!
         overriding procedure P (V : Child'Class);

----------------------------
'Class and Prefix Notation
----------------------------

.. admonition:: Language Variant

   Ada 2012

Prefix notation rules apply when the first parameter is of a class wide type

      .. code:: Ada

         type Root is tagged null record;
         procedure P (V : Root'Class);
         type Child is new Root with null record;

         V1 : Root;
         V2 : Root'Class := Root'(others => <>);
         ...
         P (V1);
         P (V2);
         V1.P;
         V2.P;

===============================
Dispatching and Redispatching
===============================

---------------------------------
Calls on class-wide types (1/3)
---------------------------------

* Any subprogram expecting a T object can be called with a :ada:`T'Class` object

.. code:: Ada

   type Root is tagged null record;
   procedure P (V : Root);

   type Child is new Root with null record;
   procedure P (V : Child);

      V1 : Root'Class := [...]
      V2 : Child'Class := [...]
   begin
      P (V1);
      P (V2);

---------------------------------
Calls on class-wide types (2/3)
---------------------------------

* The *actual* type of the object is not known at compile time
* The *right* type will be selected at runtime

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

---------------------------------
Calls on class-wide types (3/3)
---------------------------------

* It is still possible to force a call to be static using a conversion of view

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
        Root (V1).P; -- calls P of Root
        Root (V2).P; -- calls P of Root

 .. container:: column

   *C++*

   .. code:: C++

      Root * V1 = new Root ();
      Root * V2 = new Child ();
      ((Root) *V1).P ();
      ((Root) *V2).P ();

-------------------------------
Definite and class wide views
-------------------------------

* In C++, dispatching occurs only on pointers
* In Ada, dispatching occurs only on class wide views

.. code:: Ada

   type Root is tagged null record;
   procedure P1 (V : Root);
   procedure P2 (V : Root);
   type Child is new Root with null record;
   overriding procedure P2 (V : Child);
   procedure P1 (V : Root) is
   begin
      P2 (V); -- always calls P2 from Root
   end P1;
   procedure Main is
      V1 : Root'Class :=
           Child'(others => <>);
   begin
      -- Calls P1 from the implicitly overridden subprogram
      -- Calls P2 from Root!
      V1.P1;

.. container:: speakernote

   P1 operates on ROOT, not ROOT'Class

---------------
Redispatching
---------------

* :ada:`tagged` types are always passed by reference

   - The original object is not copied

* Therefore, it is possible to convert them to different views

.. code:: Ada

   type Root is tagged null record;
   procedure P1 (V : Root);
   procedure P2 (V : Root);
   type Child is new Root with null record;
   overriding procedure P2 (V : Child);

-----------------------
Redispatching Example
-----------------------

.. code:: Ada

   procedure P1 (V : Root) is
      V_Class : Root'Class renames
                Root'Class (V); -- naming of a view
   begin
      P2 (V);              -- static: uses the definite view
      P2 (Root'Class (V)); -- dynamic: (redispatching)
      P2 (V_Class);        -- dynamic: (redispatching)

      -- Ada 2005 "distinguished receiver" syntax
      V.P2;                -- static: uses the definite view
      Root'Class (V).P2;   -- dynamic: (redispatching)
      V_Class.P2;          -- dynamic: (redispatching)
   end P1;

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

   with P; use P;
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
   B. Would be correct if Z was a :ada:`Child` - :ada:`Child'Class` leaves the object as :ada:`Grandchild`
   C. Object is initialized to something in :ada:`Root'Class`, but it doesn't have to be :ada:`Root`
   D. Would be correct if function parameter types were :ada:`'Class`

===============================
Exotic Dispatching Operations
===============================

-------------------------------
Multiple dispatching operands
-------------------------------

* Primitives with multiple dispatching operands are allowed if all operands are of the same type

   .. code:: Ada

      type Root is tagged null record;
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

   type Root is tagged null record;
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
     V4 : Root'Class := F;         -- Error - ambiguous expression
     ...
     V1 := F; -- Dispatching call to Root primitive
     V2 := F; -- Dispatching call to Root primitive
     V3 := F; -- Dispatching call to Child primitive

* No dispatching is possible when returning access types

========
Lab
========

.. include:: labs/180_polymorphism.lab.rst

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
