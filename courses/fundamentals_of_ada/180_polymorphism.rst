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

* Runtime call dispatch vs compile-time call dispatching

==================
Classes of Types
==================

----------
Classes
----------

* In Ada, a Class denotes an inheritance subtree
* Class of `Root` is the class of `Root` and all its children
* Type :ada:`Root'Class` can designate any object typed after type of class of `Root`

   .. code:: Ada

      type Root is tagged null record;
      type Child1 is new Root with null record;
      type Child2 is new Root with null record;
      type Grand_Child1 is new Child1 with null record;
      -- Root'Class = {Root, Child1, Child2, Grand_Child1}
      -- Child1'Class = {Child1, Grand_Child1}
      -- Child2'Class = {Child2}
      -- Grand_Child1'Class = {Grand_Child1}

* Objects of type :ada:`Root'Class` have at least the properties of `Root`

   - Fields of `Root`
   - Primitives of `Root`

-----------------
Indefinite Type
-----------------

* A class wide type is an indefinite type

   - Just like an unconstrained array or a record with a discriminant

* Properties and constraints of indefinite types apply

   - Can be used for parameter declarations
   - Can be used for variable declaration with initialization

.. code:: Ada

   procedure Main is
      type Root_Type is tagged null record;
      type Derived_Type is new Root_Type with null record;
      procedure Some_Procedure (X : in out Root_Type'Class) is null;
      Obj : Derived_Type;
      Derived_Class  : Derived_Type'Class := Obj;
      Root_Class_1 : Root_Type'Class := Derived_Class;
      Root_Class_2 : Root_Type'Class := Obj;
      -- initialization required in class-wide declaration
      Root_Class_3 : Root_Type'Class;       -- compile error
      Derived_Class_2 : Derived_Type'Class; -- compile error
   begin
      Some_Procedure (Derived_Class);
      Some_Procedure (Obj);
   end Main;

-------------------------------
Testing the Type of an Object
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
   Child_Class    : Child'Class := Child (Parent_Class_2);
                    -- Child_Class'Tag  = Child'Tag

   B1 : Boolean := Parent_Class_1 in Parent'Class; -- True
   B2 : Boolean := Parent_Class_1'Tag = Child'Tag; -- False
   B3 : Boolean := Child_Class'Tag = Parent'Tag;   -- False
   B4 : Boolean := Child_Class in Child'Class;     -- True

----------------
Abstract Types
----------------

* A tagged type can be declared :ada:`abstract`
* Then, :ada:`abstract tagged` types:

   - cannot be instantiated
   - can have abstract subprograms (with no implementation)
   - Non-abstract derivation of an abstract type must override and implement abstract subprograms

---------------------------
Abstract Types Ada Vs C++
---------------------------

* Ada

    .. code:: Ada

       type Root is abstract tagged record
          Field : Integer;
       end record;
       procedure P1 (The_Record : Root) is abstract;
       procedure P2 (The_Record : Root);
       type Child is abstract new Root with null record;
       type Grand_Child is new Child with null record;

       overriding  -- Ada 2005 and later
       procedure P1 (The_Record : Grand_Child);

* C++

    .. code:: Ada

       class Root {
          public:
             int Field;
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

Warning: Subprograms with parameter of type `Root'Class` are not primitives of `Root`

      .. code:: Ada

         type Root is tagged null record;
         procedure Some_Procedure (Some_Param : Root'Class);
         type Child is new Root with null record;
         -- This does not override Some_Procedure!
         overriding procedure Some_Procedure (Some_Param : Child'Class);

----------------------------
'Class and Prefix Notation
----------------------------

Prefix notation rules apply when the first parameter is of a class wide type

      .. code:: Ada

         type Root is tagged null record;
         procedure Some_Procedure (Some_Param : Root'Class);
         type Child is new Root with null record;

         Var_1 : Root;
         Var_2 : Root'Class := Root'(others => <>);
         ...
         Some_Procedure (Var_1);
         Some_Procedure (Var_2);
         Var_1.Some_Procedure;
         Var_2.Some_Procedure;

..
  language_version 2005

===============================
Dispatching and Redispatching
===============================

---------------------------------
Calls on Class-Wide Types (1/3)
---------------------------------

* Any subprogram expecting a `Root` object can be called with a :ada:`Root'Class` object

.. code:: Ada

   type Root is tagged null record;
   procedure Some_Procedure (The_Record : Root);

   type Child is new Root with null record;
   procedure Some_Procedure (The_Record : Child);

      Var_1 : Root'Class := [...]
      Var_2 : Child'Class := [...]
   begin
      Some_Procedure (Var_1);
      Some_Procedure (Var_2);

---------------------------------
Calls on Class-Wide Types (2/3)
---------------------------------

* The *actual* type of the object is not known at compile time
* The *right* type will be selected at run-time

.. container:: columns

 .. container:: column

   *Ada*

      .. code:: Ada

         declare
           Var_1 : Root'Class :=
                Root'(others => <>);
           Var_2 : Root'Class :=
                Child'(others => <>);
         begin
           Var_1.Some_Procedure; -- calls Some_Procedure of Root
           Var_2.Some_Procedure; -- calls Some_Procedure of Child

 .. container:: column

   *C++*

      .. code:: C++

         Root * Var_1 = new Root ();
         Root * Var_2 = new Child ();
         Var_1->Some_Procedure ();
         Var_2->Some_Procedure ();

---------------------------------
Calls on Class-Wide Types (3/3)
---------------------------------

* It is still possible to force a call to be static using a conversion of view

.. container:: columns

 .. container:: column

   *Ada*

   .. code:: Ada

      declare
        Var_1 : Root'Class :=
             Root'(others => <>);
        Var_2 : Root'Class :=
             Child'(others => <>);
      begin
        Root (Var_1).Some_Procedure; -- calls Some_Procedure of Root
        Root (Var_2).Some_Procedure; -- calls Some_Procedure of Root

 .. container:: column

   *C++*

   .. code:: C++

      Root * Var_1 = new Root ();
      Root * Var_2 = new Child ();
      ((Root) *Var_1).Some_Procedure ();
      ((Root) *Var_2).Some_Procedure ();

-------------------------------
Definite and Class Wide Views
-------------------------------

* In C++, dispatching occurs only on pointers
* In Ada, dispatching occurs only on class wide views

.. code:: Ada

   type Root is tagged null record;
   procedure Primitive_1 (The_Record : Root);
   procedure Primitive_2 (The_Record : Root);
   type Child is new Root with null record;
   overriding procedure Primitive_2 (The_Record : Child);
   procedure Primitive_1 (The_Record : Root) is
   begin
      Primitive_2 (The_Record); -- always calls Primitive_2 from Root
   end Primitive_1;
   procedure Main is
      A_Record : Root'Class :=
           Child'(others => <>);
   begin
      -- Calls Primitive_1 from the implicitly overridden subprogram
      -- Calls Primitive_2 from Root!
      A_Record.Primitive_1;

.. container:: speakernote

   Primitive_1 operates on ROOT, not ROOT'Class

---------------
Redispatching
---------------

* :ada:`tagged` types are always passed by reference

   - The original object is not copied

* Therefore, it is possible to convert them to different views

.. code:: Ada

   type Root is tagged null record;
   procedure Primitive_1 (The_Record : Root);
   procedure Primitive_2 (The_Record : Root);
   type Child is new Root with null record;
   overriding procedure Primitive_2 (The_Record : Child);

-----------------------
Redispatching Example
-----------------------

.. code:: Ada

   procedure Primitive_1 (The_Record : Root) is
      V_Class : Root'Class renames
                Root'Class (The_Record);     -- naming of a view
   begin
      Primitive_2 (The_Record);              -- static: uses the definite view
      Primitive_2 (Root'Class (The_Record)); -- dynamic: (redispatching)
      Primitive_2 (V_Class);                 -- dynamic: (redispatching)

      -- Ada 2005 "distinguished receiver" syntax
      The_Record.Primitive_2;                -- static: uses the definite view
      Root'Class (The_Record).Primitive_2;   -- dynamic: (redispatching)
      V_Class.Primitive_2;                   -- dynamic: (redispatching)
   end Primitive_1;

------
Quiz
------

.. code::Ada

   package The_Package is
      type Root is tagged null record;
      function Function_1 (The_Record : Root) return Integer is (101);
      type Child is new Root with null record;
      function Function_1 (The_Record : Child) return Integer is (201);
      type Grandchild is new Child with null record;
      function Function_1 (The_Record : Grandchild) return Integer is (301);
   end The_Package;

   with The_Package; use The_Package;
   procedure Main is
      Record_Object : Root'Class := Grandchild'(others => <>);

What is the value returned by :ada:`F1 (Child'Class (Record_Object));`?

   A. :answer:`301`
   B. 201
   C. 101
   D. Compilation error

.. container:: animate

   Explanations

   A. Correct
   B. Would be correct if Record_Object was a :ada:`Child` - :ada:`Child'Class` leaves the object as :ada:`Grandchild`
   C. Object is initialized to something in :ada:`Root'Class`, but it doesn't have to be :ada:`Root`
   D. Would be correct if function parameter types were :ada:`'Class`

===============================
Exotic Dispatching Operations
===============================

-------------------------------
Multiple Dispatching Operands
-------------------------------

* Primitives with multiple dispatching operands are allowed if all operands are of the same type

   .. code:: Ada

      type Root is tagged null record;
      procedure Root_Proc (Left : Root; Right : Root);
      type Child is new Root with null record;
      overriding procedure Root_Proc (Left : Child; Right : Child);

* At call time, all actual parameters' tags have to match, either statically or dynamically

   .. code:: Ada

      Root_1, Root_2   : Root;
      Child_1, Child_2 : Child;
      Class_Wide_1 : Root'Class := Root_1;
      Class_Wide_2 : Root'Class := Root_2;
      Class_Wide_3 : Root'Class := Child_1;
      ...
      Root_Proc (Root_1, Root_2);                     -- static:  ok
      Root_Proc (Root_1, Child_1);                    -- static:  error
      Root_Proc (Class_Wide_1, Class_Wide_2);         -- dynamic: ok
      Root_Proc (Class_Wide_1, Class_Wide_3);         -- dynamic: error
      Root_Proc (Root_1, Class_Wide_1);               -- static:  error
      PRoot_Proc (Root'Class (Root_1), Class_Wide_1); -- dynamic: ok

---------------------------
Special Case for Equality
---------------------------

* Overriding the default equality for a :ada:`tagged` type involves the use of a function with multiple controlling operands
* As in general case, static types of operands have to be the same
* If dynamic types differ, equality returns false instead of raising exception

.. code:: Ada

   type Root is tagged null record;
   function "=" (Left : Root; Right : Root) return Boolean;
   type Child is new Root with null record;
   overriding function "=" (Left : Child; Right : Child) return Boolean;
   Root_1, Root_2   : Root;
   Child_1, Child_2 : Child;
   Class_Wide_1 : Root'Class := Root_1;
   Class_Wide_2 : Root'Class := Root_2;
   Class_Wide_3 : Root'Class := Child_1;
   ...
   -- overridden "=" called via dispatching
   if Class_Wide_1 = Class_Wide_2 then [...]
   if Class_Wide_1 = Class_Wide_3 then [...] -- returns false

--------------------------
Controlling Result (1/2)
--------------------------

* The controlling operand may be the return type

   - This is known as the constructor pattern

      .. code:: Ada

         type Root is tagged null record;
         function Root_Function (Some_Var : Integer) return Root;

* If the child adds fields, all such subprograms have to be overridden

      .. code:: Ada

         type Root is tagged null record;
         function Root_Function (Some_Var : Integer) return Root;

         type Child is new Root with null record;
         --  OK, Root_Function is implicitly inherited

         type Child1 is new Root with record
            Field : Integer;
         end record;
         --  ERROR no implicitly inherited function Root_Function

* Primitives returning abstract types have to be abstract

      .. code:: Ada

         type Root is abstract tagged null record;
         function Root_Function (Some_Var : Integer) return Root is abstract;

--------------------------
Controlling Result (2/2)
--------------------------

* Primitives returning :ada:`tagged` types can be used in a static context

   .. code:: Ada

      type Root is tagged null record;
      function Root_Function return Root;
      type Child is new Root with null record;
      function Root_Function return Child;
      Static_Record : Root := Root_Function;

* In a dynamic context, the type has to be known to correctly dispatch

   .. code:: Ada

     Static_Rec_1  : Root'Class := Root'(Root_Function);  -- Static call to Root primitive
     Static_Rec_2  : Root'Class := Static_Rec_1;
     Static_Rec_3  : Root'Class := Child'(Root_Function); -- Static call to Child primitive
     Broken_Record : Root'Class := Root_Function;         -- Error - ambiguous expression
     ...
     Static_Rec_1 := Root_Function; -- Dispatching call to Root primitive
     Static_Rec_2 := Root_Function; -- Dispatching call to Root primitive
     Static_Rec_3 := Root_Function; -- Dispatching call to Child primitive

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

* :ada:`'Class` attribute

   - Allows subprograms to be used for multiple versions of a type

* Dispatching

   - Abstract types require concrete versions
   - Abstract subprograms allow template definitions

      + Need an implementation for each abstract type referenced

* Runtime call dispatch vs compile-time call dispatching

   - Compiler resolves appropriate call where it can
   - Runtime resolves appropriate call where it can
   - If not resolved, exception
