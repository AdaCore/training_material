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
      type Animal is tagged null record;
      type Dog is new Animal with null record;
      procedure Handle_Animal (Some_Animal : in out Animal'Class) is null;
      My_Dog     : Dog;
      Pet        : Dog'Class    := My_Dog;
      Pet_Animal : Animal'Class := Pet;
      Pet_Dog    : Animal'Class := My_Dog;
      -- initialization required in class-wide declaration
      Bad_Animal : Animal'Class; -- compile error
      Bad_Dog    : Dog'Class;    -- compile error
   begin
      Handle_Animal (Pet);
      Handle_Animal (My_Dog);
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

       type Animal is abstract tagged record
          Number_Of_Eyes : Integer;
       end record;
       procedure Feed (The_Animal : Animal) is abstract;
       procedure Pet (The_Animal : Animal);
       type Dog is abstract new Animal with null record;
       type Bulldog is new Dog with null record;

       overriding  -- Ada 2005 and later
       procedure Feed (The_Animal : Bulldog);

* C++

    .. code:: Ada

       class Animal {
          public:
             int Number_Of_Eyes;
             virtual void Feed (void) = 0;
             virtual void Pet (void);
       };
       class Dog : public Animal {
       };
       class Bulldog {
          public:
             virtual void Feed (void);
       };

.. container:: speakernote

   "overriding" keyword is optional

------------------------
Relation to Primitives
------------------------

Warning: Subprograms with parameter of type `Root'Class` are not primitives of `Root`

      .. code:: Ada

         type Root is tagged null record;
         procedure Not_A_Primitive (Param : Root'Class);
         type Child is new Root with null record;
         -- This does not override Not_A_Primitive!
         overriding procedure Not_A_Primitive (Param : Child'Class);

----------------------------
'Class and Prefix Notation
----------------------------

Prefix notation rules apply when the first parameter is of a class wide type

      .. code:: Ada

         type Animal is tagged null record;
         procedure Handle_Animal (Some_Animal : Animal'Class);
         type Cat is new Animal with null record;

         Stray_Animal : Animal;
         Pet_Animal   : Animal'Class := Animal'(others => <>);
         ...
         Handle_Animal (Stray_Animal);
         Handle_Animal (Pet_Animal);
         Stray_Animal.Handle_Animal;
         Pet_Animal.Handle_Animal;

..
  language_version 2005

===============================
Dispatching and Redispatching
===============================

---------------------------------
Calls on Class-Wide Types (1/3)
---------------------------------

* Any subprogram expecting a `Root` object can be called with a :ada:`Animal'Class` object

.. code:: Ada

   type Animal is tagged null record;
   procedure Feed (The_Animal : Animal);

   type Dog is new Animal with null record;
   procedure Feed (The_Dog : Dog);

      Stray_Dog : Animal'Class := [...]
      My_Dog    : Dog'Class := [...]
   begin
      Feed (Stray_Dog);
      Feed (My_Dog);

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
           Stray : Animal'Class :=
                Animal'(others => <>);
           My_Dog : Animal'Class :=
                Dog'(others => <>);
         begin
           Stray.Feed;  -- calls Feed of Animal
           My_Dog.Feed; -- calls Feed of Dog

 .. container:: column

   *C++*

      .. code:: C++

         Animal * Stray = 
                     new Animal ();
         Animal * My_Dog = new Dog ();
         Stray->Feed ();
         My_Dog->Feed ();

---------------------------------
Calls on Class-Wide Types (3/3)
---------------------------------

* It is still possible to force a call to be static using a conversion of view

.. container:: columns

 .. container:: column

   *Ada*

   .. code:: Ada

      declare
        Stray : Animal'Class :=
             Animal'(others => <>);
        My_Dog : Animal'Class :=
             Dog'(others => <>);
      begin
        Animal (Stray).Feed;  -- calls Feed of Animal
        Animal (My_Dog).Feed; -- calls Feed of Animal

 .. container:: column

   *C++*

   .. code:: C++

      Animal * Stray = 
                  new Animal ();
      Animal * My_Dog = new Dog ();
      ((Animal) *Stray).Feed ();
      ((Animal) *My_Dog).Feed ();

-------------------------------
Definite and Class Wide Views
-------------------------------

* In C++, dispatching occurs only on pointers
* In Ada, dispatching occurs only on class wide views

.. code:: Ada

   type Animal is tagged null record;
   procedure Groom (The_Animal : Animal);
   procedure Give_Treat (The_Animal : Animal);
   type Dog is new Animal with null record;
   overriding procedure Give_Treat (The_Dog : Dog);
   procedure Groom (The_Animal : Animal) is
   begin
      Give_Treat (The_Animal); -- always calls Give_Treat from Animal
   end Groom;
   procedure Main is
      My_Dog : Animal'Class :=
           Dog'(others => <>);
   begin
      -- Calls Groom from the implicitly overridden subprogram
      -- Calls Give_Treat from Animal!
      My_Dog.Groom;

.. container:: speakernote

   Groom operates on Animal, not Animal'Class

---------------
Redispatching
---------------

* :ada:`tagged` types are always passed by reference

   - The original object is not copied

* Therefore, it is possible to convert them to different views

.. code:: Ada

   type Animal is tagged null record;
   procedure Feed (An_Animal : Animal);
   procedure Pet (An_Animal : Animal);
   type Cat is new Animal with null record;
   overriding procedure Pet (A_Cat : Cat);

-----------------------
Redispatching Example
-----------------------

.. code:: Ada

   procedure Feed (Anml : Animal) is
      Fish : Animal'Class renames
                Animal'Class (Anml); -- naming of a view
   begin
      Pet (Anml); -- static: uses the definite view
      Pet (Animal'Class (Anml)); -- dynamic: (redispatching)
      Pet (Fish);                -- dynamic: (redispatching)

      -- Ada 2005 "distinguished receiver" syntax
      Anml.Pet; -- static: uses the definite view
      Animal'Class (Anml).Pet; -- dynamic: (redispatching)
      Fish.Pet;                -- dynamic: (redispatching)
   end Feed;

------
Quiz
------

.. code::Ada

   package Robots is
      type Robot is tagged null record;
      function Service_Code (The_Bot : Robot) return Integer is (101);
      type Appliance_Robot is new Robot with null record;
      function Service_Code (The_Bot : Appliance_Robot) return Integer is (201);
      type Vacuum_Robot is new Appliance_Robot with null record;
      function Service_Code (The_Bot : Vacuum_Robot) return Integer is (301);
   end Robots;

   with Robots; use Robots;
   procedure Main is
      Robot_Object : Robot'Class := Vacuum_Robot'(others => <>);

What is the value returned by :ada:`Service_Code (Appliance_Robot'Class (Robot_Object));`?

   A. :answer:`301`
   B. 201
   C. 101
   D. Compilation error

.. container:: animate

   Explanations

   A. Correct
   B. Would be correct if :ada:`Robot_Object` was a :ada:`Appliance_Robot` - :ada:`Appliance_Robot'Class` leaves the object as :ada:`Vacuum_Robot`
   C. Object is initialized to something in :ada:`Robot'Class`, but it doesn't have to be :ada:`Robot`
   D. Would be correct if function parameter types were :ada:`'Class`

===============================
Exotic Dispatching Operations
===============================

-------------------------------
Multiple Dispatching Operands
-------------------------------

* Primitives with multiple dispatching operands are allowed if all operands are of the same type

   .. code:: Ada

      type Animal is tagged null record;
      procedure Interact (Left : Animal; Right : Animal);
      type Dog is new Animal with null record;
      overriding procedure Interact (Left : Dog; Right : Dog);

* At call time, all actual parameters' tags have to match, either statically or dynamically

   .. code:: Ada

      Animal_1, Animal_2   : Animal;
      Dog_1, Dog_2 : Dog;
      Any_Animal_1 : Animal'Class := Animal_1;
      Any_Animal_2 : Animal'Class := Animal_2;
      Dog_Animal : Animal'Class := Dog_1;
      ...
      Interact (Animal_1, Animal_2);                    -- static:  ok
      Interact (Animal_1, Dog_1);                       -- static:  error
      Interact (Any_Animal_1, Any_Animal_2);            -- dynamic: ok
      Interact (Any_Animal_1, Dog_Animal);              -- dynamic: error
      Interact (Animal_1, Any_Animal_1);                -- static:  error
      Interact (Animal'Class (Animal_1), Any_Animal_1); -- dynamic: ok

---------------------------
Special Case for Equality
---------------------------

* Overriding the default equality for a :ada:`tagged` type involves the use of a function with multiple controlling operands
* As in general case, static types of operands have to be the same
* If dynamic types differ, equality returns false instead of raising exception

.. code:: Ada

   type Animal is tagged null record;
   function "=" (Left : Animal; Right : Animal) return Boolean;
   type Dog is new Animal with null record;
   overriding function "=" (Left : Dog; Right : Dog) return Boolean;
   Animal_1, Animal_2 : Animal;
   Dog_1, Dog_2 : Child;
   Any_Animal_1 : Animal'Class := Animal_1;
   Any_Animal_2 : Animal'Class := Animal_2;
   Dog_Animal   : Animal'Class := Dog_1;
   ...
   -- overridden "=" called via dispatching
   if Any_Animal_1 = Any_Animal_2 then [...]
   if Any_Animal_1 = Dog_Animal then [...] -- returns false

--------------------------
Controlling Result (1/2)
--------------------------

* The controlling operand may be the return type

   - This is known as the constructor pattern

      .. code:: Ada

         type Animal is tagged null record;
         function Feed_Treats (Number_Of_Treats : Integer) return Animal;

* If the child adds fields, all such subprograms have to be overridden

      .. code:: Ada

         type Animal is tagged null record;
         function Feed_Treats (Number_Of_Treats : Integer) return Animal;

         type Dog is new Animal with null record;
         --  OK, Feed_Treats is implicitly inherited

         type Bulldog is new Animal with record
            Has_Underbite : Boolean;
         end record;
         --  ERROR no implicitly inherited function Feed_Treats

* Primitives returning abstract types have to be abstract

      .. code:: Ada

         type Animal is abstract tagged null record;
         function Feed_Treats (Number_Of_Treats : Integer) return Animal is abstract;

--------------------------
Controlling Result (2/2)
--------------------------

* Primitives returning :ada:`tagged` types can be used in a static context

   .. code:: Ada

      type Animal is tagged null record;
      function Feed return Animal;
      type Dog is new Animal with null record;
      function Feed return Dog;
      Fed_Animal : Animal := Feed;

* In a dynamic context, the type has to be known to correctly dispatch

   .. code:: Ada
     
     Fed_Animal : Animal'Class := 
                           Animal'(Feed);    -- Static call to Animal primitive
     Another_Fed_Animal : Animal'Class := Fed_Animal;
     Fed_Dog : Animal'Class := Dog'(Feed);   -- Static call to Dog primitive
     Starving_Animal : Animal'Class := Feed; -- Error - ambiguous expression
     ...
     Fed_Animal := Feed;         -- Dispatching call to Animal primitive
     Another_Fed_Animal := Feed; -- Dispatching call to Animal primitive
     Fed_Dog := Feed;            -- Dispatching call to Dog primitive

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
