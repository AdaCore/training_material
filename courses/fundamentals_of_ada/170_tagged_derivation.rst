*****************
Tagged Derivation
*****************

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

---------------------------------------------
Object-Oriented Programming with Tagged Types
---------------------------------------------

* For :ada:`record` types

    .. code:: Ada

       type T is tagged record
       ...

* Child types can add new components (*attributes*)
* Object of a child type can be **substituted** for base type
* Primitive (*method*) can :dfn:`dispatch` **at run-time** depending on the type at call-site
* Types can be **extended** by other packages

    - Conversion and qualification to base type is allowed

* Private data is encapsulated through **privacy**

------------------------------
Tagged Derivation Ada Vs C++
------------------------------

.. container:: columns

 .. container:: column

    .. code:: Ada

       type T1 is tagged record
         Member1 : Integer;
       end record;

       procedure Attr_F (This : T1);

       type T2 is new T1 with record
         Member2 : Integer;
       end record;

       overriding procedure Attr_F (
            This : T2);
       procedure Attr_F2 (This : T2);

 .. container:: column

    .. code:: C++

       class T1 {
         public:
           int Member1;
           virtual void Attr_F(void);
         };

       class T2 : public T1 {
         public:
           int Member2;
           virtual void Attr_F(void);
           virtual void Attr_F2(void);
         };

=================
Tagged Derivation
=================

---------------------------------
Difference with Simple Derivation
---------------------------------

* Tagged derivation **can** change the structure of a type

    - Keywords :ada:`tagged record` and :ada:`with record`

   .. code:: Ada

      type Root is tagged record
         F1 : Integer;
      end record;

      type Child is new Root with record
         F2 : Integer;
      end record;

* Conversion is only allowed from **child to parent**

   .. code:: Ada

      V1 : Root;
      V2 : Child;
      ...
      V1 := Root (V2);
      V2 := Child (V1); -- illegal

------------
Primitives
------------

* Child **cannot remove** a primitive
* Child **can add** new primitives
* :dfn:`Controlling parameter`

    - Parameters the subprogram is a primitive of
    - For :ada:`tagged` types, all should have the **same type**

   .. code:: Ada

      type Root1 is tagged null record;
      type Root2 is tagged null record;

      procedure P1 (V1 : Root1;
                    V2 : Root1);
      procedure P2 (V1 : Root1;
                    V2 : Root2); -- illegal

-------------------------------
Freeze Point for Tagged Types
-------------------------------

* Freeze point definition does not change

   - A variable of the type is declared
   - The type is derived
   - The end of the scope is reached

* Declaring tagged type primitives past freeze point is **forbidden**

.. code:: Ada

   type Root is tagged null record;

   procedure Prim (V : Root);

   type Child is new Root with null record; -- freeze root

   procedure Prim2 (V : Root); -- illegal

   V : Child; --  freeze child

   procedure Prim3 (V : Child); -- illegal

---------------------
Overriding Indicators
---------------------

* Optional :ada:`overriding` and :ada:`not overriding` indicators

   .. code:: Ada

      type Shape_T is tagged record
         Name : String (1..10);
      end record;

      -- primitives of "Shape_T"
      function Get_Name (S : Shape_T) return String;
      procedure Set_Name (S : in out Shape_T);

      -- Derive "Point" from Shape_T
      type Point_T is new Shape_T with record
         Origin : Coord_T;
      end Point_T;

      -- Get_Name is inherited
      -- We want to _change_ the behavior of Set_Name
      overriding procedure Set_Name (P : in out Point_T);
      -- We want to _add_ a new primitive
      not overriding procedure Set_Origin (P : in out Point_T);

..
  language_version 2005

-----------------
Prefix Notation
-----------------

* Tagged types primitives can be called as usual
* The call can use prefixed notation

    - **If** the first argument is a controlling parameter
    - No need for :ada:`use` or :ada:`use type` for visibility

   .. code:: Ada

      -- Prim1 visible even without *use Pkg*
      X.Prim1;

      declare
         use Pkg;
      begin
         Prim1 (X);
      end;

..
  language_version 2005

------
Quiz
------

.. include:: quiz/tagged_primitives/quiz.rst

------
Quiz
------

.. include:: quiz/tagged_dot_and_with/quiz.rst

------
Quiz
------

Which code block(s) is (are) legal?

.. container:: columns

  .. container:: column

    A. | ``type A1 is record``
       |    ``Field1 : Integer;``
       | ``end record;``
       | ``type A2 is new A1 with null record;``
    B. | :answermono:`type B1 is tagged record`
       |    :answermono:`Field2 : Integer;`
       | :answermono:`end record;`
       | :answermono:`type B2 is new B1 with record`
       |    :answermono:`Field2b : Integer;`
       | :answermono:`end record;`

  .. container:: column

    C. | ``type C1 is tagged record``
       |    ``Field3 : Integer;``
       | ``end record;``
       | ``type C2 is new C1 with record``
       |    ``Field3 : Integer;``
       | ``end record;``
    D. | ``type D1 is tagged record``
       |    ``Field1 : Integer;``
       | ``end record;``
       | ``type D2 is new D1;``

.. container:: animate

   Explanations

   A. Cannot extend a non-tagged type
   B. Correct
   C. Components must have distinct names
   D. Types derived from a tagged type must have an extension

========================
Extending Tagged Types
========================

----------------------------------
How Do You Extend a Tagged Type?
----------------------------------

* Premise of a tagged type is to :dfn:`extend` an existing type

* In general, that means we want to add more fields

  * We can extend a :ada:`tagged` type by adding fields

  .. code:: Ada

    package Animals is
      type Animal_T is tagged record
        Age : Natural;
      end record;
    end Animals;

    with Animals; use Animals;
    package Mammals is
      type Mammal_T is new Animal_T with record
        Number_Of_Legs : Natural;
      end record;
    end Mammals;

    with Mammals; use Mammals;
    package Canines is
      type Canine_T is new Mammal_T with record
        Domesticated : Boolean;
      end record;
    end Canines;

------------------
Tagged Aggregate
------------------

* At initialization, all fields (including **inherited**) must have a **value**

   .. code:: Ada

     Animal : Animal_T := (Age => 1);
     Mammal : Mammal_T := (Age            => 2,
                           Number_Of_Legs => 2);
     Canine : Canine_T := (Age            => 2,
                           Number_Of_Legs => 4,
                           Domesticated   => True);

* But we can also "seed" the aggregate with a parent object

  .. code:: Ada

    Mammal := (Animal with Number_Of_Legs => 4);
    Canine := (Animal with Number_Of_Legs => 4,
                           Domesticated   => False);
    Canine := (Mammal with Domesticated => True);

----------------------
Private Tagged Types
----------------------

* But data hiding says types should be private!

* So we can define our base type as private

  .. container:: latex_environment tiny

    .. code:: Ada

      package Animals is
        type Animal_T is tagged private;
        function Get_Age (P : Animal_T) return Natural;
        procedure Set_Age (P : in out Animal_T; A : Natural);
      private
        type Animal_T is tagged record
           Age : Natural;
        end record;
      end Animals;

* And still allow derivation

  .. container:: latex_environment tiny

    .. code:: Ada

      with Animals;
      package Mammals is
        type Mammal_T is new Animals.Animal_T with record
          Number_Of_Legs : Natural;
        end record;

* But now the only way to get access to :ada:`Age` is with accessor subprograms

--------------------
Private Extensions
--------------------

* In the previous slide, we exposed the fields for :ada:`Mammal_T`!

* Better would be to make the extension itself private

  .. code:: Ada

    package Mammals is
      type Mammal_T is new Animals.Animal_T with private;
    private
      type Mammal_T is new Animals.Animal_T with record
         Number_Of_Legs : Natural;
      end record;
    end Mammals;

--------------------------------------
Aggregates with Private Tagged Types
--------------------------------------

* Remember, an aggregate must specify values for all components

  * But with private types, we can't see all the components!

* So we need to use the "seed" method:

  .. code:: Ada

    procedure Inside_Mammals_Pkg is
      Animal : Animal_T := Animals.Create;
      Mammal : Mammal_T;
    begin
      Mammal := (Animal with Number_Of_Legs => 4);
      Mammal := (Animals.Create with Number_Of_Legs => 4);
    end Inside_Mammals_Pkg;

* Note that we cannot use :ada:`others => <>` for components that are not visible to us

  .. code:: Ada

    Mammal := (Number_Of_Legs => 4,
               others         => <>);  -- Compile Error

-----------------
Null Extensions
-----------------

* To create a new type with no additional fields

  * We still need to "extend" the record - we just do it with an empty record

    .. code:: Ada

      type Dog_T is new Canine_T with null record;


* We still need to specify the "added" fields in an aggregate

  .. code:: Ada

    C    : Canine_T := Canines.Create;
    Dog1 : Dog_T := C; -- Compile Error
    Dog2 : Dog_T := (C with null record);

------
Quiz
------

Given the following code:

  .. code::ada

    package Parents is
      type Parent_T is tagged private;
      function Create return Parent_T;
    private
      type Parent_T is tagged record
         Id : Integer;
      end record;
    end Parents;

    with Parents; use Parents;
    package Children is
      P : Parent_T;
      type Child_T is new Parent_T with record
         Count : Natural;
      end record;
      function Create (C : Natural) return Child_T;
    end Children;

Which completion(s) of Create is (are) valid?

  A. :answermono:`function Create return Child_T is (Parents.Create with Count => 0);`
  B. ``function Create return Child_T is (others => <>);``
  B. ``function Create return Child_T is (0, 0);``
  D.  :answermono:`function Create return Child_T is (P with Count => 0);`

.. container:: animate

   Explanations

   A. Correct - :ada:`Parents.Create` returns :ada:`Parent_T`
   B. Cannot use :ada:`others` to complete private part of an aggregate
   C. Aggregate has no visibility to :ada:`Id` field, so cannot assign
   D. Correct - :ada:`P` is a :ada:`Parent_T`

========
Lab
========

.. include:: labs/170_tagged_derivation.lab.rst

=========
Summary
=========

---------
Summary
---------

* Tagged derivation

   - Building block for OOP types in Ada

* Primitives rules for tagged types are trickier

    - Primitives **forbidden** below freeze point
    - **Unique** controlling parameter
    - Tip: Keep the number of tagged type per package low
