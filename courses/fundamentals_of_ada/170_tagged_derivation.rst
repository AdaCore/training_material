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

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

---------------------------------------------
Object-Oriented Programming With Tagged Types
---------------------------------------------

* For :ada:`record` types

    .. code:: Ada

       type T is tagged record
       ...

* Child types can add new components (*attributes*)
* Object of a child type can be **substituted** for base type
* Primitive (*method*) can :dfn:`dispatch` **at runtime** depending on the type at call-site
* Types can be **extended** by other packages

    - Casting and qualification to base type is allowed

* Private data is encapsulated through **privacy**

------------------------------
Tagged Derivation Ada vs C++
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

----------
Examples
----------

.. include:: examples/170_tagged_derivation/tagged_derivation.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/170_tagged_derivation.html#tagged-derivation`

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

--------------
Type Extension
--------------

* A tagged derivation **has** to be a type extension

    - Use :ada:`with null record` if there are no additional components

   .. code:: Ada

      type Child is new Root with null record;
      type Child is new Root; -- illegal

* Conversions is only allowed from **child to parent**

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
Freeze Point For Tagged Types
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

------------------
Tagged Aggregate
------------------

* At initialization, all fields (including **inherited**) must have a **value**

   .. code:: Ada

       type Root is tagged record
           F1 : Integer;
       end record;

       type Child is new Root with record
           F2 : Integer;
       end record;

       V : Child := (F1 => 0, F2 => 0);

* For **private types** use :dfn:`aggregate extension`

    - Copy of a parent instance
    - Use :ada:`with null record` absent new fields

   .. code:: Ada

      V2 : Child := (Parent_Instance with F2 => 0);
      V3 : Empty_Child := (Parent_Instance with null record);

---------------------
Overriding Indicators
---------------------

.. admonition:: Language Variant

   Ada 2005

* Optional :ada:`overriding` and :ada:`not overriding` indicators

   .. code:: Ada

      type Root is tagged null record;

      procedure Prim1 (V : Root);
      procedure Prim2 (V : Root);

      type Child is new Root with null record;

      overriding procedure Prim1 (V : Child);
      -- Prim2 (V : Child) is implicitely inherited
      not overriding procedure Prim3 (V : Child);

-----------------
Prefix Notation
-----------------

.. admonition:: Language Variant

   Ada 2012

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

Which code block is legal?

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
