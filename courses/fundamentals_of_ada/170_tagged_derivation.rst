
*************
Inheritance
*************


.. role:: ada(code)
    :language: Ada

==============
Introduction
==============

---------------------------------------
Object-Oriented Programming via Types
---------------------------------------

* Child types can add new components (*attributes*)
* Object of a child type can be **substituted** for base type
* Primitive (*method*) can **dispatch at runtime** depending on the type at call-site
* Types can be **extended** by other modules

-------------------------------------
Ada Mechanisms for Tagged Inheritance
-------------------------------------

* Only applies to :ada:`record` types
* Can add new components
* Casting and qualification to base type are allowed
* Dynamic dispatch

    - Handled through **class type**
    - More on that later

* Large control of visibility

=================
Tagged Derivation
=================

----------
Examples
----------

.. include:: examples/170_inheritance/tagged_derivation.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/170_inheritance.html#tagged-derivation`

---------------------------------
Difference with Simple Derivation
---------------------------------

* Tagged derivation extends simple derivation

    - Tagged derivation **can** change the structure of a type
    - Additional **restrictions** apply on primitives

* Keywords :ada:`tagged record` and :ada:`with record`

   .. code:: Ada
    
      type Root is tagged record
         F1 : Integer;
      end record;

      type Child is new Root with record
         F2 : Integer;
      end record;

------------------------------
Tagged Derivation Ada vs C++
------------------------------

.. container:: columns

 .. container:: column

    .. code:: Ada
    
       type T is tagged record
         Attr_D : Integer;
       end record;
       procedure Attr_F (This : T);
       type T2 is new T with record
         Attr_D2 : Integer;
       end record;
       overriding procedure Attr_F (This : T2);
       procedure Attr_F2 (This : T2);

 .. container:: column
    
    .. code:: C++
    
       class T {
         public:
           int Attr_D;
           virtual void Attr_F(void);
         };
       
       class T2 : public T {
         public:
           int Attr_D2;
           virtual void Attr_F(void);
           virtual void Attr_F2(void);
         };

--------------------------------------
Forbidden Operations in Tagged Types
--------------------------------------

* A tagged derivation **has** to be a type extension

    - Use :ada:`with null record` if there are no additional components

   .. code:: Ada

      type Child is new Root with null record;
      type Child is new Root; -- illegal

* A tagged derivation **cannot remove** primitives
* New primitive **are forbidden** below freeze point
* Conversions is only allowed from **child to parent**

   .. code:: Ada

      type Child is new Root with null record;

      V1 : Root;
      V2 : Child;
      ...
      V1 := Root (V2);
      V2 := Child (V1); -- illegal

------------
Primitives
------------

* Still true: implicitly inherited, can be overridden
* Child **cannot remove** a primitive
* Child **can add** new primitives
* Ada 2005 optional :ada:`overriding` and :ada:`not overriding` indicators

   .. code:: Ada
    
      type Root is tagged null record;
      procedure Prim1 (V : Root);
      procedure Prim2 (V : Root);
      type Child is new Root with null record;
      overriding procedure Prim1 (V : Child);
      not overriding procedure Prim3 (V : Child);
      -- implicitly inherited:
      -- procedure Prim2 (V : Child);

* **Controlling parameter**: Parameter the subprogram is a primitive of

    - For tagged types, all should have the **same type**

   .. code:: Ada
    
      type Root1 is tagged null record;
      type Root2 is tagged null record;
      procedure P1 (V1 : Root1;
                    V2 : Root1);
      procedure P2 (V1 : Root1;
                    V2 : Root2); -- illegal

------------------
Tagged Aggregate
------------------

* All fields of the type hierarchy must have value

   .. code:: Ada
    
       type Root is tagged record
           F1 : Integer;
         end record;
         type Child is new Root with
           record
           F2 : Integer;
         end record;
         V2 : Child := (F1 => 0, F2 => 0);

* Impossible when private types are involved
* **Aggregate extension**: copy of parent instance, or default initialization of the parent
* Use :ada:`with null record` if there are no additional components

   .. code:: Ada
    
      V  : Root := (F1 => 0);
      V2 : Child := (V with F2 => 0);
      V3 : Empty_Child := (V with null record);

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
   type Child is new Root
      with null record; -- freeze root
   procedure Prim2 (V : Root); -- illegal

   V : Child; --  freeze child
   procedure Prim3 (V : Child); -- illegal

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
    
      -- Prim1 visible even without **use Pkg**
      X.Prim1;

      use Pkg;
      Prim1 (X);

------
Quiz
------

Which code block is legal?

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

.. include:: labs/170_inheritance.lab.rst

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
