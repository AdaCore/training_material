
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

* Most object oriented languages allow user to add fields to derived types
* Objects of a type derived from a base type can be substituted for objects of the base type
* Subprogram (*method*) attached to object type can **dispatch at runtime** depending on exact type of the object
* Other modules can derive from your object type and define their own behaviors

-------------------------------------
Ada Mechanisms for Tagged Inheritance
-------------------------------------

* *Primitive* operations on type
* Define types from records to add new fields
* Can be handled through **class type**
    
    - Able to **dispatch dynamically**
    - More on that later

============
Primitives
============

----------
Examples
----------

.. include:: examples/170_inheritance/tagged_derivation.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/170_inheritance.html#tagged-derivation`

-------------------
Tagged Derivation
-------------------

* Tagged derivation extends simple derivation
    
    - Tagged derivation **can** change the structure of a type
    - Additional **restrictions** apply on primitives

* :ada:`tagged record`
    - Equivalent of a **class** in terms of OOP

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
       overriding
       procedure Attr_F (This : T2);
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

* A tagged derivation has to be a type extension
    
   .. code:: Ada
    
      type Root is tagged record
         F1 : Integer;
      end record;
      type Child is new Root; -- illegal
     
* A tagged derivation cannot remove primitives

*  Conversions from child to parent are allowed, but not the other way around (need extra fields to be provided)
    
   .. code:: Ada
    
      type Root is tagged record
          F1 : Integer;
        end record;
      type Child is new Root with record
          F2 : Integer;
        end record;
      V1 : Root  := (F1 => 0);
      V2 : Child := (F1 => 0, F2 => 0);
      ...
      V1 := Root (V2);
      V2 := Child (V1); -- illegal
      V2 := (V1 with F2 => 0);
     
------------
Primitives
------------

* As for regular types, primitives are implicitly inherited, and can be overridden
* A child can add new primitives
    
   .. code:: Ada
    
      type Root is tagged null record;
      procedure Prim1 (V : Root);
      procedure Prim2 (V : Root);
      type Child is new Root with null record;
      overriding procedure Prim1 (V : Child);
      not overriding procedure Prim3 (V : Child);
      -- implicitly inherited:
      -- procedure Prim2 (V : Child);
     
* The parameter which the subprogram is primitive of is called the controlling parameter
* All controlling parameters must be of the same type
    
   .. code:: Ada
    
      type Root1 is tagged null record;
      type Root2 is tagged null record;
      procedure P1 ( V1 : Root1;
                     V2 : Root1);
      procedure P2 ( V1 : Root1;
                     V2 : Root2); -- illegal
 
------------------
Tagged Aggregate
------------------

* Regular aggregate works - values must be given to all fields of the type hierarchy
    
   .. code:: Ada
    
       type Root is tagged record
           F1 : Integer;
         end record;
         type Child is new Root with
           record
           F2 : Integer;
         end record;
         V2 : Child := (F1 => 0, F2 => 0);
     
* Doesn't work if there are private types involved!

* Aggregate extension allows using a copy of parent instance, or default initialization of the parent
* `with null record` can be used when there are no additional components
    
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

* Primitives of tagged types can be called like any other
    
   .. code:: Ada
    
      type Root is tagged record
         F1 : Integer;
      end record;
      procedure Prim1 (V : Root);
      procedure Prim2 (V : access Root; V2 : Integer);
      type Root_Access is access all Root;
      X  : Root_Access := new Root;
      X2 : aliased Root;
      ...
      Prim1 (X.all);
      Prim2 (X2'Access, 5);
     
* When the first parameter is a controlling parameter, the call can be prefixed by the object
    
   .. code:: Ada
    
      X.Prim1;
      X.all.Prim1;
      X.Prim2 (5);
      X2'Access.Prim2 (5);
     
* No `use` or `use type` clause is needed to have visibility over the primitives in this case

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
