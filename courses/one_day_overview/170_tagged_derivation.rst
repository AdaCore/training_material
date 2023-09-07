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

---------------------
Overriding Indicators
---------------------

.. admonition:: Language Variant

   Ada 2005

* Optional :ada:`overriding` and :ada:`not overriding` indicators

   .. code:: Ada

      type Shape_T is tagged record
         Name : String(1..10);
      end record;

      -- primitives of "Shape_T"
      procedure Set_Name (S : in out Shape_T);
      function Name (S : Shape_T) return string;

      -- Derive "Point" from Shape_T
      type Point is new Shape_T with record
         Origin : Coord_T;
      end Point;

      -- We want to _change_ the behavior of Set_Name
      overriding procedure Set_Name (P : in out Point_T);
      -- We want to _add_ a new primitive
      not overriding Origin (P : Point_T) return Point_T;
      -- We get "Name" for free

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
