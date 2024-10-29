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

       type T2 is new T1
         with record
           Member2 : Integer;
         end record;

       overriding
       procedure Attr_F
          (This : T2);
       procedure Attr_F2
          (This : T2);

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
