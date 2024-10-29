************
Genericity
************

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

-------------------------
The Notion of a Pattern
-------------------------

* Sometimes algorithms can be abstracted from types and subprograms

   .. code:: Ada

      procedure Swap_Int (Left, Right : in out Integer) is
        V : Integer := Left;
      begin
         Left := Right;
         Right := V;
      end Swap_Int;

      procedure Swap_Bool (Left, Right : in out Boolean) is
         V : Boolean := Left;
      begin
         Left := Right;
         Right := V;
      end Swap_Bool;

* It would be nice to extract these properties in some common pattern, and then just replace the parts that need to be replaced

   .. code:: Ada

      procedure Swap (Left, Right : in out (Integer | Boolean)) is
        V : (Integer | Boolean) := Left;
      begin
         Left := Right;
         Right := V;
      end Swap;

--------------------------------------
Ada Generic Compared to C++ Template
--------------------------------------

.. container:: columns

 .. container:: column

  Ada Generic

  .. container:: latex_environment scriptsize

    .. code:: Ada

      -- specification
      generic
        type T is private;
      procedure Swap (L, R : in out T);

      -- implementation
      procedure Swap (L, R : in out T) is
         Tmp : T := L;
      begin
         L := R;
         R := Tmp;
      end Swap;

      -- instance
      procedure Swap_F is new Swap (Float);

 .. container:: column

  C++ Template

  .. container:: latex_environment scriptsize

    .. code:: C++

      // prototype
      template <class T>
      void Swap (T & L, T & R);

      // implementation
      template <class T>
      void Swap (T & L, T & R) {
         T Tmp = L;
         L = R;
         R = Tmp;
      }

      // instance
      int x, y;
      Swap<int>(x,y);

Works for Ada packages as well (similar to templates for C++ classes)

===================
Generic Contracts
===================

-------------
Definitions
-------------

* A formal generic parameter is a template
* Properties are either :dfn:`constraints` or :dfn:`capabilities`

    - Expressed from the **body** point of view
    - Constraints: e.g. unconstrained, :ada:`limited`
    - Capabilities: e.g. :ada:`tagged`, primitives

.. code:: Ada

   generic
      type Pv is private;           -- allocation, copy, assignment, "="
      with procedure Sort (T : Pv); -- primitive of Pv
      type Unc (<>) is private;     -- allocation require a value
      type Lim is limited private;  -- no copy or comparison
      type Disc is (<>);            -- 'First, ordering
   package Generic_Pkg is [...]

* Actual parameter **may** require constraints, and **must** provide capabilities

.. code:: Ada

   package Pkg is new Generic_Pkg (
      Pv => Integer, -- has capabilities of private
      Sort => Sort -- procedure Sort (T : Integer)
      Unc => String,  -- uses "unconstrained" constraint
      Lim => Float,   -- does not use "limited" constraint
      Disc => Boolean, -- has capability of discrete
  );

=====================
Generic Formal Data
=====================

--------------------------------------------
Generic Constants and Variables Parameters
--------------------------------------------

.. container:: columns

 .. container:: column

    * Variables can be specified on the generic contract
    * The mode specifies the way the variable can be used:

       - :ada:`in` |rightarrow| read only
       - :ada:`in out` |rightarrow| read write

    * Generic variables can be defined after generic types

 .. container:: column

    .. code:: Ada

       generic
          type T is private;
          X1 : Integer;  -- constant
          X2 : in out T; -- variable
       procedure P;

       V : Float;

       procedure P_I is new P
          (T  => Float,
           X1 => 42,
           X2 => V);

-------------------------------
Generic Subprogram Parameters
-------------------------------

* Subprograms can be defined in the generic contract
* Must be introduced by :ada:`with` to differ from the generic unit

   .. code:: Ada

      generic
         with procedure Callback;
      procedure P;
      procedure P is
      begin
         Callback;
      end P;
      procedure Something;
      procedure P_I is new P (Something);
