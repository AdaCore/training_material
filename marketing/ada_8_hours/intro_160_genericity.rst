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

========
Syntax
========

-------
Usage
-------

* Instantiated with the :ada:`new` keyword

.. code:: Ada

   --  Standard library
   function Convert is new Ada.Unchecked_Conversion
     (Integer, Array_Of_4_Bytes);
   --  Callbacks
   procedure Parse_Tree is new Tree_Parser
     (Visitor_Procedure);
   --  Containers, generic data-structures
   package Integer_Stack is new Stack (Integer);

* Advanced usages for testing, proof, meta-programming

-------------
Declaration
-------------

* Subprograms

   .. code:: Ada

      generic
         type T is private;
      procedure Swap (L, R : in out T);

* Packages

  .. code:: Ada

      generic
         type T is private;
      package Stack is
         procedure Push (Item : T);
      end Stack;

* Body is required

    - Will be specialized and compiled for **each instance**

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

------------------
Syntax (partial)
------------------

.. code:: Ada

   type T1 is (<>); -- discrete
   type T2 is range <>; -- integer
   type T3 is digits <>; -- float
   type T4 is private; -- indefinite
   type T5 (<>) is private; -- indefinite
   type T6 is tagged;
   type T7 is array (Boolean) of Integer;
   type T8 is access Integer;
   type T9 is limited private;

* Not limited to those choices

.. code:: Ada

   type T is not null access all limited tagged private;

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
