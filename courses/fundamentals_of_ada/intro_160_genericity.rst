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
        V : Integer;
      begin
         V := Left;
         Left := Right;
         Right := V;
      end Swap_Int;

      procedure Swap_Bool (Left, Right : in out Boolean) is
         V : Boolean;
      begin
         V := Left;
         Left := Right;
         Right := V;
      end Swap_Bool;

* It would be nice to extract these properties in some common pattern, and then just replace the parts that need to be replaced

   .. code:: Ada

      -- T := Integer | Boolean
      procedure Swap (Left, Right : in out T) is
        V : T;
      begin
         V := Left;
         Left := Right;
         Right := V;
      end Swap;

--------------------
Solution: Generics
--------------------

* A :dfn:`generic unit` is a code pattern which can be reused

   - Does not get compiled as-is

* The instantiation applies the pattern to certain parameters

   - Based on properties
   - Use a :dfn:`generic contract`
   - Parameters can be constant, variable, subprogram, type, package

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

------
Quiz
------

Which of the following statement is true?

A. :answer:`Generics allow for code reuse`
B. :answer:`Generics can take packages as parameters`
C. Genericity is specific to Ada
D. :answer:`Genericity is available in all versions of Ada and/or SPARK`

------
Quiz
------

Which one(s) of the following can be made generic?

.. code:: Ada

    generic
       type T is private;
    <code goes here>

A. :answermono:`package`
B. ``record``
D. :answermono:`function`
C. ``array``

.. container:: animate

   Only packages, functions, and procedures, can be made generic.

------
Quiz
------

Which of the following statement is true?

A. Generic instances must be nested inside a non-generic package
B. Generic instances must be instantiated at compile-time
C. :answer:`Generics instances can create new tagged types`
D. :answer:`Generics instances can create new tasks`

.. container:: animate

    Generic instances can be instantiated at any point, at a cost, and
    can do anything a package or subprogram can do, which make them
    versatile **but** potentially complex to use.

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
   type T2 is range <>; -- Integer
   type T3 is digits <>; -- float
   type T4 is private; -- indefinite
   type T5 (<>) is private; -- indefinite
   type T6 is tagged private;
   type T7 is array (Boolean) of Integer;
   type T8 is access Integer;
   type T9 is limited private;

* Not limited to those choices

.. code:: Ada

   type T is not null access all limited tagged private;

------
Quiz
------

Which of the following statement is true?

A. Generic contracts define new types
B. Generic contracts can express any type constraint
C. :answer:`Generic contracts can express inheritance constraint`
D. Generic contracts can require a type to be numeric (:ada:`Real` or :ada:`Integer`)

.. container:: animate

   A. No, the formal type and the actual type just have different views
   B. Counter-example: representation clauses

------
Quiz
------

.. include:: quiz/generic_subp_syntax/quiz.rst

------
Quiz
------

.. code:: Ada

   generic
      type T1 is (<>);
      type T2 (<>) is private;
   procedure G
     (A : T1;
      B : T2);

Which is **not** a legal instantiation?

   A. :answermono:`procedure A is new G (String, Character);`
   B. ``procedure B is new G (Character, Integer);``
   C. ``procedure C is new G (Integer, Boolean);``
   D. ``procedure D is new G (Boolean, String);``

.. container:: animate

   :ada:`T1` must be discrete - so an integer or an enumeration. :ada:`T2` can be any type

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
      procedure Something is null;
      procedure P_I is new P (Something);

------
Quiz
------

.. include:: quiz/genericity_type_and_variable/quiz.rst

------
Quiz
------

.. include:: quiz/genericity_limited_type/quiz.rst

=========
Summary
=========

---------
Summary
---------

* Generics are useful for **reusing code**

    - Sorting, containers, etc

* Generic contracts syntax is different from Ada declaration

    - But has some resemblance to it
    - e.g. discretes' :ada:`type Enum is (A, B, C)` vs generics' :ada:`type T is (<>)`

* Instantiation "generates" code

    - Costly
    - Beware of local generic instances!
