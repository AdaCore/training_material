**************
Ada.Numerics
**************

.. container:: PRELUDE BEGIN

.. container:: PRELUDE ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. role:: Rust(code)
    :language: Rust

.. container:: PRELUDE SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. container:: PRELUDE REQUIRES

.. container:: PRELUDE PROVIDES

.. container:: PRELUDE END

==============
Introduction
==============

-----------------------
Common Math Equations
-----------------------

* Package :ada:`Ada.Numerics` is the parent package for mathematical types and functionality

  * Standard mathematical functions (square root, cosine, etc)

    * :ada:`Ada.Numerics.Elementary_Functions`
    * :ada:`Ada.Numerics.Generic_Elementary_Functions`

  * Random number generation

    * :ada:`Ada.Numerics.Discrete_Random`
    * :ada:`Ada.Numerics.Float_Random`

  * Complex types (not discussed in this module)

    * :ada:`Ada.Numerics.Generic_Complex_Elementary_Functions`
    * :ada:`Ada.Numerics.Complex_Elementary_Functions` (instantiation of above using :ada:`Float`)
    * :ada:`Ada.Numerics.Generic_Complex_Types`
    * :ada:`Ada.Numerics.Complex_Types` (instantiation of above using :ada:`Float`)

--------------
Math Symbols
--------------

* :ada:`Ada.Numerics` also contains the following symbols as named numbers

  * *e* - also known as Euler's number
  * Pi - approximation to a large precision

    * :math:`\pi` can also be used for compilers that support extended character sets

=================================
Standard Mathematical Functions
=================================

-----------------------------------
Ada.Numerics.Elementary_Functions
-----------------------------------

* Contains standard math and trigonometric functions

* All floating point parameters use standard floating point type :ada:`Float`

* Exceptions raised

  * When an invalid value is used (e.g. :ada:`Sqrt (-1)`), the exception :ada:`Argument_Error` is raised
  * When a value exceeds it's constraints, the normal :ada:`Constraint_Error` is raised

--------------------------------------------
Ada.Numerics.Elementary_Functions Contents
--------------------------------------------

* Standard math functions

  .. list-table::

    * - :ada:`"**"`

      - Allows exponents to be floating point

    * - :ada:`Sqrt`

      - Square root

    * - :ada:`Log`

      - Logarithm (one form for base *e* and one for user-specified base)

    * - :ada:`Exp`

      - *e* to the specified power

* Trigonometric functions

  .. list-table::

    * - :ada:`Arccos`

      - :ada:`Arcsin`

      - :ada:`Cos`

      - :ada:`Sin`

    * - :ada:`Arccosh`

      - :ada:`Arcsinh`

      - :ada:`Cosh`

      - :ada:`Sinh`

    * - :ada:`Arccot`

      - :ada:`Arctan`

      - :ada:`Cot`

      - :ada:`Tan`

    * - :ada:`Arccoth`

      - :ada:`Arctanh`

      - :ada:`Coth`

      - :ada:`Tanh`

-------------------------------------------
Ada.Numerics.Generic_Elementary_Functions
-------------------------------------------

* :ada:`Ada.Numerics.Generic_Elementary_Functions` is a generic package of mathematical functions

  * Can be instantiated for any floating point type

* :ada:`Ada.Numerics.Elementary_Functions` is actually just an instantiation of :ada:`Ada.Numerics.Generic_Elementary_Functions` with :ada:`Standard.Float`

================
Random Numbers
================

-------------------------------------------------
Differences Between Discrete and Floating Point
-------------------------------------------------

* Two packages for random number generation

  * :ada:`Ada.Numerics.Discrete_Random`

    * Generic package that needs to be instantiated with a discrete type (typically integer-based, but could be an enumeral)
    * Function :ada:`Random` returns a value within the discrete type

  * :ada:`Ada.Numerics.Float_Random`

    * Not generic
    * Function :ada:`Random` returns a value between 0.0 and 1.0
    * Treat return value as a fraction of the range

---------------------
Randon Number Usage
---------------------

.. code:: Ada

  with Ada.Numerics.Discrete_Random;
  with Ada.Numerics.Float_Random;
  with Ada.Text_IO;
  use Ada.Text_IO;
  procedure Main is
    type Count_T is range 1 .. 10;
    type Value_T is digits 6 range 0.0 .. 1_000.0;
    -- Create instance for an integer-based random number
    package I_Random is new Ada.Numerics.Discrete_Random (Count_T);
    -- Use a rename to simplify floating point random number
    package F_Random renames Ada.Numerics.Float_Random;
    -- Generators keep track for pseudo-random algorithm
    I_Generator : I_Random.Generator;
    F_Generator : F_Random.Generator;
    V           : Value_T;
  begin
    -- Intialize generators
    I_Random.Reset (I_Generator);
    F_Random.Reset (F_Generator);
    -- Loop a random number of times
    for I in 1 .. I_Random.Random (I_Generator) loop
      -- Print a random floating point number
      V := Value_T (F_Random.Random (F_Generator)) * Value_T'Last;
      Ada.Text_IO.Put_Line (Count_T'Image (I) & " => " & Value_T'Image (V));
    end loop;
  end Main;

========
Lab
========

.. include:: labs/893_ada_numerics.lab.rst

=========
Summary
=========

---------
Summary
---------

* :ada:`Ada.Numerics` contains higher-level math functions

  * Low-level functions are built into the language!

* :ada:`Ada.Numerics.Generic_Elementary_Functions`

   * Create instances for each floating point type as needed

* :ada:`Ada.Numerics.*_Random`

   * Random floating point numbers are generated as a fraction
   * Random integer numbers are created over the whole range based on the instantiation
