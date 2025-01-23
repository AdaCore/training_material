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

