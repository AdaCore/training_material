**************
Declarations
**************

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

====================================
Identifiers, Comments, and Pragmas
====================================

-------------
Identifiers
-------------

* Syntax

   .. code::

      identifier ::= letter {[underline] letter_or_digit}

* Character set **Unicode** 4.0

   - 8, 16, 32 bit-wide characters

* Case **not significant**

   - `SpacePerson` |equivalent| `SPACEPERSON`
   - but **different** from `Space_Person`

* Reserved words are **forbidden**

----------
Comments
----------

* Terminate at end of line (i.e., no comment terminator sequence)

   .. code:: Ada

      -- This is a multi-
      -- line comment
      A : B; -- this is an end-of-line comment

---------
Pragmas
---------

* Compiler directives

   - Compiler action
   - Serve various roles
      - May suggest or modify compiler behavior
      - May restrict feature usage
      - May generate specfic code
   - Either standard or implementation-defined

* Unrecognized pragmas

   - **No effect**
   - Cause **warning** (standard mode)

* Malformed pragmas are **illegal**

.. code:: Ada

   pragma Page;
   pragma Optimize (Off);

==================
Numeric Literals
==================

--------------------------
Decimal Numeric Literals
--------------------------

* Syntax

   .. code::

      decimal_literal ::=
        numeral [.num] E [+numeral|-numeral]
      numeral ::= digit {[underline] digit}

* Underscore is not significant
* **E** (exponent) must always be integer
* Examples

   .. code:: Ada

      12      0       1E6         123_456
      12.0    0.0     3.14159_26  2.3E-4

------------------------
Based Numeric Literals
------------------------

.. code::

   based_literal ::= base # numeral [.numeral] # exponent
   numeral ::= base_digit { '_' base_digit }

* Base can be 2 .. 16
* Exponent is always a base 10 integer

   ::

      16#FFF#           => 4095
      2#1111_1111_1111# => 4095 -- With underline
      16#F.FF#E+2       => 4095.0
      8#10#E+3          => 4096 (8 * 8**3)

=====================
Object Declarations
=====================

--------------
Declarations
--------------

* Associate a :dfn:`name` to an :dfn:`entity`

    - Objects
    - Types
    - Subprograms
    - et cetera

* Declaration **must precede** use
* **Some** implicit declarations

    - **Standard** types and operations
    - **Implementation**-defined

---------------------
Object Declarations
---------------------

* Variables and constants
* Basic Syntax

   .. code:: Ada

      <name> : subtype_indication [:= <initial value>];

* Examples

   .. code:: Ada

      Z, Phase : Analog;
      Max : constant Integer := 200;
      -- variable with a constraint
      Count : Integer range 0 .. Max := 0;
      -- dynamic initial value via function call
      Root : Tree := F(X);
      -- Will call G(X) twice, once per variable
      A, B : Integer := G(X);
