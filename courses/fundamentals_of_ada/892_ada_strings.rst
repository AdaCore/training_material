*************
Ada.Strings
*************

.. container:: PRELUDE BEGIN

.. container:: PRELUDE ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

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

-------------------------------
Predefined Type :ada:`String`
-------------------------------

* :ada:`String` type allows varying lengths, but :ada:`String` objects are fixed lengths

  * It's just an unconstrained array of characters

* Language does not have any built-in string manipulation subprograms

* What if we want to change the length of the object?

--------------------------
:ada:`Ada.Strings.Fixed`
--------------------------

* Based on fixed-length string

* Strings are unconstrained arrays, so objects cannot change length

* Operations that return string of unknown (or different) length can only be used for initialization

----------------------------
:ada:`Ada.Strings.Bounded`
----------------------------

* Contains generic package

  * Must create instance passing in maximum string length

* String length is maintained internally

  * Operations can modify objects in-place
  * Subject to limit of maximum length

* Contains query to get maximum length

  * Allows client to pre-determine if length will be exceeded

------------------------------
:ada:`Ada.Strings.Unbounded`
------------------------------

* Not a generic package

  * No maximum length (except runtime limits!)

* String length is maintained internally

  * Operations can modify objects in-place
  * Subject to limit of maximum length

* Requires dynamic memory allocation

=====================
String Operations
=====================

----------------------------
Primitive String Functions
----------------------------

* Operations like concatenation (:ada:`"&"`) and comparison (:ada:`">="`, etc)

  * Built in for **fixed-length** strings
  * Defined in appropriate package for **bounded** and **unbounded**

    * Require :ada:`use` or :ada:`use type` for simple visibility

--------------------
Common Subprograms
--------------------

.. container:: latex_environment scriptsize

  .. list-table::

    * - :ada:`"*"`

      - Return the character or string duplicated N times

    * - :ada:`Count`

      - Number of occurrences of specified string/character set

    * - :ada:`Delete`

      - Remove slice

    * - :ada:`Find_Token`

      - Location of token that matches/doesn't match character set

    * - :ada:`Head`

      - Front N characters (padded as necessary)

    * - :ada:`Index`

      - Index of character/string, given starting location/direction

    * - :ada:`Index_Non_Blank`

      - Index of first/last character/string, given starting location/direction

    * - :ada:`Insert`

      - Insert substring into source before the specified position

    * - :ada:`Overwrite`

      - Overwrite source with new substring starting at the specified position

    * - :ada:`Replace_Slice`

      - Replace specified slice with new string

    * - :ada:`Tail`

      - Last N characters (padded as necessary)

    * - :ada:`Translate`

      - Translate string using specified character mapping

    * - :ada:`Trim`

      - Remove leading/trailing characters from source

-------------------------------
Bounded/Unbounded Subprograms
-------------------------------

.. container:: latex_environment scriptsize

  .. list-table::

    * - :ada:`Append`

      - Concatenate bounded strings and/or standard strings

    * - 

      - to create an unbounded string

    * - :ada:`Element`

      - Character at specified position

    * - :ada:`Length`

      - Length of string

    * - :ada:`Replace_Element`

      - Put input character specified position

    * - :ada:`Slice`

      - Standard string slice from specified positions

    * - :ada:`To_String`

      - Convert unbounded string to standard string

-------------------------------
Unique Subprograms
-------------------------------

* :ada:`Ada.Strings.Fixed`

  .. container:: latex_environment scriptsize

    .. list-table::

      * - :ada:`Move`

        -  Copy source to target with truncation/padding

* :ada:`Ada.Strings.Bounded`

  .. container:: latex_environment scriptsize

    .. list-table::

      * - :ada:`Bounded_Slice`

        - Bounded string slice from specified positions

      * - :ada:`Replicate`

        - Return the bounded string duplicated N times

      * - :ada:`Set_Bounded_String`

        - Procedural copy standard string to bounded string

      * - :ada:`To_Bounded_String`

        - Copy standard string to bounded string

* :ada:`Ada.Strings.Unbounded`

  .. container:: latex_environment scriptsize

    .. list-table::

      * - :ada:`Set_Unbounded_String`

        - Procedural copy standard string to unbounded string

      * - :ada:`To_Unbounded_String`

        - Copy standard string to unbounded string

      * - :ada:`Unbounded_Slice`

        - Unbounded string slice from specified positions

========
Lab
========

.. include:: labs/892_ada_strings.lab.rst

=========
Summary
=========

---------
Summary
---------

* :ada:`Ada.Strings.Fixed`

  - String operations for :ads:`String`

* :ada:`Ada.Strings.Bounded`

  - Varying length string where the maximum length is constrained
  - Requires generic instantiation
  - Implementation may be handled without dynamic memory allocation

* :ada:`Ada.Strings.Unbounded`

  - Varying length string with no maximum length
  - Implementation typically requires dynamic memory allocation
