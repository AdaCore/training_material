****************
Ada.Characters
****************

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

------------------------
Character Capabilities
------------------------

* Package :ada:`Ada.Characters` is the parent package for identification and manipulation of characters

  * :ada:`Ada.Characters.Handling` - queries and conversion subprograms
  * :ada:`Ada.Characters.Latin_1` - constants for character values 0 .. 255

========================
Ada.Characters.Latin_1
========================

----------------------------
Package Contents (Partial)
----------------------------

.. code::

  package Ada.Characters.Latin_1 is
    NUL          : constant Character := Character'Val (0);
    SOH          : constant Character := Character'Val (1);
    STX          : constant Character := Character'Val (2);
    ETX          : constant Character := Character'Val (3);
    EOT          : constant Character := Character'Val (4);
    ENQ          : constant Character := Character'Val (5);
    -- ...
    Space        : constant Character := ' ';  -- Character'Val (32)
    Exclamation  : constant Character := '!';  -- Character'Val (33)
    Quotation    : constant Character := '"';  -- Character'Val (34)
    Number_Sign  : constant Character := '#';  -- Character'Val (35)
    Dollar_Sign  : constant Character := '$';  -- Character'Val (36)
    -- ...
    LC_A         : constant Character := 'a';  -- Character'Val (97)
    LC_B         : constant Character := 'b';  -- Character'Val (98)
    LC_C         : constant Character := 'c';  -- Character'Val (99)
    LC_D         : constant Character := 'd';  -- Character'Val (100)
    LC_E         : constant Character := 'e';  -- Character'Val (101)
    -- ...
  end Ada.Characters.Latin_1;

--------
Idioms
--------

* Obvious - giving names to unprintable characters

* Good coding practice to use names instead of literals

  * Easier searching for non-alphanumeric characters

* Some symbols have multiple names, such as:

  * :ada:`Minus_Sign` |rightarrow| :ada:`Hyphen`
  * :ada:`NBSP` |rightarrow| :ada:`No_Break_Space`
  * :ada:`Ring_Above` |rightarrow| :ada:`Degree_Sign`

=========================
Ada.Characters.Handling
=========================
    
-------------------
Character Queries
-------------------

* Boolean functions whose return is based on the *category* of the character, such as:

  .. container:: latex_environment footnotesize

    .. code:: Ada

     function Is_Control           (Item : Character) return Boolean;
     function Is_Graphic           (Item : Character) return Boolean;
     function Is_Letter            (Item : Character) return Boolean;
     function Is_Lower             (Item : Character) return Boolean;
     function Is_Upper             (Item : Character) return Boolean;
     function Is_Basic             (Item : Character) return Boolean;
     function Is_Digit             (Item : Character) return Boolean;
     function Is_Decimal_Digit     (Item : Character) return Boolean
       renames Is_Digit;
     function Is_Hexadecimal_Digit (Item : Character) return Boolean;
     function Is_Alphanumeric      (Item : Character) return Boolean;

--------------------------
Character Transformation
--------------------------

* Functions to force case 

  .. container:: latex_environment footnotesize

    .. code:: Ada

      function To_Lower (Item : in Character) return Character;
      function To_Upper (Item : in Character) return Character;

* Functions to force case (string version)

  .. container:: latex_environment footnotesize

    .. code:: Ada

      function To_Lower (Item : in String) return String;
      function To_Upper (Item : in String) return String;

* Functions to convert to/from :ada:`Wide_Character` and :ada:`Wide_String`

  .. container:: latex_environment footnotesize

    .. code:: Ada

       function To_Character (Item       : Wide_Character;
                              Substitute : Character := ' ')
                              return Character;
       function To_String (Item       : Wide_String;
                           Substitute : Character := ' ')
                           return String;
       function To_Wide_Character (Item : Character)
                                   return Wide_Character;
       function To_Wide_String (Item : String)
                                return Wide_String;

========
Lab
========

.. include:: labs/891_ada_characters.lab.rst

=========
Summary
=========

---------
Summary
---------

* :ada:`Ada.Characters` contains consistent mechanisms for

  * Referring to unprintable and special characters
  * Queries on the properties of characters

* Same capabilities for other character sets in :ada:`Ada.Wide_Characters` and :ada:`Ada.Wide_Wide_Characters`

