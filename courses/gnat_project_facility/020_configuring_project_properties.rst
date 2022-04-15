********************************
Configuring Project Properties
********************************

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

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

====================
Project Properties
====================

-----------------
Property Values
-----------------

+ Strings
+ Lists of strings

    :ada:`("-v", "-gnatv")`

+ Associative arrays

  + Map input string to either single string or list of strings

     *for <name> (<string-index>) use <list-of_strings>;*

     :ada:`for Switches ("Ada") use ("-gnaty", "-gnatwa");`

-----------
Variables
-----------

**Typed**
  Set of possible string values

**Untyped**
  Unspecified set of values (strings and lists)

.. code:: Ada

   project Build is
      type Targets is ("release", "test");
      -- Typed variable
      Target : Targets := external("target", "test");
      -- Untyped string variable
      Var := "foo";
      -- Untyped string list variable
      Var2 := ("-gnato", "-gnata");
      ...
   end Build;

--------------------------------
Typed Versus Untyped Variables
--------------------------------

+ Typed variables have only listed values possible

  + Case sensitive, unlike Ada

+ Typed variables are declared once per scope

  + Once at project or package level
  + Essentially read-only constants

    + Useful for external inputs

+ Untyped variables may be "declared" many times

  + No previous declaration required

------------------------------
Packages Correspond to Tools
------------------------------

+ Packages within project file contain switches (generally) for specific tools

+ Allowable names and content defined by vendor

  + Not by users

.. columns::

  .. column::

    * Binder
    * Builder
    * Check
    * Clean
    * Compiler
    * Cross_Reference
    * Documentation
    * Eliminate
    * Finder
    * Gnatls

  .. column::

    * Gnatstub
    * IDE
    * Install
    * Linker
    * Metrics
    * Naming
    * Pretty_Printer
    * Remote
    * Stack
    * Synchronize

-----------------------
Setting Tool Switches
-----------------------

+ May be specified to apply by default

   .. code:: Ada

      package Compiler is
         for Default_Switches ("Ada") use ("-gnaty", "-v");
      end Compiler;


+ May be specified on per-unit basis

  + Associative array "Switches" indexed by unit name

   .. code:: Ada

      package Builder is
         for Switches ("main1.adb") use ("-O2");
         for Switches ("main2.adb") use ("-g");
      end Builder;

=====
Lab
=====

.. include:: labs/020_configuring_project_properties.lab.rst
