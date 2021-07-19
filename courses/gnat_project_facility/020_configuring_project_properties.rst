********************************
Configuring Project Properties
********************************

.. role:: ada(code)
   :language: ada

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

  + Like functions that map an input string to either a single string or a list of strings

     *for <name> (<string-index>) use <list-of_strings>;*

     :ada:`for Switches ("Ada") use ("-gnaty", "-gnatwa");`

-----------
Variables
-----------

+ **Typed** - a set of possible string values
+ **Untyped** - unspecified set of values

  + Strings and lists

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

  + Once at project level
  + Once within any package
  + Essentially read-only constants

    + Especially nice for external inputs

+ Untyped variables may be "declared" many times

  + No previous declaration required

--------------------------------
"Packages" Correspond to Tools
--------------------------------

+ :ada:`Builder`

  + :toolname:`gnatmake` or :toolname:`gprbuild`

+ :ada:`Compiler`

  + :toolname:`gcc`

+ :ada:`Linker`

  + :toolname:`gnatlink`

+ :ada:`Binder`

  + :toolname:`gnatbind`

+ Others...
+ Allowable names and content defined by vendor

  + Not by users

-----------------------
Setting Tool Switches
-----------------------

+ May be specified to apply by default

   .. code:: Ada

      package Compiler is
         for Default_Switches ("Ada") use ("-gnaty", "-v");
      end Compiler;


+ May be specified on a per-unit basis

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
