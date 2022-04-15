********************
Project Properties
********************

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

==============
Introduction
==============

------------------------
Specifying Directories
------------------------

+ Any number of Source Directories

  + Source Directories contain source files
  + If not specified, defaults to directory containing project file
  + Possible to create a project with no Source Directory

    + Not the same as not specifying the Source Directory!

+ One Object Directory

  + Contains object files and other tool-generated files
  + If not specified, defaults to directory containing project file

+ One Executables Directory

  + Contains executable(s)
  + If not specified, defaults to same location as Object Directory

+ *Tip: use forward slashes rather than backslashes for the most portability*

   * Backslash will only work on Windows
   * Forward slash will work on all supported systems (including Windows)

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

=============
Directories
=============

--------------------
Source Directories
--------------------

+ One or more in any project file
+ Default is same directory as project file
+ Can specify additional / other directories

   :ada:`for Source_Dirs use ("src/mains", "src/drivers", "foo");`

+ Can specify an entire tree of directories

     :ada:`for Source_Dirs use ("src/**");`

   + :filename:`src` directory and every subdirectory underneath

--------------
Source Files
--------------

+ Must be at least one **immediate** source file

  + In any source directory of project file
  + Unless explicitly specified none present

     :ada:`for Source_Files use ();`

+ Can specify source files by name

  :ada:`for Source_Files use ("pack1.ads","pack2.adb");`

+ Can specify an external file containing source names

  :ada:`for Source_List_File use "source_list.txt";`

------------------
Object Directory
------------------

+ Specifies location for compiler-generated files

  + Such as :filename:`.ali` files and object files
  + For the project's immediate sources

     .. code:: Ada

        project Release is
           for Object_Dir use "release";
           ...
        end Release;

+ Only one per project

  + When extending a parent project

    + Child's object directory contains output for source not already compiled in parent

----------------------
Executable Directory
----------------------

+ Specifies the location for executable image

   .. code:: Ada

      project Release is
         for Exec _Dir use "executables";
         ...
      end Release;

+ Default is same directory as object files
+ Only one per project

====================
Project Properties
====================

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

.. include:: labs/020_project_properties.lab.rst
