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

==================
Project Packages
==================

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

=======================
Naming Considerations
=======================

-----------
Rationale
-----------

+ Project files assume source files have GNAT naming conventions

  Specification
    ``<unitname>[-<childunit>].ads``

  Body
    ``<unitname>[-<childunit>].adb``

+ Sometimes you want different conventions

  + Third-party libraries
  + Legacy code used different compiler

    + Changing filenames would make tracking changes harder

----------------------------
Source File Naming Schemes
----------------------------

+ Allow arbitrary naming conventions

  + Other than GNAT default convention

+ May be applied to all source files in a project

  + Specified in a package named :ada:`Naming`

+ May be applied to specific files in a project

  + Individual attribute specifications

-------------------------------------
Foreign Default File Naming Example
-------------------------------------

* Sample source file names

  * Package spec for :ada:`Utilities` in :filename:`utilities.spec`
  * Package body for :ada:`Utilities` in :filename:`utilities.body`
  * Package spec for :ada:`Utilities.Child` in :filename:`utilities.child.spec`
  * Package body for :ada:`Utilities.Child` in :filename:`utilities.child.body`

.. code:: Ada

   project Legacy_Code is
      ...
      package Naming is
         for Casing use "lowercase";
         for Dot_Replacement use ".";
         for Spec_Suffix ("Ada") use ".spec";
         for Body_Suffix ("Ada") use ".body";
      end Naming;
      ...
   end Legacy_Code;

----------------------------------
GNAT Default File Naming Example
----------------------------------

* Sample source file names

  * Package spec for :ada:`Utilities` in :filename:`utilities.ads`
  * Package body for :ada:`Utilities` in :filename:`utilities.adb`
  * Package spec for :ada:`Utilities.Child` in :filename:`utilities-child.ads`
  * Package body for :ada:`Utilities.Child` in :filename:`utilities-child.adb`

.. code:: Ada

   project GNAT is
      ...
      package Naming is
         for Casing use "lowercase";
         for Dot_Replacement use "-";
         for Spec_Suffix ("Ada") use ".ads";
         for Body_Suffix ("Ada") use ".adb";
      end Naming;
      ...
   end GNAT;

------------------------------------
Individual (Arbitrary) File Naming
------------------------------------

+ Uses associative arrays to specify file names

  + Index is a string containing the unit name

    + Case insensitive

  + Value is a string containing the file name

    + Case sensitivity depends on host file system

+ Has distinct attributes for specs and bodies

   *for Spec ("<unit name>") use "<filename>";*

   :ada:`for Spec ("MyPack.MyChild") use "MMS1AF32.A";`

   :ada:`for Body ("MyPack.MyChild") use "MMS1AF32.B";`

======================================
Variables for Conditional Processing
======================================

---------------------------------------------------
Two Sample Projects for Different Switch Settings
---------------------------------------------------

.. container:: latex_environment scriptsize

 .. columns::

   .. column::

      .. code:: Ada

         project Debug is 
           for Object_Dir use "debug"; 
           package Builder is
             for Default_Switches ("Ada")
                use ("-g"); 
           end Builder; 
           package Compiler is
             for Default_Switches ("Ada") 
                use ("-fstack-check",
                     "-gnata",
                     "-gnato"); 
           end Compiler;
         end Debug; 

   .. column::

      .. code:: Ada

         project Release is
           for Object_Dir use "release";
           package Compiler is 
             for Default_Switches ("Ada")
                use ("-O2"); 
           end Compiler;
         end Release; 

-------------------------------------
External and Conditional References
-------------------------------------

+ Allow project file content to depend on value of environment variables and command-line arguments
+ Reference to external values is by function

  ``external (<name> [, default])``

  + Returns value of **name** as supplied via

    * Command line
    * Environment variable
    * If not specified, uses **default** or else ""

+ Command line switch

  *gprbuild -P... -Xname=value ...*

  .. container:: latex_environment footnotesize

     :command:`gprbuild -P common/build.gpr -Xtarget=test  common/main.adb`

+ **Note:** Command line values override environment variables

----------------------------------------
External/Conditional Reference Example
----------------------------------------

.. code:: Ada

   project Build is
      type Targets is ("release", "test");
      Target : Targets := external("target", "test");
      case Target is -- project attributes
         when "release" =>
            for Object_Dir use "release";
            for Exec_Dir use ".";
         when "test" =>
            for Object_Dir use "debug";
      end case;
      package Compiler is
         case Target is
            when "release" =>
               for Default_Switches ("Ada") use ("-O2");
            when "test" =>
               for Default_Switches ("Ada") use
                     ("-g", "-fstack-check", "-gnata", "-gnato");
         end case;
      end Compiler;
      ...
   end Build;

--------------------------------------------
Scenario Controlling Source File Selection
--------------------------------------------

.. code:: Ada

   project Demo is
      ...
      type Displays is ("Win32", "ANSI");
      Output : Displays := external ("OUTPUT", "Win32");
      ...
      package Naming is
         case Output is
            when "Win32" =>
               for Body ("Console") use "console_win32.adb";
            when "ANSI" =>
               for Body ("Console") use "console_ansi.adb";
           end case;
      end Naming;
   end Demo;

* Source Files

 .. list-table::
   :header-rows: 1
    
   * - :filename:`console.ads`

     - 
     - :filename:`console_win32.adb`
     - 
     - :filename:`console_ansi.adb`

   * - :ada:`package Console is`

     - 
     - :ada:`package body Console is`
     - 
     - :ada:`package body Console is`

   * - :ada:`...`

     - 
     - :ada:`...`
     - 
     - :ada:`...`

   * - :ada:`end Console;`

     - 
     - :ada:`end Console;`
     - 
     - :ada:`end Console;`

=====
Lab
=====

.. include:: labs/030_project_properties.lab.rst
