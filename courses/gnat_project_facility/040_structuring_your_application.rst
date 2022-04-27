******************************
Structuring Your Application
******************************

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

--------------
Introduction
--------------

+ Most applications can be broken into pieces

  + Modules, components, etc - whatever you want to call them

+ Helpful to have a project file for each component

  + Or even multiple project files for better organization

------------
Dependency
------------

+ Units of one component typically depend units in other components

  + Types packages, utilities, external interfaces, etc

+ A project can :ada:`with` another project to allow visibility

  + Ambiguity issues can occur if the same unit appears in multiple projects

-----------
Extension
-----------

+ Sometimes we want to replace units for certain builds

  + Testing might require different package bodies
  + Different targets might require different values for constants

+ A project can *extend* another project

  + Project inherits properties and units from its parent
  + Project can create new properties and units to override parent

=========================
Building an Application
=========================

--------------------
Importing Projects
--------------------

+ Source files of one project may depend on source files of other projects

  + *Depend* in Ada sense (contains :ada:`with` clauses)

+ Want to localize properties of other projects

  + Switches etc.
  + Defined in one place and not repeated elsewhere

+ Thus dependent projects *import* other projects to add source files to search path

-------------------------
Project Import Notation
-------------------------

+ Similar to Ada's :ada:`with` clauses

  + But uses strings

  *with <literal string> {, <literal  string>};*

+ String literals are path names of project files

  + Relative
  + Absolute

  .. code:: Ada

     with "/gui/gui.gpr", "../math.gpr";
     project MyApp is
       ...
     end MyApp;

----------------------------
Importing Projects Example
----------------------------

.. code:: Ada

   with GUI, Math;
   package body Pack is 
      ...

* Source Architecture

 .. list-table::
   :header-rows: 1
    
   * - :filename:`/gui`

     - 
     - :filename:`/myapp`
     - 
     - :filename:`/math`

   * - :filename:`gui.gpr`

     - :math:`\rightarrow`
     - :filename:`myapp.gpr`
     - :math:`\leftarrow`
     - :filename:`math.gpr`

   * - :filename:`gui.ads`

     - 
     - :filename:`pack.ads`
     - 
     - :filename:`math.ads`

   * - :filename:`gui.adb`

     - 
     - :filename:`pack.adb`
     - 
     - :filename:`math.adb`

   * -

     - 
     - :filename:`main.adb`
     - 
     -

* Project File

   .. code:: Ada

      with "/gui/gui.gpr", "/math/math.gpr";
      project MyApp is
        ...
      end MyApp;

------------------------------
Referencing Imported Content
------------------------------

+ Use the Ada *dot notation* concept for declarations

  + Start with the project name

+ Use the tick (') for attributes

   .. code:: Ada

      with "foo.gpr";
      project P is
         package Compiler is
            for Default_Switches ("Ada") use 
               Foo.Compiler'Default_Switches & "-gnatwa";
         end Compiler;
      end P;

   + Project P uses all the compiler switches in project Foo and adds ``-gnatwa``

----------
Renaming
----------

+ Packages can rename (imported) packages
+ Effect is as if the package is declared locally

  + Much like the Ada language

.. code:: Ada

   with "../naming_schemes/rational.gpr";
   project Clients is
      package Naming renames Rational.Naming;
      for Languages use ("Ada");
      for Object_Dir use ".";
      ...
   end Clients;

----------------------
Project Dependencies
----------------------

+ Not unusual for projects to be interdependent

  + Source in project A refers to source in project B, and source in project B refers to source in project A

+ Project files cannot create a cycle using :ada:`with`

    + Neither direct (A |rightarrow| B |rightarrow| A)
    + Nor indirect (A |rightarrow| B |rightarrow| C |rightarrow| A)

+ So how do we allow the sources in each project to interact?

    + :ada:`limited with`
    + Allows sources to be interdependent, but not the projects

   .. code:: Ada

      limited with "A.gpr";
      project B is
         package Compiler is
            for Default_Switches ("Ada") use 
               A.Compiler'Default_Switches & "-gnatwa"; -- illegal
         end Compiler;
      end B;

------------
Subsystems
------------

+ Sets of sources and folders managed together
+ Represented by project files

  + Connected by project *with clauses* or project extensions
  + Generally one **primary** project file
  + Potentially many project files, assuming subsystems composed of other subsystems

+ Have at most one *objects* folder per subsystem

  + A defining characteristic
  + Typical, not required

--------------------
Subsystems Example
--------------------

.. code:: Ada

   with "gui.gpr";
   with "utilities.gpr";
   with "hardware.gpr";
   project Application is
      for Main use ("demo");
      for Object_Dir use ("objs");
      ...
   end Application;

   with "utilities.gpr";
   project Gui is
      for Object_Dir use ("objs");
      ...
   end Gui;

   with "utilities.gpr";
   project Hardware is
      for Object_Dir use ("objs");
      ...
   end Hardware;

   project Utilities is
      for Object_Dir use ("objs");
      ...
   end Utilities;

---------------------
Building Subsystems
---------------------

+ One project file given to the builder
+ Everything necessary will be built, transitively

  + Build :ada:`Utilities`

    + Only source specified in :filename:`utilities.gpr` will be built

  + Build :ada:`Hardware` (or :ada:`Gui`)

    + Source specified in :filename:`hardware.gpr` (or :filename:`gui.gpr`) will be built
    + Source specified in :filename:`utilities.gpr` will be built if needed

  + Build :ada:`Application`

    + Any source specified in any of the project files will be built as needed

====================
Extending Projects
====================

--------------------
Extending Projects
--------------------

+ Allows using modified versions of source files without changing the original sources
+ Based on *inheritance* of parent project's properties

  + Source files
  + Switch settings

+ Supports localized build decisions and properties

  + Inherited properties may be overridden with new versions

+ Hierarchies permitted

------------------------------
Limits on Extending Projects
------------------------------

+ A project that extends/modifies a project can also import other projects.
+ Can't import both a parent and a modified project.

  + If you import the extension, you get the parent

+ Can extend only one other project at a time.

------------------------------------------
Multiple Versions of Unit Bodies Example
------------------------------------------

+ Assume *Baseline* directory structure:

   + :filename:`baseline.gpr` contains

     + :filename:`filename.ads`
     + :filename:`filename.adb`
     + :filename:`application.adb`

+ For testing, you want to

   + Replace :filename:`filename.adb` with a dummy version
   + Use :filename:`test_driver.adb` as the main program

----------------------------------------
Multiple Versions of Unit Bodies Files
----------------------------------------

* *Baseline* GPR file might look like:

   .. code:: Ada

      project Baseline is
         for Source_Dirs use ("src");
         for Main use ("application");
      end Baseline;

* Test GPR file might look like:

   .. code:: Ada

      project Test_Baseline extends "Baseline" is
         for Source_Dirs use ( "test_code" );
         for Main use ( "test_driver" );
      end Test_Baseline;

=====
Lab
=====

.. include:: labs/040_structuring_your_application.lab.rst
