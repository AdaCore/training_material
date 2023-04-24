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
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

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

-----------------------
GPRBuild search paths
-----------------------

GPR with **relative** paths are searched

- From the current project directory
- From the environment variables

    * Path to a file listing directory paths

        + ``GPR_PROJECT_PATH_FILE``

    * List of directories, separated by PATH-like (``:``, ``;``) separator

        + ``GPR_PROJECT_PATH``
        + ``ADA_PROJECT_PATH`` (deprecated)

- From the current toolchain's install dir

    * Can be target-specific
    * Can be runtime-specific
    * See GPR Tool's User Guide

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

+ When referencing imported projects, use the Ada *dot notation* concept for declarations

  + Start with the project name
  + Use the tick (') for attributes

    .. code:: Ada

      with "foo.gpr";
      project P is
         package Compiler is
            for Default_Switches ("Ada") use 
               Foo.Compiler'Default_Switches("Ada") & "-gnatwa";
         end Compiler;
      end P;

   + Project P uses all the compiler switches in project Foo and adds ``-gnatwa``

   + *Note: in GPR files, "&" can be used to concatenate string lists and string*

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

----------------------------------
Project Source Code Dependencies
----------------------------------

+ Not unusual for projects to be interdependent

  + In the :ada:`Nav` project

    .. code:: Ada

      with Hmi.Controls;
      package body Nav.Engine is
         Global_Speed : Speed_T := 0.0;
         procedure Increase_Speed (Change : Speed_Delta_T) is
            Max_Change : Speed_T := Global_Speed * 0.10;
         begin
            Global_Speed :=
              Global_Speed + Speed_T'max (Speed_T (Change),
                                          Max_Change);
            Hmi.Controls.Display_Speed (Global_Speed);
         end Increase_Speed;
      end Nav.Engine;

  + In the :ada:`HMI` project

    .. code:: Ada

      package body Hmi.Controls is
         procedure Display_Speed (Speed : Nav.Engine.Speed_T) is
         begin
            Display_Speed_On_Console (Speed);
         end Display_Speed;
         procedure Change_Speed (Speed_Change : Nav.Engine.Speed_Delta_T) is
         begin
            Nav.Engine.Increase_Speed (Speed_Change);
         end Change_Speed;
      end Hmi.Controls;

----------------------
Project Dependencies
----------------------

+ Project files cannot create a cycle using :ada:`with`

    + Neither direct (:ada:`Hmi` |rightarrow| :ada:`Nav` |rightarrow| :ada:`Hmi`)
    + Nor indirect (:ada:`Hmi` |rightarrow| :ada:`Nav` |rightarrow| :ada:`Monitor` |rightarrow| :ada:`Hmi`)

+ So how do we allow the sources in each project to interact?

    + :ada:`limited with`
    + Allows sources to be interdependent, but not the projects

.. code:: Ada

   limited with "Hmi.gpr";
   project Nav is
     package Compiler is
       for Switches ("Ada") use 
           Hmi.Compiler'Switches & "-gnatwa"; -- illegal
     end Compiler;
   end Nav;

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
