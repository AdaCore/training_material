***********************
Advanced Capabilities
***********************

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

--------------------------
Other Types of GPR Files
--------------------------

* Project files can also be used for

  * Building libraries
  * Building systems

* Project files can also have children

  * Similar to Ada packages

==================
Library Projects
==================

-----------
Libraries
-----------

+ Subsystems packaged in specific way
+ Represented by project files with specific attributes
+ Referenced by other project files, as usual

  + Contents become available automatically, etc.

+ Library Project

   .. code:: Ada

      library project Static_Lib is
         -- keyword "library" is optional
         ...
      end Static_Lib;

+ Standard Project referencing library

   .. code:: Ada

      with "static_lib.gpr";
      project Main is
         ...
      end Main;

---------------------------
Creating Library Projects
---------------------------

+ Several global attributes are involved/possible
+ Required attributes

   Library_Name
      Name of library

   Library_Dir
      Where library is installed

+ Important optional attributes

   Library_Kind
      *static*, *static-pic*, *dynamic*, *relocatable* (same as *dynamic*)

   Library_Interface
      Restrict interface to subset of units

   Library_Auto_Init
      Should autoinit at load (if supported)

   Library_Options
      Extra arguments to pass to linker

   Library_GCC
      Use custom linker

-------------------------
Supported Library Types
-------------------------

+ Static Libraries

  + Code statically linked into client applications
  + Becomes permanent part of client during build
  + Each client gets separate, independent copy

+ Dynamic Libraries

  + Code dynamically linked at run-time
  + Not permanent part of application
  + Code shared among all clients

+ Stand-Alone Libraries (SAL)

  + Minimize client recompilations when library internals change
  + Contain all necessary elaboration code for Ada units within
  + Can be static or shared

+ See the *GNAT Pro Users Guide* for details

--------------------------------
Static Library Project Example
--------------------------------

.. code:: Ada

   library project Name is
      for Source_Dirs use ("src1", "src2");
      for Library_Dir use "lib";
      for Library_Name use "name";
      for Library_Kind use "static";
   end Name;

+ Creates library :filename:`libname.a` on Windows

------------------------------------
Standalone Library Project Example
------------------------------------

.. code:: Ada

   library project Name is
      Version := "1";
      for Library_Interface use ("int1", "int1.child");
      for Library_Dir use "lib";
      for Library_Name use "name";
      for Library_Kind use "relocatable";
      for Library_Version use "libdummy.so." & Version;
   end Name;

+ Creates library :filename:`libname.so.1` with a symlink :filename:`libname.so` that points to it

====================
Aggregate Projects
====================

----------------------
Complex Applications
----------------------

+ Many applications have multiple exectuables and/or libraries

  + Shared source code
  + Multiple "top-level" project files

+ Assume project A :ada:`withs` project B and project C

  + Build of project A will only compile/link whatever is necessary for project A's executable(s)
  + Executables in project B and C will need to be generated separately
  + Running :command:`gprbuild` on all three projects causes redundant processing

    + Determination of files that need to be compiled
    + Libraries are always built when :command:`gprbuild` is called

--------------------
Aggregate Projects
--------------------

+ Represent multiple, related projects

  + Related especially by common source code

+ Allow managing options in a centralized way
+ Compilation optimized for sources common to multiple projects

  + Doesn't compile more than necessary

---------------------------
Aggregate Project Example
---------------------------

.. code:: Ada

   aggregate project Agg is
      -- Projects to be built
      for Project_Files use ("A.gpr", "B.gpr", "C.gpr"); 
      -- Directories to search for project files
      for Project_Path use ("../dir1", "../dir1/dir2"); 
      -- Scenario variable
      for external ("BUILD") use "PRODUCTION"; 

      -- Common build switches
      package Builder is 
         for Global_Compilation_Switches ("Ada")
               use ("-O1", "-g");
      end Builder; 
   end Agg;

================
Child Projects
================

-------------------
Grouping Projects
-------------------

+ Sometimes we want to emphasize project relationships

  + Similar to parent/child relationship in Ada packages

+ Child project

  + Declare child of project same as in Ada: :ada:`project Parent.Child ...`
  + **No inheritance assumed** (unlike Ada)
  + Behavior of child follows normal project definition rules

----------------
Child Projects
----------------

* Original project

  .. code:: Ada

    -- math_proj.gpr
    project Math_Proj is
       ...
    end Math_Proj;

* Child *depends* on parent

  .. code:: Ada

     with "math_proj.gpr";
     project Math_Proj.Tests is
        ...
     end Math_Proj.Tests;

* Child *extends* parent

  .. code:: Ada

     project Math_Proj.High_Performance extends "math_proj.gpr" is
        ...
     end Math_Proj.High_Performance;

* Illegal project

  .. code:: Ada

     project Math_Proj.Test is
        ...
     end Math_Proj.Test;
