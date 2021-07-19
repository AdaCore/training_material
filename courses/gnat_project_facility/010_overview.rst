
**********
Overview 
**********

==============
Introduction
==============

----------------------------------
Origins and Purposes of Projects
----------------------------------

+ The need for flexibility

  + Managing huge applications is a difficult task; many AdaCore customers benefit from a tool to help them

+ The GNAT compilation model

  + Compiler needs to know where to find the Ada files imported by the Ada unit which is being compiled

+ The IDEs

  + AdaCore IDEs need to know where to find the source files, and also the object files

+ The Tools (metrics, documentation generator, etc)

  + AdaCore Tools benefit from having knowledge of the application structure

---------------------------------
Subsystems of Subsystems of ...
---------------------------------

+ Projects support incremental, modular project definition

  + Projects can import other projects containing needed files
  + Child projects can extend parent projects, inheriting source files and optionally overriding any of them

+ Facilitates the structuring of large development efforts into hierarchical subsystems

  + With build decisions deferred to the subsystem level

--------------------
GNAT Project Files
--------------------

+ Text files with Ada-like syntax
+ Also known as ``GPR files`` due to file extension
+ Integrated into command-line tools

  + Specified via the :command:`-P project-file-name` switch

+ Integrated into the IDEs

  + A fundamental part
  + Automatically generated if desired

+ Should be under configuration management

-------------------------
Configurable Properties
-------------------------

+ Source directories and specific files' names
+ Output directory for object modules and .ali files
+ Target directory for executable programs
+ Switch settings for project-enabled tools
+ Source files for main subprogram(s) to be built
+ Source programming languages

  + Ada / C / C++ are preconfigured

+ Source file naming conventions
+ et cetera

--------------------------
The Minimal Project File
--------------------------

.. code:: Ada

   project My_Project is
   end My_Project;

-------------------------------
Specifying Main Subprogram(s)
-------------------------------

+ Optional, otherwise requires specification on command line to build
+ Can have more than one file named
+ A project-level setting

.. code:: Ada

   project Foo is
      for Main use ("bar.adb", "baz.adb");
   end Foo;

-----------------------------------
About Project Files and Makefiles
-----------------------------------

+ A Makefile performs actions (indirectly)
+ A project file describes a project
+ Command lines using project files fit naturally in the Makefile paradigm

:command:`gnatmake -P <project-file> ...`

-----------------------------
Source Directories For Labs
-----------------------------

* **sources**

   * gnat

   * gps

      * *custom*
      * *diners*
      * *common*
      * *my_proj*
      * *solutions*

   * projects

      * *solutions*
      * *common*
      * *amazing*
      * *library*
      * *filenames* (initially empty)


=====
Lab
=====

.. include:: labs/010_overview.lab.rst
