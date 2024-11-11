**********
Overview 
**********

.. PRELUDE:: BEGIN

.. PRELUDE:: ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. PRELUDE:: SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. PRELUDE:: REQUIRES

.. PRELUDE:: PROVIDES

.. PRELUDE:: END

==============
Introduction
==============

----------------------------------
Origins and Purposes of Projects
----------------------------------

+ Need for flexibility

  + Managing huge applications is a difficult task
  + Build tools are always useful

+ GNAT compilation model

  + Compiler needs to know where to find Ada files imported by Ada unit being compiled

+ IDEs

  + AdaCore IDEs need to know where to find source and object files

+ Tools (metrics, documentation generator, etc)

  + AdaCore Tools benefit from having knowledge of application structure

---------------------------------
Subsystems of Subsystems of ...
---------------------------------

+ Projects support incremental, modular project definition

  + Projects can import other projects containing needed files
  + Child projects can extend parent projects

    + Inheriting all attributes of parent
    + Can optionally override source files and other attributes

+ Allows structuring of large development efforts into hierarchical subsystems

  + Build decisions deferred to subsystem level

===============
Project Files
===============

--------------------
GNAT Project Files
--------------------

+ Text files with Ada-like syntax
+ Also known as :dfn:`GPR files` due to file extension
+ Integrated into command-line tools

  + Specified via the :command:`-P project-file-name` switch

+ Integrated into IDEs

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

+ Optional

  + If not specified in file, must be specified on command-line

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
+ Command lines using project files fit naturally in Makefile paradigm

:command:`gprbuild -P <project-file> ...`
