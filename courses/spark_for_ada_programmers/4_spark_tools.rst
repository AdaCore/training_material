*************
SPARK Tools
*************

..
    Coding language

.. role:: ada(code)
    :language: Ada

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
Identifying SPARK Code
------------------------

* Pragma or aspect :code:`SPARK_Mode` identifies SPARK code

* As a pragma in the global/local configuration pragmas file

* As a configuration pragma at the start of a unit

  - Note: it comes before :code:`with` clauses

  .. code:: ada

     pragma SPARK_Mode (On); -- On is the default

     with Lib; use Lib;

     package Pack is ...

* As an aspect on the unit declaration

  .. code:: ada

     package Pack
       with SPARK_Mode
     is ...

* Both unit spec and unit body need a pragma/aspect

----------------------
Main Tools for SPARK
----------------------

* GNAT development tools: SPARK is a subset of Ada

  - Compiler also checks SPARK-specific legality rules

* SPARK analysis tools

  - Flow analysis and proof

  - File dependencies are different from the compiler

    + Due to generation of data dependencies

    + Analysis of unit depends on bodies of :code:`with`'ed units

    + ...unless all data dependencies are specified

  - Behavior similar to builder like GPRbuild

    + Units can be analyzed in parallel on multicore machines

    + Minimal rework if code and dependencies did not change

* IDEs for Ada/SPARK development

========================
GNAT Development Tools
========================

----------------------
Compiling SPARK Code
----------------------

* GNAT compiler for Ada/SPARK

  - Checks conformance of source with Ada and SPARK legality rules

  - Compiles source into executable

* Native and cross compilers

* Any runtime library: full, embedded, light

---------------------------------
Enabling Assertions at Run-Time
---------------------------------

* Assertions can be enabled globally with switch :command:`-gnata`

* Assertions can be enabled/disabled locally with pragma
  :code:`Assertion_Policy`

  For example to enable preconditions and disable postconditions:

  .. code:: ada

     pragma Assertion_Policy (Pre => Check, Post => Ignore);

* Pragma can also be used in global/local configuration pragmas file

* Failing assertion raises exception :code:`Assertion_Failure`

----------------------
Debugging SPARK Code
----------------------

* GDB debugger for Ada/SPARK

  - Code should be compiled with :command:`-g -O0`

* Assertions can be debugged too!

  - Code should be compiled with :command:`-gnata`

======================
SPARK Analysis Tools
======================

---------------------------------------------
:toolname:`GNATprove` - A Command Line Tool
---------------------------------------------

* Invocation syntax: :command:`gnatprove -P prj-file [switches]`

* If project file not given, like :toolname:`GPRbuild`:

  - Takes the project file in the current directory if present
  - Otherwise generates a basic project file

* See all switches with: :command:`gnatprove --help`

  - Basic switches such as number of processors to use

    + Analysis modes with :command:`--mode=`
    + Reporting mode with :command:`--report=`
    + Warnings mode with :command:`--warnings=`
    + Proof level with :command:`--level=0/1/2/3/4`

  - Advanced switches for fine-grain control

    + Prover selection with :command:`--prover=`
    + Prover control with :command:`--timeout= --steps= --memlimit=`

--------------------------------------------
:toolname:`GNATprove` - Project File Usage
--------------------------------------------

* Tool package :code:`Prove` corresponds to :toolname:`GNATprove`

  - Use attribute :code:`Proof_Switches` to apply tool-defined switches

    - For all files with value :code:`"Ada"`
    - For specific file with its name

.. code:: Ada

   project Proj is
     package Prove is
       for Proof_Switches ("Ada") use ("--level=2");
       for Proof_Switches ("file.adb") use ("--level=3");
     end Prove;
   end Proj;

  - Use attribute :code:`Proof_Dir` to specify directory for session files

----------------------------------------------
Setting the Default :code:`SPARK_Mode` Value
----------------------------------------------

* Set :code:`SPARK_Mode` in a global/local configuration pragmas file
  :filename:`config.adc`

  .. code:: Ada

     pragma SPARK_Mode (On);

* Set the :code:`Global_Configuration_Pragmas` attribute in the project file

  .. code:: Ada

     project Proj is
        package Builder is
           for Global_Configuration_Pragmas use "config.adc";
        end Builder;
     end Proj;

----------------------------------------
Adapting the Project File for Analysis
----------------------------------------

* If needed, define a project variable to control sources, compilation
  switches, etc.

  .. code:: Ada

     type Modes is ("Compile", "Analyze");
     Mode : Modes := External ("MODE", "Compile");

     case Mode is
        when "Compile" =>
           for Source_Dirs use (...);
        when "Analyze" =>
           for Source_Dirs use ("dir1", "dir2");
           for Source_Files use ("file1.ads", "file2.ads");
     end case;

* Run :toolname:`GNATprove` with appropriate value of :code:`MODE` defined in
  the environment or on the command-line

  .. code:: Ada

     gnatprove -P my_project -XMODE=Analyze

------------------------------------
Structure of :toolname:`GNATprove`
------------------------------------

.. image:: spark_structure.png

.. container:: speakernote

   Image comes from Appendix of SPARK User's Guide on "SPARK Architecture,
   Quality Assurance and Maturity".

-------------------
Legality Checking
-------------------

* First step in analysis

* :toolname:`GNATprove` does only that with switch :command:`--mode=check_all`

* Error messages on violations

  - Need to fix to go beyond this step

  - Ex: :command:`<expr> cannot depend on variable input <var>` |rightarrow|
    declare a constant :code:`value` to get the value of :code:`var` and use
    :code:`value` inside :code:`expr`

  - Ex: :command:`uninitialized allocator without default initialization is not
    allowed` |rightarrow| use :code:`new T'(Value)` instead of :code:`new T`

  - Ex: :command:`<such-and-such> not allowed` |rightarrow| rewrite code
    without such-and-such construct

* Includes ownership checking, detailed in course on Pointer Programs

---------------
Flow Analysis
---------------

* :dfn:`Flow analysis` is a prerequisite to proof

* :toolname:`GNATprove` does only that with switch :command:`--mode=flow`

* Corresponds to :menu:`Examine` menus in IDEs

* Outputs messages:

  - Error messages need to be fixed

  - Check messages need to be reviewed, and either fixed or justified

  - Warnings can be inspected and silenced

-------
Proof
-------

* :dfn:`Proof` is the final step

* :toolname:`GNATprove` does it all with switch :command:`--mode=all` (the
  default)

* Corresponds to :menu:`Prove` menus in IDEs

* Outputs messages:

  - Check messages need to be reviewed, and either fixed or justified

  - Warnings can be inspected and silenced

----------------------------------------
:toolname:`GNATprove` Output for Users
----------------------------------------

.. image:: gnatprove-output-options.png

-------------------------------------------------
Analysis Summary File :filename:`gnatprove.out`
-------------------------------------------------

* Located in :filename:`gnatprove/` under project object dir
* An overview of results for all checks in project
* Especially useful when results must be documented
* Details in SPARK User's Guide

|

.. image:: gnatprove-output-file.jpeg
   :width: 60%

================
IDEs for SPARK
================

---------------------------------------
Three Available IDEs Supporting SPARK
---------------------------------------

* :toolname:`GNAT Studio`

  - The AdaCore flagship IDE
  - Best integration overall

    + Most interaction capabilities
    + Specialized display of rich messages
    + Display of traces and counterexamples

* GNATbench for Eclipse

   - If you are already using Eclipse

* Ada/SPARK extension for Visual Studio Code

   - If you are already using VS Code

---------------------------------------------
Basic :toolname:`GNAT Studio` Look and Feel
---------------------------------------------

.. image:: gnatstudio-look_and_feel.png

-----------------------------------------------
:toolname:`GNATprove` :menu:`SPARK` Main Menu
-----------------------------------------------

.. image:: spark_menu-explanations.png

------------------------------
Project Tree Contextual Menu
------------------------------

.. image:: spark_rightclick-source_tree.jpeg
   :width: 100%

-----------------------------
Source Code Contextual Menu
-----------------------------

.. image:: spark_rightclick-code.jpeg

.. container:: speakernote

   Prove Line - The current line under the cursor when the contextual menu was invoked.

----------------------------
"Basic" Proof Dialog Panel
----------------------------

.. image:: prove_dialog-basic.jpeg

-----------------------------------------------------
Example Analysis Results in :toolname:`GNAT Studio`
-----------------------------------------------------

.. image:: gnatprove-output-ide.jpeg

----------------------------------
Preference for Selecting Profile
----------------------------------

.. container:: columns

 .. container:: column

    * Controlled by SPARK preference "User profile"

       - Basic
       - Advanced

    * Allow more control and options

       - Prover timeout (seconds)
       - Prover steps (effort)
       - Etc.

 .. container:: column

    .. image:: gnatstudio-preferences-spark.jpeg

-------------------------------
"Advanced" Proof Dialog Panel
-------------------------------

.. image:: prove_dialog-advanced.jpeg

=====
Lab
=====

----------------
SPARK Tutorial
----------------

* Open the SPARK User's Guide

  - From your SPARK release (under menu :menu:`Help` |rightarrow| :menu:`SPARK`
    |rightarrow| :menu:`SPARK User's Guide` in :toolname:`GNAT Studio`)

  - Or online at :url:`https://www.adacore.com/documentation`

* Go to section 6 about the :menu:`SPARK Tutorial`

* Follow intructions to use the development and analysis tools

* Discuss these with the instructor

=========
Summary
=========

-------------
SPARK Tools
-------------

* Development tools for SPARK are those for Ada

* Analysis tools in :toolname:`GNATprove`

  - Flow analysis

  - Proof

* Project files supports both command-line and IDEs

  - Package :code:`Prove` specific to :toolname:`GNATprove`

  - Possibility to indicate that all code is in SPARK by default

* All integrated in multiple IDEs

  - But :toolname:`GNAT Studio` provides the best integration
