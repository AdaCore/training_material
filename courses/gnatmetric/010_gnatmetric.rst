************
GNATmetric
************

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
Overview of gnatmetric
------------------------
+ GNAT software metrics generation tool
+ Configurable metrics selection
+ Configurable scope of analysis

  + All source files in the project
  + One source file in the project
  + A specific entity in a source file, e.g. a subprogram

    + When applicable

+ :toolname:`GNAT Studio` provides a GUI interface

  + Selecting the metrics to compute
  + Selecting the scope of analysis
  + Displaying the results

-----------------------
Invoking the Analyses
-----------------------
From the :menu:`Analyze` menu

.. image:: ../../images/gnatmetric/menu_cascade.jpg

---------------------
Analysis Dialog Box
---------------------

.. image:: ../../images/gnatmetric/execute_dialog.jpg

For this module, we will select *All line metrics*, *All complexity metrics*, *All syntax element metrics*, and *All coupling metrics*

=======================
Exploring the Results
=======================

-----------------
Project Results
-----------------

Select project name (or GPR file) to get results across project

.. image:: ../../images/gnatmetric/project_results.jpg

----------------------
Package Spec Results
----------------------

Select specification file to get results for that particular file

.. image:: ../../images/gnatmetric/spec_results.jpg

----------------------
Package Body Results
----------------------

Select body file to get results for that particular file

.. image:: ../../images/gnatmetric/body_results.jpg

------------------------
Line Metrics Explained
------------------------

**Average Lines In Body**
   Average number of code lines in subprogram bodies, task bodies, entry bodies and package body executable code

**All Lines**
   Total number of lines in file(s)

**Blank Lines**
   Total number of blank in file(s)

**Code Lines**
   Total lines of code in file(s)

**Comment Lines**
   Total lines of comments in file(s)

**Comment Percentage**
   Comment lines divided by total of code lines and comment lines

**End-Of-Line Comments**
   Count of code lines that also contain comments

----------------------------------
Syntax Element Metrics Explained
----------------------------------

**All Declarations**
 Total number of objects declared

**All Statements**
 Total number of statements in file(s)

**All Subprogram Bodies**
 Total number of subprograms in file(s)

**All Type Definitions**
 Total number of types in file(s)

**Logical SLOC**
 Total of declarations plus statements

**Public Subprograms**
 Count of subprograms declared in visible part of package

**Public Types**
 Count of types (not subtypes) declared in the visible part of a package plus in the visible part of a generic nested package

**Maximal Construct Nesting**
 Maximal nesting level of composite syntactic constructs

**Maximum Unit Nesting**
 Maximal static nesting level of inner program units

------------------------------
Complexity Metrics Explained
------------------------------

**Average Complexity**
    Total Cyclomatic Complexity divided by total number of subprograms

**Cyclomatic Complexity**
    McCabe cyclomatic complexity (number of independent paths in the control flow graph)

**Essential Complexity**
    McCabe essential complexity (cyclomatic complexity after removing blocks with single entry/exit points)

**Expression Complexity**
    Complexity introduced by short-circuit control forms only

**Maximum Loop Nesting**
    Maximum depth of nested loops

**Statement Complexity**
    Complexity introduced by control statements only, without taking into account short-circuit forms 

---------------------------------
Understanding McCabe Complexity
---------------------------------

http://www.mccabe.com/pdf/mccabe-nist235r.pdf

+ Given a control flow graph of a program

  + E - number of edges
  + N - number of nodes
  + P - number of connected components (exit nodes)

+ The complexity is computed by:

  + E - N + 2 * P

+ Aimed a measuring the complexity of execution paths
+ Needs to be adapted for each language

----------------
McCabe Example
----------------

.. columns::

  .. column::

   .. code:: Ada

      if A then
         Put_Line ("A");
      else
         Put_Line ("!A");
      end if;

      if B or else C then
         Put_Line ("BC");
      end if;

  .. column::

     .. image:: ../../images/gnatmetric/cyclomatic_complexity_edges_and_nodes.jpg

     9 edges - 7 nodes + 2 * 1 exit = complexity 4

------------------------------
Coupling Metrics Explained
------------------------------

+ Measures dependencies between given entity and other entities in the program

  + High coupling may signal potential issues with maintainability

+ Metrics computed:

  **Object-oriented coupling**
     Classes in traditional object-oriented sense

  **Unit coupling**
     All units making up a program

  **Control coupling**
     Dependencies between unit and other units that contain subprograms

------------------------------
Coupling Metrics
------------------------------

+ Uses Ada's approach to definition of *class*

  + Tagged types declared within packages
  + Interface types declared within packages

+ Two kinds of coupling computed

  **Fan-out coupling**
    Number of classes given class

  **Fan-in coupling**
    Number of classes that depend on given class

+ Package bodies and specs for *classes* are both considered when computing dependencies

==============
Command Line
==============

-------------------------
Command Line Invocation
-------------------------

:command:`gnatmetric [options] {filename}`

* In general, same process as :toolname:`GNAT Studio`

  * Without the fancy graphical output

* Easier access to output file control


-----------------------------
Useful Command Line Options
-----------------------------

.. container:: latex_environment scriptsize

 .. list-table::

  * - :command:`--help`

    - Display usage and exit

  * - :command:`-Pproject`

    - Use project file project. Only one such switch can be used

  * - :command:`-U main`

    - Process the closure of units rooted at unit main

  * - :command:`--contract-all`

    - All contract metrics

  * - :command:`--complexity-all`

    - All complexity metrics

  * - :command:`--lines-all`

    - All line metrics

  * - :command:`--syntax-all`

    - All syntax element metrics

  * - :command:`--coupling-all`

    - All coupling metrics

  * - :command:`--output-dir=dirname`

    - Put files with detailed metrics into 'dirname'

  * - :command:`--generate-xml-output`

    - Generate XML output

  * - :command:`--generate-xml-schema`

    - Generate XML output and corresponding schema file

  * - :command:`--no-text-output`

    - Do not generate output in text form

=====
Lab
=====

.. include:: labs/010_gnatmetric.lab.rst

=========
Summary
=========

-----------------
Closing Remarks
-----------------

+ See the GNAT User's Guide for the meaning of all the switches
+ For :toolname:`GNAT Studio`, switches specified via the "Metrics" package in the project's GNAT project file
+ Note requirements on input source files

  + They must all be available

    + Including all those mentioned in context clauses, transitively

  + They must be able to be compiled
