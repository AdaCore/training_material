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

:dfn:`Average Lines In Body`
   Average number of code lines in subprogram bodies, task bodies, entry bodies and package body executable code

:dfn:`All Lines`
   Total number of lines in file(s)

:dfn:`Blank Lines`
   Total number of blank in file(s)

:dfn:`Code Lines`
   Total lines of code in file(s)

:dfn:`Comment Lines`
   Total lines of comments in file(s)

:dfn:`Comment Percentage`
   Comment lines divided by total of code lines and comment lines

:dfn:`End-Of-Line Comments`
   Count of code lines that also contain comments

----------------------
Line Metrics Example
----------------------

.. columns::

 .. column::

  .. container:: latex_environment tiny

   .. code:: Ada
      :number-lines: 1

    with Ada.Text_IO; use Ada.Text_IO;
    package body Example is

       procedure Internal (C : Character) is
       begin
          Put (C);
       end Internal;

       -- Print the prompt
       procedure Example (S1, S2 : String_T) is
          S : constant String := To_String (S1 & S2);
       begin
          for C of S
          loop
             Internal (C);
          end loop;
          New_Line;
       end Example;

    end Example;

 .. column::

  .. container:: latex_environment tiny

   ::

     Metrics computed for src\example.adb
     containing package body Example
     === Code line metrics ===
       all lines           : 20
       code lines          : 16
       comment lines       : 1
       end-of-line comments: 0
       comment percentage  : 5.88
       blank lines         : 3
     Average lines in body: 6.50

     Example (package body - library item at lines  2: 20)
     === Code line metrics ===
        all lines           : 19
        code lines          : 15
        comment lines       : 1
        end-of-line comments: 0
        comment percentage  : 6.25
        blank lines         : 3

        Internal (procedure body at lines  4: 7)
        === Code line metrics ===
           all lines           : 4
           code lines          : 4
           comment lines       : 0
           end-of-line comments: 0
           comment percentage  : 0.00
           blank lines         : 0

        Example (procedure body at lines  10: 18)
        === Code line metrics ===
           all lines           : 9
           code lines          : 9
           comment lines       : 0
           end-of-line comments: 0
           comment percentage  : 0.00
           blank lines         : 0

----------------------------------
Syntax Element Metrics Explained
----------------------------------

:dfn:`All Declarations`
 Total number of objects declared

:dfn:`All Statements`
 Total number of statements in file(s)

:dfn:`All Subprogram Bodies`
 Total number of subprograms in file(s)

:dfn:`All Type Definitions`
 Total number of types in file(s)

:dfn:`Logical SLOC`
 Total of declarations plus statements

:dfn:`Public Subprograms`
 Count of subprograms declared in visible part of package

:dfn:`Public Types`
 Count of types (not subtypes) declared in the visible part of a package plus in the visible part of a generic nested package

:dfn:`Maximal Construct Nesting`
 Maximal nesting level of composite syntactic constructs

:dfn:`Maximum Unit Nesting`
 Maximal static nesting level of inner program units

--------------------------------
Syntax Element Metrics Example
--------------------------------

.. columns::

 .. column::

  .. container:: latex_environment tiny

   .. code:: Ada
      :number-lines: 1

    package body Strings is

       function "&"
         (L, R : String_T)
          return String_T is
        (From_String (To_String (L) & To_String (R)));

       function To_String
         (S : String_T)
          return String is
        (S.Text (1 .. S.Length));

       function From_String
         (S : String)
          return String_T is
          L      : constant Natural :=
                   Integer'min (S'Length, Maximum_Length);
          Retval : String_T;
       begin
          Retval.Length        := L;
          Retval.Text (1 .. L) :=
              S (S'First .. S'First + L - 1);
          return Retval;
       end From_String;

    end Strings;

 .. column::

  .. container:: latex_environment tiny

   ::

    Metrics computed for src\strings.adb
    containing package body Strings

    Strings (package body - library item at lines  1: 26)
    === Element metrics ===
       all subprogram bodies    : 1
       all statements           : 3
       all declarations         : 9
       logical SLOC             : 12
       maximal unit nesting     : 1
       maximal construct nesting: 2

       "&" (expression function at lines  3: 6)
       === Element metrics ===
          all statements           : 0
          all declarations         : 2
          logical SLOC             : 2
          maximal construct nesting: 1
          all parameters           : 2
          IN parameters            : 2
          OUT parameters           : 0
          IN OUT parameters        : 0

       To_String (expression function at lines  8: 11)
       === Element metrics ===
          all statements           : 0
          all declarations         : 2
          logical SLOC             : 2
          maximal construct nesting: 1
          all parameters           : 1
          IN parameters            : 1
          OUT parameters           : 0
          IN OUT parameters        : 0

       From_String (function body at lines  13: 24)
       === Element metrics ===
          all statements           : 3
          all declarations         : 4
          logical SLOC             : 7
          maximal construct nesting: 1

------------------------------
Complexity Metrics Explained
------------------------------

:dfn:`Average Complexity`
    Total Cyclomatic Complexity divided by total number of subprograms

:dfn:`Cyclomatic Complexity`
    McCabe cyclomatic complexity (number of independent paths in the control flow graph)

:dfn:`Essential Complexity`
    McCabe essential complexity (cyclomatic complexity after removing blocks with single entry/exit points)

:dfn:`Expression Complexity`
    Complexity introduced by short-circuit control forms only

:dfn:`Maximum Loop Nesting`
    Maximum depth of nested loops

:dfn:`Statement Complexity`
    Complexity introduced by control statements only, without taking into account short-circuit forms 

---------------------------------
Understanding McCabe Complexity
---------------------------------

:url:`http://www.mccabe.com/pdf/mccabe-nist235r.pdf`

+ Given a control flow graph of a program

  + E - number of edges
  + N - number of nodes
  + P - number of connected components (exit nodes)

+ The complexity *v(G)* is computed by:

.. math::

  v(G) = E - N + 2 * P

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

----------------------------
Complexity Metrics Example
----------------------------

.. columns::

 .. column::

  .. container:: latex_environment tiny

   .. code:: Ada
      :number-lines: 1

    with Ada.Text_IO; use Ada.Text_IO;
    package body Example is

       procedure Internal (C : Character) is
       begin
          Put (C);
       end Internal;

       -- Print the prompt
       procedure Example (S1, S2 : String_T) is
          S : constant String := To_String (S1 & S2);
       begin
          for C of S
          loop
             Internal (C);
          end loop;
          New_Line;
       end Example;

    end Example;

 .. column::

  .. container:: latex_environment tiny

   ::

    Metrics computed for src\example.adb
    containing package body Example
    Example (package body - library item at lines  2: 20)

       Internal (procedure body at lines  4: 7)
       === Complexity metrics ===
          statement complexity     : 1
          expression complexity    : 0
          cyclomatic complexity    : 1
          essential complexity     : 1
          maximum loop nesting     : 0
          extra exit points        : 0

       Example (procedure body at lines  10: 18)
       === Complexity metrics ===
          statement complexity     : 2
          expression complexity    : 0
          cyclomatic complexity    : 2
          essential complexity     : 1
          maximum loop nesting     : 1
          extra exit points        : 0

    === Average complexity metrics ===
          statement_complexity     :  1.50
          expression_complexity    :  0.00
          cyclomatic_complexity    :  1.50
          essential_complexity     :  1.00
          max_loop_nesting         :  1.00

-----------------------------
Coupling Metrics Explained
-----------------------------

+ Measures dependencies between given entity and other entities in the program

  + High coupling may signal potential issues with maintainability

+ Metrics computed:

  :dfn:`Object-oriented coupling`
     Classes in traditional object-oriented sense

  :dfn:`Unit coupling`
     All units making up a program

  :dfn:`Control coupling`
     Dependencies between unit and other units that contain subprograms

------------------
Coupling Metrics
------------------

+ Uses Ada's approach to definition of *class*, but only for polymorphic classes:

  + Tagged types declared within packages
  + Interface types declared within packages

+ Two kinds of coupling computed

  :dfn:`Fan-out coupling`
    Number of classes given class depends on

  :dfn:`Fan-in coupling`
    Number of classes that depend on given class

+ Package bodies and specs for *classes* are both considered when computing dependencies

--------------------------
Coupling Metrics Example
--------------------------

.. columns::

 .. column::

  .. container:: latex_environment tiny

   .. code:: Ada

    package Strings is
       type String_T is tagged private;
       function Length (S : String_T) return Natural;
       function "&" (L, R : String_T) return String_T;
       function To_String (S : String_T) return String;
       function From_String (S : String) return String_T;
    private
       Maximum_Length : constant := 100;
       subtype Index_T is Natural range 0 .. Maximum_Length;
       type String_T is tagged record
          Length : Index_T := 0;
          Text   : String (1 .. Maximum_Length);
       end record;
    end Strings;

    with Strings; use Strings;
    package Example is
       procedure Example (S1, S2 : String_T);
    end Example;

    with Example; use Example;
    with Strings; use Strings;
    procedure Main is
       S1 : constant String_T := From_String ("Hello ");
       S2 : constant String_T := From_String ("World");
    begin
       Example.Example (S1, S2);
    end Main;

 .. column::

  .. container:: latex_environment tiny

   ::

    Coupling metrics:
    =================
       Unit Example (src\example.ads)
          control fan-out coupling  : 1
          control fan-in coupling   : 1
          unit fan-out coupling     : 1
          unit fan-in coupling      : 1

       Unit Main (src\main.adb)
          control fan-out coupling  : 2
          control fan-in coupling   : 0
          unit fan-out coupling     : 2
          unit fan-in coupling      : 0

       Unit Strings (src\strings.ads)
          tagged fan-out coupling   : 0
          hierarchy fan-out coupling: 0
          tagged fan-in coupling    : 0
          hierarchy fan-in coupling : 0
          control fan-out coupling  : 0
          control fan-in coupling   : 2
          unit fan-out coupling     : 0
          unit fan-in coupling      : 2

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
