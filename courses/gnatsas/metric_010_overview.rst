*******************
GNAT Metrics Tool
*******************

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

------------------------------------------------------
Overview of GNAT Metrics Tool :toolname:`gnatmetric`
------------------------------------------------------

+ Utility for computing various program metrics
+ Select desired metrics from:

  + Lines
  + Complexity
  + Contract
  + Syntax elements (e.g. nesting levels, number of parameters)
  + Coupling

+ Configurable scope of analysis

  + Current file
  + Current project
  + Current project and subprojects

+ :toolname:`GNAT Studio` provides a GUI interface

  + Selecting the metrics to compute
  + Selecting the scope of analysis
  + Displaying the results

---------------------------------------
Invoking From :toolname:`GNAT Studio`
---------------------------------------

* Start analysis

   .. image:: ../../images/gnatmetric/menu_cascade.jpg

* Select options and perform analysis

   .. image:: ../../images/gnatmetric/execute_dialog.jpg

---------------------------------------
Invoking From the Command Line
---------------------------------------

* Command line help (partial output)

  .. container:: latex_environment footnotesize

    :command:`gnatmetric --help`

    ::

      usage: gnatmetric [options] {filename}
       options:
       --version - Display version and exit
       --help    - Display usage and exit

       -Pproject        - Use project file project. Only one such switch can be used
       -U               - process all sources of the argument project
       -U main          - process the closure of units rooted at unit main
       --no-subprojects - Process sources of root project only
       -Xname=value     - specify an external reference for argument project file
       --subdirs=dir    - specify subdirectory to place the result files into
       -eL              - follow all symbolic links when processing project files

       --verbose    - verbose mode
       --quiet      - quiet mode

* Command line invocation

  .. container:: latex_environment footnotesize

   :command:`gnatmetric -P sdc.gpr -U --lines-all --complexity-all --syntax-all --coupling-all`

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

    - Process the closure of units rooted at unit :ada:`main`

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

================
Output Control
================

-------------------
Generated Outputs
-------------------

:toolname:`gnatmetric` has three types of outputs

* Execution log

  * Text output from command

* Command results

  * Text files for each unit processed
  * :filename:`<ada-filename>.metrix`

* Complete results

  * XML file containing results for all units processed
  * :filename:`metrix.xml`

-------------------------------
Controlling Output Generation
-------------------------------

* From :toolname:`GNAT Studio`

  * *Execution log* generated and stored in :filename:`gnathub/logs/gnatmetric.log` in object file folder
  * *Command results* generated and stored in object file folder
  * *Complete results* generated and stored in object file folder

* From the command line

  * When setting switch :command:`--no-text-output`

     * *Execution log* not generated
     * *Command results* not generated
     * *Complete results* generated and stored in object file folder

  * Without switch :command:`--no-text-output`

     * *Execution log* displayed on console
     * *Command results* generated and stored in object file folder
     * *Complete results* only generated if switches :command:`--generate-xml-output` or :command:`--generate-xml-schema` specified

----------------
Execution Log
----------------

:command:`gnatmetric -P default.gpr -U --lines-all`

::

  Line metrics summed over 11 units
    all lines            : 141
    code lines           : 118
    comment lines        : 1
    end-of-line comments : 0
    comment percentage   : 0.84
    blank lines          : 22

  Average lines in body: 7.33

-----------------
Command Results
-----------------

:command:`gnatmetric -P default.gpr -U --lines-all`

::

  Metrics computed for src\line_metrics_example.adb
  containing package body Line_Metrics_Example

  === Code line metrics ===
    all lines           : 19
    code lines          : 15
    comment lines       : 1
    end-of-line comments: 0
    comment percentage  : 6.25
    blank lines         : 3

  Average lines in body: 6.00

  Line_Metrics_Example (package body - library item at lines  2: 19)

  === Code line metrics ===
     all lines           : 18
     code lines          : 14
     comment lines       : 1
     end-of-line comments: 0
     comment percentage  : 6.66
     blank lines         : 3

     Internal (procedure body at lines  4: 7)

     === Code line metrics ===
        all lines           : 4
        code lines          : 4
        comment lines       : 0
        end-of-line comments: 0
        comment percentage  : 0.00
        blank lines         : 0

     Example (procedure body at lines  10: 17)

     === Code line metrics ===
        all lines           : 8
        code lines          : 8
        comment lines       : 0
        end-of-line comments: 0
        comment percentage  : 0.00
        blank lines         : 0

------------------
Complete Results
------------------

:command:`gnatmetric -P default.gpr -U --lines-all --generate-xml-output`

*(partial file)*

::

  <file name="C:\temp\gnatmetric\src\line_metrics_example.adb">
     <metric name="all_lines">19</metric>
     <metric name="code_lines">15</metric>
     <metric name="comment_lines">1</metric>
     <metric name="eol_comments">0</metric>
     <metric name="comment_percentage">6.25</metric>
     <metric name="blank_lines">3</metric>
     <metric name="average_lines_in_bodies">6.00</metric>
     <unit name="Line_Metrics_Example" kind="package body" line="2" col="1">
        <metric name="all_lines">18</metric>
        <metric name="code_lines">14</metric>
        <metric name="comment_lines">1</metric>
        <metric name="eol_comments">0</metric>
        <metric name="comment_percentage">6.66</metric>
        <metric name="blank_lines">3</metric>
        <unit name="Internal" kind="procedure body" line="4" col="4">
           <metric name="all_lines">4</metric>
           <metric name="code_lines">4</metric>
           <metric name="comment_lines">0</metric>
           <metric name="eol_comments">0</metric>
           <metric name="comment_percentage">0.00</metric>
           <metric name="blank_lines">0</metric>
        </unit>
        <unit name="Example" kind="procedure body" line="10" col="4">
           <metric name="all_lines">8</metric>
           <metric name="code_lines">8</metric>
           <metric name="comment_lines">0</metric>
           <metric name="eol_comments">0</metric>
           <metric name="comment_percentage">0.00</metric>
           <metric name="blank_lines">0</metric>
        </unit>
     </unit>
  </file>

------------------------------------------------
A Little More on Controlling Output Generation
------------------------------------------------

Some more switches to control output generation

.. container:: latex_environment footnotesize

  .. list-table::

    * - **output-dir**=dirname

      - Store :filename:`<ada-filename>.metrix` into :filename:`dirname`

    * - **generate-xml-output**

      - Generate XML output

    * - **generate-xml-schema**

      - Generate XML output and corresponding schema file

    * - **no-text-output**

      - No :filename:`<ada-filename>.metrix` or log files

    * - **output-suffix**=file-suffix

      - Add ``file-suffix`` to end of filename for file results

    * -

      - *Add "." if you want it to be a file extension*

    * - **global-file-name**=filename

      - Full path to the executable log file

    * - **xml-file-name**=filename

      - Full path to the XML file

    * - **short-file-names**

      - Use short source file names in output

=======================
Exploring the Results
=======================

------------------------
Line Metrics Explained
------------------------

.. list-table::

  * - **Average Lines In Body**

    - Average number of code lines in subprogram

  * -

    - bodies, task bodies, entry bodies and

  * -

    - package body executable code

  * - **All Lines**

    - Total number of lines in file(s)

  * - **Blank Lines**

    - Total number of blank in file(s)

  * - **Code Lines**

    - Total lines of code in file(s)

  * - **Comment Lines**

    - Total lines of comments in file(s)

  * - **Comment Percentage**

    - Comment lines divided by total of code lines

  * -

    - and comment lines

  * - **End-Of-Line Comments**

    - Count of code lines that also contain

  * -

    - comments

:dfn:`Code line` is a non-blank line that is not a comment

---------------------------
Line Metrics Code Example
---------------------------

.. code:: Ada
  :number-lines: 1

  with Ada.Text_IO; use Ada.Text_IO;
  package body Line_Metrics_Example is

     procedure Internal (C : Character) is
     begin
        Put (C);
     end Internal;

     -- Print the prompt
     procedure Example (S1, S2 : String) is
        S : constant String := S1 & S2;
     begin
        for C of S loop
           Internal (C);
        end loop;
        New_Line;
     end Example;

  end Line_Metrics_Example;

---------------------
Line Metrics Output
---------------------

:command:`gnatmetric -Pdefault.gpr --lines-all line_metrics_example.adb`

:filename:`line_metrics_example.metrix`

.. container:: latex_environment footnotesize

  ::

    === Code line metrics ===
      all lines           : 19
      code lines          : 15
      comment lines       : 1
      end-of-line comments: 0
      comment percentage  : 6.25
      blank lines         : 3

    Average lines in body: 6.00

    Line_Metrics_Example (package body - library item at lines  2: 19)

    === Code line metrics ===
       all lines           : 18
       code lines          : 14
       comment lines       : 1
       end-of-line comments: 0
       comment percentage  : 6.66
       blank lines         : 3

       Internal (procedure body at lines  4: 7)

       === Code line metrics ===
          all lines           : 4
          code lines          : 4
          comment lines       : 1
          end-of-line comments: 0
          comment percentage  : 0.00
          blank lines         : 0

       Example (procedure body at lines  10: 17)

       === Code line metrics ===
          all lines           : 8
          code lines          : 8
          comment lines       : 0
          end-of-line comments: 0
          comment percentage  : 0.00
          blank lines         : 0

----------------------------------
Syntax Element Metrics Explained
----------------------------------

.. list-table::

  * - **All Declarations**

    - Total number of objects declared

  * - **All Statements**

    - Total number of statements in file(s)

  * - **All Subprogram Bodies**

    - Total number of subprograms in file(s)

  * - **All Type Definitions**

    - Total number of types in file(s)

  * - **Logical SLOC**

    - Total of declarations plus statements

  * - **Public Subprograms**

    - Count of subprograms declared in

  * -

    - visible part of package

  * - **Public Types**

    - Count of types (not subtypes) declared in

  * -

    - the visible part of a package plus in

  * -

    - the visible part of a generic nested package

  * - **Maximal Construct Nesting**

    - Maximal nesting level of composite

  * -

    - syntactic constructs

  * - **Maximum Unit Nesting**

    - Maximal static nesting level of inner

  * -

    - program units

-------------------------------------
Syntax Element Metrics Code Example
-------------------------------------

.. code:: Ada
   :number-lines: 1

  package body Syntax_Metrics_Example is

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
       L      : constant Natural
              := Integer'Min (S'Length, Maximum_Length);
       Retval : String_T;
    begin
       Retval.Length        := L;
       Retval.Text (1 .. L) := S (S'First .. S'First + L - 1);
       return Retval;
    end From_String;

  end Syntax_Metrics_Example;

-------------------------------
Syntax Element Metrics Output
-------------------------------

:command:`gnatmetric -Pdefault.gpr --syntax-all syntax_metrics_example.adb`

:filename:`syntax_metrics_example.metrix`

.. container:: latex_environment footnotesize

  ::

    Syntax_Metrics_Example (package body - library item at lines  1: 25)

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

       From_String (function body at lines  13: 23)

       === Element metrics ===
          all statements           : 3
          all declarations         : 4
          logical SLOC             : 7
          maximal construct nesting: 1

------------------------------
Complexity Metrics Explained
------------------------------

.. list-table::

  * - **Average Complexity**

    - Total Cyclomatic Complexity divided by

  * -

    - total number of subprograms

  * - **Cyclomatic Complexity**

    - McCabe cyclomatic complexity (number of

  * -

    - independent paths in the control flow graph)

  * - **Essential Complexity**

    - McCabe essential complexity (cyclomatic

  * -

    - complexity after removing blocks with single

  * -

    - entry/exit points)

  * - **Expression Complexity**

    - Complexity introduced by short-circuit

  * -

    - control forms only

  * - **Maximum Loop Nesting**

    - Maximum depth of nested loops

  * - **Statement Complexity**

    - Complexity introduced by control statements

  * -

    - only, without taking into account

  * -

    - short-circuit forms 

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

---------------------------------
Complexity Metrics Code Example
---------------------------------

.. code:: Ada
   :number-lines: 1

   package body Complexity_Metrics_Example is

      procedure Example (S : in out String) is
         Retval : String (S'First .. S'Last);
         Next   : Integer := S'First;
         procedure Set (C : Character) is
         begin
            Retval (Next) := C;
            Next          := Next + 1;
         end Set;
      begin
         if S'Length > 0 then
            for C of reverse S loop
               Set (C);
            end loop;
         end if;
      end Example;

   end Complexity_Metrics_Example;

---------------------------
Complexity Metrics Output
---------------------------

:command:`gnatmetric -Pdefault.gpr --complexity-all complexity_metrics_example.adb`

:filename:`complexity_metrics_example.metrix`

.. container:: latex_environment footnotesize

  ::

    Complexity_Metrics_Example (package body - library item at lines  1: 19)

       Example (procedure body at lines  3: 17)

       === Complexity metrics ===
          statement complexity     : 3
          expression complexity    : 0
          cyclomatic complexity    : 3
          essential complexity     : 1
          maximum loop nesting     : 1
          extra exit points        : 0

          Set (procedure body at lines  6: 10)

          === Complexity metrics ===
             statement complexity     : 1
             expression complexity    : 0
             cyclomatic complexity    : 1
             essential complexity     : 1
             maximum loop nesting     : 0
             extra exit points        : 0

    === Average complexity metrics ===
          statement_complexity     :  2.00
          expression_complexity    :  0.00
          cyclomatic_complexity    :  2.00
          essential_complexity     :  1.00
          max_loop_nesting         :  1.00

-----------------------------
Coupling Metrics Explained
-----------------------------

+ Measures dependencies between given entity and other entities in the program

  + High coupling may signal potential issues with maintainability

+ Metrics computed:

  .. list-table::

    * - **Object-oriented coupling**

      - Classes in traditional object-oriented sense

    * - **Unit coupling**

      - All units making up a program

    * - **Control coupling**

      - Dependencies between unit and other units

    * -

      - that contain subprograms

------------------
Coupling Metrics
------------------

+ Uses Ada's approach to definition of *class*, but only for polymorphic classes:

  + Tagged types declared within packages
  + Interface types declared within packages

+ Two kinds of coupling computed:

  .. list-table::

    * - **Fan-out coupling**

      - Number of classes given class depends on

    * - **Fan-in coupling**

      - Number of classes that depend on given class

+ Package bodies and specs for *classes* are both considered when computing dependencies

-------------------------------
Coupling Metrics Code Example
-------------------------------

.. code:: Ada

   package Coupling_Metrics_Dependency is
      type Record_T is tagged private;
      function Set (A, B : Integer) return Record_T;
      function Get (A : Record_T) return Integer;
      function Add (A, B : Record_T) return Record_T;
   private
      type Record_T is tagged record
         Field1, Field2 : Integer;
      end record;
   end Coupling_Metrics_Dependency;

   with Coupling_Metrics_Dependency;
   use Coupling_Metrics_Dependency;
   package Coupling_Metrics_Example is
      procedure Example (L, R : Record_T);
   end Coupling_Metrics_Example;

   with Coupling_Metrics_Dependency;
   use Coupling_Metrics_Dependency;
   with Coupling_Metrics_Example;
   procedure Main is
      A : constant Record_T := Set (1, 2);
      B : constant Record_T := Set (30, 40);
   begin
      Coupling_Metrics_Example.Example (A, B);
   end Main;

-------------------------
Coupling Metrics Output
-------------------------

:command:`gnatmetric -Pdefault.gpr -U --coupling-all`

.. container:: latex_environment footnotesize

  ::

    Coupling metrics:
    =================
       Unit Coupling_Metrics_Dependency (coupling_metrics_dependency.ads)
          tagged fan-out coupling   : 0
          hierarchy fan-out coupling: 0
          tagged fan-in coupling    : 0
          hierarchy fan-in coupling : 0
          control fan-out coupling  : 0
          control fan-in coupling   : 2
          unit fan-out coupling     : 0
          unit fan-in coupling      : 2

       Unit Coupling_Metrics_Example (coupling_metrics_example.ads)
          control fan-out coupling  : 1
          control fan-in coupling   : 1
          unit fan-out coupling     : 1
          unit fan-in coupling      : 1

       Unit Main (main.adb)
          control fan-out coupling  : 2
          control fan-in coupling   : 0
          unit fan-out coupling     : 2
          unit fan-in coupling      : 0

=====
Lab
=====

.. include:: labs/metric_010_overview.lab.rst

=========
Summary
=========

-----------------
Closing Remarks
-----------------

+ See the GNAT User's Guide for further details of all the switches
+ :toolname:`gnatmetric` switches can be specified in a GPR file via the "Metrics" package
+ :toolname:`gnatmetric` is based on the LKQL library

  + Allows tool to parse files that may not actually compile
