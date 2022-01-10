***********************
:toolname:`GNATcheck`
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

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

------------------------------
:toolname:`GNATcheck` Is...
------------------------------

+ An **automated** coding standards checker
+ Capable of expressing a variety of rules

  + GNAT compiler **warnings and style** checks
  + Language-defined and GNAT-defined **restrictions**
  + Complexity **metrics**
  + **Specific** :toolname:`GNATcheck` rules

+ Qualified to DO-178 in several programs
+ Integrated in :toolname:`GNAT Studio`

--------------------
Required by DO-178
--------------------

   .. image:: gnatcheck/do178_table_a5.jpg

-----------------------------------------------
Conformance To Standards Requirement - DO-178
-----------------------------------------------

.. container:: latex_environment beamercolorbox {blueonorange}

   **6.3.4 Reviews and Analyses of the Source Code**

   [...]

   d. Conformance to standards

   The objective is to **ensure that the Software Code Standards were followed** during the development of the code, especially **complexity restrictions and code constraints** that would be consistent with the system safety objectives.

   Complexity includes the degree of coupling between software components, the nesting levels for control structures, and the complexity of logical or numeric expressions.

   This analysis also ensures that **deviations to the standards are justified**.

---------------------------------------
:toolname:`GNATcheck` Is An ASIS Tool
---------------------------------------

+ Sources must be **compilable**

  + Otherwise incomplete
  + Warnings and :toolname:`GNATcheck` will ignore those files
  + Changing with upcoming *lalcheck*

+ All source dependencies must be **available**

  + Those units named in :ada:`with`-clauses, transitively
  + Whether or not they are to be analyzed themselves

=============
Basic Usage
=============

-------------------------
Command Line Invocation
-------------------------

.. container:: latex_environment tiny

.. container:: latex_environment scriptsize

  .. code::

    gnatcheck [options] {filename} {-files=filename} [-cargs gcc_switches] -rules rule_switches

.. list-table::
   :header-rows: 1

   * - Argument

     - Description

   * - {filename}

     - File to analyze (wildcards allowed)

   * - {files=filename}

     - :filename:`filename` specifies text file containing list of files to analyze

   * - -rules rule_switches

     - Rules to apply for analysis

Where ``rule_switches`` can be any combination of the following:

  .. list-table::
     :header-rows: 1

     * - Switch

       - Explanation

     * - -from=filename

       - read rule options from :filename:`filename`

     * - +R<rule_id>[:param]

       - turn ON a given rule [with given parameter]

     * - -R<rule_id>

       - turn OFF a given rule

     * - -R<rule_id>:param

       - turn OFF some of the checks for a given  rule,

     * - 

       - depending on the specified parameter

----------------------
Some Useful Options
----------------------

.. container:: latex_environment small

  .. list-table::
   :header-rows: 1

   * - Option

     - Description

   * - ``--help``

     - Usage information

   * - ``-h``

     - List currently implemented rules

   * - ``--write-rules=<filename>``

     - Create text file containing all rules

   * -

     - (Rules will be disabled,

   * -

     - and have a comment indicating behavior)

   * - ``--show-rule``

     - Append rule names to messages

   * - ``-o filename``

     - Specify the name of the **report** file

   * -

     - Default is :filename:`[toolprefix-]gnatcheck.out`

===================
Lab - Basic Usage
===================

---------------------
Verify Installation
---------------------

* Copy :filename:`labs` folder locally

  * :filename:`source` folder - Ada code
  * :filename:`coding_standards.rules`  - :toolname:`GNATcheck` rules to apply

* Open a command prompt in the :filename:`labs` folder and run the command:

.. container:: latex_environment scriptsize

    ``gnatcheck --show-rule source\*.ad? -rules -from=coding_standard.rules``

.. container:: latex_environment scriptsize

  .. list-table::

    * - --show-rule

      - Append rule name to message

    * - source\*.ad?

      - Examine all files in :filename:`source` folder

    * - -rules -from=coding_standard.rules

      - Apply rules from file :filename:`coding_standard.rules`

-------------------
Results (Partial)
-------------------

.. container:: latex_environment tiny

  ::

    compiler_checks.adb:6:04: warning: violation of restriction "No_Tasking" [Restrictions]
    compiler_checks.adb:12:07: warning: variable "T" is assigned but never read [Warnings:m]
    compiler_checks.adb:20:04: (style) "end Proc" required [Style_Checks]
    feature_usage.adb:3:04: branching in inlined subprogram (line 5) [Complex_Inlined_Subprograms]
    feature_usage.adb:6:10: block statement [Blocks]
    feature_usage.ads:4:44: declaration of abstract type [Abstract_Type_Declarations]
    feature_usage.ads:5:04: declaration of controlled type [Controlled_Type_Declarations]
    feature_usage.ads:7:34: anonymous subtype [Anonymous_Subtypes]
    object_orientation.ads:28:07: derivation tree is too deep (6) [Deep_Inheritance_Hierarchies]
    program_practice.adb:8:20: use of predefined OR for boolean type [Non_Short_Circuit_Operators]
    program_practice.adb:20:15: OTHERS choice in case statement [OTHERS_In_CASE_Statements]
    program_practice.adb:26:12: OTHERS choice in exception handler [OTHERS_In_Exception_Handlers]
    program_practice.ads:4:22: anonymous array type [Anonymous_Arrays]
    program_practice.ads:5:23: OTHERS choice in aggregate [OTHERS_In_Aggregates]
    program_structure.ads:23:16: deeply nested generic (4) [Deeply_Nested_Generics]
    readability.ads:3:09: wrong suffix in type name [Identifier_Suffixes:Type_Suffix]
    readability.ads:6:09: wrong suffix in access type name [Identifier_Suffixes:Access_Suffix]
    readability.ads:14:04: object does not have casing specified (mixed) [Identifier_Casing:Others]
    spark_ada.ads:5:27: comparison of Boolean values [Boolean_Relational_Operators]

--------------------------------
Accessing the Reference Manual
--------------------------------

* GNAT Studio

  * :menu:`Help` |rightarrow| :menu:`GNAT` |rightarrow| :menu:`GNATcheck Reference Manual`

.. image:: gnatcheck/rm_browser.png
   :width: 50%

* RM files installed in installation directory :filename:`share` folder

  ::

    share/doc/gnat/html/gnatcheck_rm/gnatcheck_rm.html
    share/doc/gnat/info/gnatcheck_rm.info
    share/doc/gnat/pdf/gnatcheck_rm.pdf
    share/doc/gnat/txt/gnatcheck_rm.txt

===============================
GNAT Studio and Project Files
===============================

---------------------
Using Project Files
---------------------

+ Recommended approach
+ Convenient for multiple source **directories**
+ Convenient for checking multiple **projects**

  + Root project and dependencies

+ Usable with **both** command line and IDEs
+ Project file specified via switch :code:`-P<project.gpr>` as usual
+ :toolname:`GNATcheck` options specified in project file via :ada:`package Check`

------------------------
Rules File In GPR File
------------------------

* Direct manual entry is supported
* Convenient due to typically large number of rules

  * Also allows easier sharing of a common set of rules

* Graphical entry through :toolname:`GNAT Studio`

.. image:: gnatcheck/properties_dialog.png

------------------------------------------------
:toolname:`GNATcheck` Switches In Project File
------------------------------------------------

.. code:: Ada

   project Gnatcheck_Example is
      package Check is
         for Default_Switches ("Ada") use
            ("-rules", -- DON'T FORGET THIS!
             "-from=coding_standard");
      end Check;
   end Gnatcheck_Example;

------------------------------
Individual Rules In GPR File
------------------------------

* Must be added by hand

   .. code:: Ada

      project Gnatcheck_Example is
         package Check is
            for Default_Switches ("Ada") use
               ("-rules",
                "+RAbstract_Type_Declarations",
                "+RAnonymous_Arrays",
                "+RLocal_Packages",
                "+RFloat_Equality_Checks",
                "+REXIT_Statements_With_No_Loop_Name",
                "+RStyle_Checks:e");
         end Check;
      end Gnatcheck_Example;

* Can combine rules file and individual rules

   .. code:: Ada

      project Gnatcheck_Example is
         package Check is
            for Default_Switches ("Ada") use
               ("-rules",
                "+RFloat_Equality_Checks",
                "-from=coding_standard");
         end Check;
      end Gnatcheck_Example;

=====================================
Lab - GNAT Studio and Project Files
=====================================

-------------------------
Analyze via GNAT Studio
-------------------------

* Start :toolname:`GNAT Studio`

* Open project :filename:`default.gpr` in :filename:`labs` folder

* Select :menu:`Edit` |rightarrow| :menu:`Project Properties...` |rightarrow| :menu:`GNATcheck` (under switches)

* Browse to and select :filename:`coding_standard.rules`

* :menu:`Save` options

* :menu:`Analyze` |rightarrow| :menu:`Coding Standard` |rightarrow| :menu:`Check Root Project`

---------
Results
---------

.. image:: gnatcheck/results_in_gnatstudio.png

--------------------------
GNAT Studio Right-Clicks
--------------------------

.. container:: columns

  .. container:: column

    * Entire Project

    .. image:: gnatcheck/gnatstudio_rightclick_project.png
       :width: 70%

    * Single Source Directory

    .. image:: gnatcheck/gnatstudio_rightclick_directory.png
       :width: 70%

  .. container:: column

    * Specific File

    .. image:: gnatcheck/gnatstudio_rightclick_file.png
       :width: 70%

    * Within Editor

    .. image:: gnatcheck/gnatstudio_rightclick_editor.png
       :width: 50%

==================
Specifying Rules
==================

-------------------
Basic Rule Syntax
-------------------

* ``+R<rule name>``

   - Activates specified rule

* ``+R<rule name>:<parameter>``

   - Activates specified rule, with a value for the parameter

* ``-R<rule name>``

   - Deactivates previously activated rule

* ``-R<rule name>:<parameter>``

   - Deactivates rule previously activated with the parameter value

**Rule names are case insensitive**

-------------------
Sample Rules File
-------------------

.. container:: latex_environment tiny

  .. include:: examples/standard_file.rules
     :code:

* Notes

  * Comments are allowed (using standard Ada comment syntax)

  * ``-from=<rule_option_filename>``

    * Textually includes rules from specified file name
    * Included files can also contain ``-from=<rule_option_filename>``

------------------------------------
Code and Results Using Sample File
------------------------------------

.. container:: columns

  .. container:: column

    .. container:: latex_environment tiny

      .. code:: Ada
         :number-lines: 1

         package Example is
           type T is abstract tagged private;
           procedure P (X : T) is abstract;
           package Inner is
              type My_Float is digits 8;
              function Is_Equal
                (L, R : My_Float)
                 return Boolean is (L = R);
           end Inner;
         private
           type T is abstract tagged null record;
         end;

  .. container:: column

    .. container:: latex_environment tiny

      .. code::

        -- GNATcheck output
        example.ads:2:07: declaration of abstract type

        example.ads:4:07: declaration of local package



        example.ads:8:35: use of equality operation for float values


        example.ads:11:33: declaration of abstract type

---------------------------------
Rule Exemptions Via Source Code
---------------------------------

+ Uses GNAT-specific :ada:`pragma Annotate`

  + Processed by source-oriented tools **external** to compiler
  + :ada:`Pragma` syntax checked by compiler but no compilation effect

+ :toolname:`GNATcheck` specific usage

     .. code:: Ada

        pragma Annotate (GNATcheck,
                         <exemption_control>,
                         <rule_name>,
                         [justification]);

  .. list-table::
     :header-rows: 1

     * - Parameter

       - Value

     * - exemption_control

       - Exempt_On | Exempt_Off

     * - rule_name

       - <string_literal>

     * - justification

       - <string_literal>

* Usage errors are detected by :toolname:`GNATcheck`

---------------------------
Adding Exemptions to Code
---------------------------

.. code:: Ada
   :number-lines: 1

   package Example is
      type T is abstract tagged private;
      procedure P (X : T) is abstract;
      package Inner is
         type My_Float is digits 8;
         pragma Annotate (Gnatcheck,
                          Exempt_On,
                          "Float_Equality_Checks",
                          "this one is fine");
         function Is_Equal
           (L, R : My_Float)
            return Boolean is (L = R);
         pragma Annotate (Gnatcheck,
                          Exempt_Off,
                          "Float_Equality_Checks");
      end Inner;
   private
      type T is abstract tagged null record;
   end;

-----------------------------
Sample Report File Produced
-----------------------------

.. container:: latex_environment tiny

  ::

    GNATCheck report
    
    date              : 2021-12-22 09:46
    gnatcheck version : gnatcheck Pro 21.1 (20210111)
    command line      : gnatcheck.exe example.ads -rules -from=standard_file.rules
    runtime           : <default>
    coding standard   : standard_file.rules
    list of sources   : gnatcheck-source-list.out
    
    1. Summary
       fully compliant sources               : 0
       sources with exempted violations only : 0
       sources with non-exempted violations  : 1
       unverified sources                    : 0
       total sources                         : 1
       ignored sources                       : 0
    
       non-exempted violations               : 4
       rule exemption warnings               : 0
       compilation errors                    : 0
       exempted violations                   : 1
       gnatcheck warnings                    : 0
    
    2. Exempted Coding Standard Violations
    
    example.ads:12:31: use of equality operation for float values
       (this one is fine)
    
    3. Non-exempted Coding Standard Violations
    
    example.ads:2:04: declaration of abstract type
    example.ads:4:04: declaration of local package
    example.ads:18:30: declaration of abstract type
    example.ads:19:01: (style) "end Example" required
    
    4. Rule exemption problems
       no rule exemption problems detected
    
    5. Language violations
       no language violations detected
    
    6. Gnatcheck warnings
       no gnatcheck warnings issued

==================
Available Rules
==================

--------------------------------------
What Predefined Rules Are Available?
--------------------------------------

+ Defined by the language and AdaCore

  + Using :ada:`pragma Restrictions`

+ Defined by GNAT compiler

  + Style checks
  + Additional identifiers for :ada:`pragma Restrictions`

+ Defined by :toolname:`GNATcheck` itself

  + Based on *Guide for the Use of the Ada Programming Language in High Integrity Systems*  (ISO/IEC TR 15942)
  + Based on customers' certification requirements
  + Others...

+ All can be listed by :toolname:`GNATcheck` with :command:`-h` switch

  + Lists rule identifiers with very brief descriptions

---------------------------------
Predefined Rules Categorization
---------------------------------

+ Style-Related Rules

  + Tasking
  + Object Orientation
  + Portability
  + Program Structure
  + Programming Practice
  + Readability
  + Source Code Presentation

+ Feature Usage Rules
+ Metrics-Related Rules
+ SPARK Ada Rules

---------------------------------
Rules for Compiler Style Checks
---------------------------------

+ Allows expressing compiler style checks as rules
+ Syntax

   ``+RStyle_Checks:style-string-literals``

+ Example: enabling GNAT's built-in style checks

  + As compiler switch or :ada:`pragma Warnings` argument

      :command:`-gnaty`

  + As :toolname:`GNATcheck` rule

      + Enable: ``-RStyle_Checks:y``
      + Disable: ``+RStyle_Checks:yN``

-----------------------------
Rules for Compiler Warnings
-----------------------------

+ Allows expressing compiler warning switches as rules
+ Syntax

   ``+RWarnings:warning-string``

+ Example: enabling (most of the) optional warnings

  + As compiler switch or :ada:`pragma Warnings` argument

      :command:`-gnatwa`

  + As :toolname:`GNATcheck` rule

      + Enable: ``+RWarnings:a``
      + Disable: ``+RWarnings:A`` - *must use individual disabler characters*

---------------------------------------
Rules for Language Restriction Checks
---------------------------------------

+ Allows expressing :ada:`pragma Restrictions` as rules

  + And GNAT-defined :ada:`pragma Restriction_Warnings`

+ Syntax

   ``+RRestrictions:<restrictions-parameter>``

+ Example: disabling dynamic dispatching

  + As :ada:`pragma Restrictions` argument

     :ada:`pragma Restrictions (No_Dispatch);`

  + As :toolname:`GNATcheck` rule

     ``+RRestrictions:No_Dispatch``

  + Disabled using -RRestrictions with parameter

-------------------------------------
Example for Detecting Implicit Code
-------------------------------------

.. code:: Ada
   :number-lines: 1

   with F; -- a function
   package P is
      -- An implicit heap allocation in GNAT
      Obj : array (1 .. F) of Integer;
   end P;

.. container:: latex_environment scriptsize

  * *Rules File*

    ::

      +RRestrictions:No_Implicit_Heap_Allocations -- defined by Ada
      +RRestrictions:No_Implicit_Loops            -- defined by GNAT
      +RRestrictions:No_Implicit_Dynamic_Code     -- defined by GNAT
      +RRestrictions:No_Implicit_Conditionals     -- defined by GNAT

  * Output

    ::

      p.ads:3:4: warning: violation of restriction "No_Implicit_Heap_Allocations"

---------------------------------------------
Generating Rules File From The Command Line
---------------------------------------------

:command:`gnatcheck --write-rules=<filename>`

+ Creates a file with name as specified

+ File contains all the rules, all turned off

  + Each rule has a short description

+ Edit this file to get your own rules file
+ You will not use all the defined rules!

  + Many of them conflict with others
  + Define the subset that matches your (existing) code

===================================
Interactive Rules File Generation
===================================
    
---------------------------------
Graphically Editing Rules Files
---------------------------------

* :menu:`Analyze` |rightarrow| :menu:`Coding Standard` |rightarrow| :menu:`Edit Rules File`

  .. image:: gnatcheck/rules_editor.png

* Notes

  * You must specify a file before the rules buttons are active
  * This dialog does **NOT** modify the GPR file

    * You need to set the *Project Properties* to point to this file

-------------------------
Comments in Rules Files
-------------------------

* If you specify an existing file that contains comments, you will get a warning message!

  .. image:: gnatcheck/rules_comment_warning.png

  * Even if you don't change anything, pressing Save removes the comments
  * To maintain comments, edit the file using a text editor

------------------------------
Edit Rules File Dialog Boxes
------------------------------

.. image:: gnatcheck/rules_feature_usage_dialog.png

.. image:: gnatcheck/rules_style_related_dialog.png

=========================================
Lab - Interactive Rules File Generation
=========================================

------------------
Build Rules File
------------------

* Using :toolname:`GNAT Studio` create a new project with the original :filename:`source` files

* Use :toolname:`GNAT Studio` to build a rules file to identify the following occurrences:

  * Abstract types
  * Controlled Types
  * Anonymous subtypes
  * Type names that do not end with ``_T``
  * Anonymous arrays
  * :ada:`declare` blocks
  * :ada:`new` keyword
  * Use of :ada:`others` clauses
  * Aggregates without qualification
  * Inlined subprograms with conditional code
  * Inequality comparions of boolean objects
  * Use of non-short-circuit boolean operations
  * Incorrect identifier naming
  * Missing :ada:`end` label

------------------
Perform Analysis
------------------

* Run :toolname:`GNATcheck` on source file with created rules file

  * Total messages per file should be:

    * compiler_checks.adb - 3
    * feature_usage.adb - 2
    * feature_usage.ads - 7
    * object_orientation.ads - 7
    * program_practice.adb - 3
    * program_practice.ads - 3
    * readability.ads - 7
    * spark_ada.ads - 1

-------------------
Fix The Problems!
-------------------

* Use the ``Locations`` window to jump to each problem

  * If the wrench icon is present, click on it to have :toolname:`GNAT Studio` make the fix
  * If an identifier is incorrect:

    * Select the identifier
    * Right-click and select ``Refactoring``
    * Select ``Rename <name>`` and enter the new name

  * If there's no easy way to fix it, annotate it

=====================
Other Popular Rules
=====================

--------------------
Visible_Components
--------------------

Flag all the type declarations located in the visible part of a library package or a library generic package that can declare a visible component.

.. code:: Ada
   :number-lines: 1

   with Types;
   package Example is
      type Record_T is record
         I : Integer;
         B : Boolean;
      end record;
      type Tagged_Record_T is tagged record
         I : Integer;
         B : Boolean;
      end record;
      type Private_Extension is new Types.Tagged_Private with private;
      type Non_Private_Extension is new Types.Tagged_Private with record
         B : Boolean;
      end record;
   private
      type Rec is tagged record
         I : Integer;
      end record;
      type Private_Extension is new Types.Tagged_Private with record
         C : Rec;
      end record;
   end Example;

::

   example.ads:3:04: type defines publicly accessible components
   example.ads:7:04: type defines publicly accessible components
   example.ads:12:04: type defines publicly accessible components

---------------------
USE_PACKAGE_Clauses
---------------------

Flag all use clauses for packages; use type clauses are not flagged.

.. code:: Ada
   :number-lines: 1

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Sample is
   begin
      Put_Line ("Hello, World");
   end Sample;

::

   sample.adb:1:19: use clause for package

Should we use "use clauses"?

  + Some say they decrease readability by removing info
  + Some say they help readability by reducing noise
  + Nowadays a decent IDE can tell you everything...

------------------
Numeric_Literals
------------------

Flag each use of a numeric literal

* Optional Parameters

  * *N* - Maximum possible value to remain unflagged
  * *All* - All integer literals are flagged
  * *Statements_Only* - Only literals in statements are flagged

* With no parameters, max unflagged value is 1 and is not limited to statements

* Exceptions to the rule: 

  * Occurring in initialization expression for a constant declaration or a named number declaration
  * Occurring in an aspect definition or in an aspect clause

.. code:: Ada
   :number-lines: 1

   procedure Sample (Value : in out Integer) is
      Const     : constant := 123;
      Temporary : Integer  := Value + (3 * Const);
   begin
      Value := Value + Temporary + 1;
      Value := Value * 5;
   end Sample;

::

  sample.adb:3:37: numeric literal (3) outside a constant declaration
  sample.adb:6:21: numeric literal (5) outside a constant declaration

---------------------------
Unassigned_OUT_Parameters
---------------------------

Flag procedures' :ada:`out` parameters that are not assigned.

.. code:: Ada
   :number-lines: 1

   package body Example is
      procedure One (Flag : out Boolean) is
      begin
         if Global_Object > 0 then
            Global_Object := 0;
         end if;
      end One;
      procedure Two (Flag : out Boolean) is
      begin
         if Global_Object * Global_Object > 0 then
            Flag := True;
         else
            Flag := False;
         end if;
      exception
         when others =>
            null;
      end Two;
   end Example;

::

  example.adb:2:04: procedure body does not define values for OUT parameters: Flag
  example.adb:16:07: exception handler does not define values for OUT parameters: Flag

Always a coding error

  + Worst case: *pass-by-copy* mechanism will copy something (junk?) back

=========
Summary
=========

--------------------------------
Why use :toolname:`GNATcheck`?
--------------------------------

+ Automated coding standards verifier/checker
+ Capable of expressing a variety of rules

  + GNAT compiler warnings and style checks
  + Language restrictions (via pragma Restrictions)
  + Complexity metrics (GNATmetric results)
  + Others, including SPARK related rules

* Enforces consistent appearance and behavior across code base
