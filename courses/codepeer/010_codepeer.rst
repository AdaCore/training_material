.. role:: ada(code)
    :language: ada

**********
CodePeer
**********

==========================
Advanced Static Analysis
==========================

--------------------------
What is Static Analysis?
--------------------------

+ **Symbolic** interpretation of **source code**

  + Find what could go wrong
  + No execution

+ **Formally** verifying **high level** or **abstract** properties

  + Strong guarantees

+ *May* be exhaustive

  + All possible errors are reported
  + No false negatives; there may be false positives
  + If the analyzer does not report a problem, there is no problem

---------------------------------
Why Static Analysis Saves Money
---------------------------------

Shifts costs from later, expensive phases to earlier, cheaper phases

.. image:: cost_to_fix_bugs.png

-------------------
Why Use CodePeer?
-------------------

+ Efficient, potentially exhaustive code reviewer

  + Identifies run-time errors with a **level of certainty**

    + E.g. buffer overflows, division by zero

  + Flags legal but **suspect** code

    + Typically logic errors

+ Detailed subprograms analysis

  + Including contracts (preconditions, postconditions)
  + **Deduced implicit** specification checked with **written explicit** specification

+ Can analyze existing code bases

  + Detect and remove **latent bugs**
  + Legacy code
  + Code from external sources

===================
CodePeer Overview
===================

------------------------------
CodePeer In A Nutshell (1/2)
------------------------------

+ :toolname:`CodePeer` is a static analysis tool

  + Provides feedback **before** execution and test
  + Provides *as-built documentation* for code reviews

+ Helps identifying and eliminating **vulnerabilities and bugs** early
+ Modular

  + Analyze entire project or a single file
  + Configurable as more or less strict

+ Scalable

  + Can filter out or emphasize certain issues
  + Can analyze the difference between baselines / versions

------------------------------
CodePeer In A Nutshell (2/2)
------------------------------

+ Large Ada support

  + Usable with Ada 83, 95, 2005, 2012
  + No vendor lock-in, supports GNAT, Apex, GHS, ObjectAda, VADS

+ Bundled with a Coding Standards Checker and a Metrics Tool

  + :toolname:`GNATcheck` and :toolname:`GNAT Metric`

+ Detects runtime and logic errors exhaustively

  + Initialization errors, run-time errors and assertion failures (16 rules)
  + Race condition errors: unprotected access to globals (3 rules)

+ Warns on dead or suspicious code (17 rules)

----------------------
CodePeer Integration
----------------------

+ Output: textual, XML, CSV, HTML
+ Command-line tool (uses GNAT project files)
+ Interactive use in :toolname:`GNAT Studio` and :toolname:`GNATbench` IDEs
+ Integration with Jenkins (continuous builder)
+ Integration with SonarQube (continuous inspection of code quality)

-------------------
Infer Integration
-------------------

+ Infer for Ada on top of main analysis
+ Based on Facebook's Infer engine
+ Adds **lightweight** checks
+ Disable with ``--no-infer`` switch

-----------------------------
Typical Users And Use Cases
-----------------------------

+ Developers, during code-writing

  + **Fix** (local) problems before integration

+ Reviewers

  + **Annotate** code with analysis of potential problems
  + **Analyse** specific CWE issues

+ Project managers and quality engineers

  + **Track** reported vulnerabilities regularly
  + **Identify** new issues quickly

+ Software auditors

  + **Identify** overall vulnerabilities or hot spots
  + **Verify** compliance to quality standards

=================
Getting Started
=================

------------------------------
Command Line Interface (1/2)
------------------------------

:command:`codepeer -P <project> [-level <level>]` ...

``-P <gpr project-file>``
   NB: All files from the project (including subprojects) will be analyzed.

   Tip: if missing a project file, use the ``--simple-project`` switch

``-level 0|1|2|3|4|min|max``
   Specify the level of analysis performed:

  + 0, min (default): fast and light checkers
  + 1: fast and per subprogram analysis
  + 2: more accurate/slower, automatic partitioning per set of units
  + 3: more accurate and much slower
  + 4, max: global analysis, no automatic partitioning

  Warning: Level 4 may exceed memory capacity and take a very long time

------------------------------
Command Line Interface (2/2)
------------------------------

:command:`codepeer` ... :command:`[-output-msg[-only]] [-html[-only]]`

``-output-msg[-only] [-output-msg switches]``
   If specified, :toolname:`CodePeer` will output its results, in various
   formats.

   If ``-output-msg`` is given, :toolname:`CodePeer` will perform a new
   analysis, and output its results.

   If ``-output-msg-only`` is specified, no new
   analysis is performed, and the results from the previous run
   (of the same level) will be emitted.

   You can control this output by adding switches.

   e.g. ``-output-msg -csv -out report.csv`` to generate a CSV file

``-html, -html-only``
   Generate HTML output. If ``-html-only``, do not run any analysis.

---------------------------------
Running CodePeer in GNAT Studio
---------------------------------

.. image:: codepeer_from_gs.jpg

---------------------
Project File Set Up
---------------------

Let's explore sections 1.4, 1.5 and 1.6 of the User's Guide

+ `Link: Basic Project File Setup<http://docs.adacore.com/codepeer-docs/users_guide/_build/html/introduction.html#basic-project-file-setup>`_
+ `Link: Project File Setup<http://docs.adacore.com/codepeer-docs/users_guide/_build/html/introduction.html#project-file-setup>`_
+ `Link: Advanced Project File Setup<http://docs.adacore.com/codepeer-docs/users_guide/_build/html/introduction.html#advanced-project-file-setup>`_

-------------------
CodePeer Tutorial
-------------------

+ Get a fresh copy of the :toolname:`GNAT Studio` tutorial directory

  + From :filename:`GNATPRO/xxx/share/examples/gnatstudio/tutorial`
  + Check it include the :filename:`sdc` project
  + Copy it as :filename:`sources/codepeer/tutorial/`

+ Open this :filename:`sdc` project copy with :toolname:`GNAT Studio`
+ Open the :toolname:`CodePeer` Tutorial from :toolname:`GNAT Studio`

  + :menu:`Help` :math:`\rightarrow` :menu:`CodePeer` :math:`\rightarrow` :menu:`CodePeer Tutorial`

+ Walk through the steps of the :toolname:`CodePeer` tutorial

---------------------------------------
CodePeer Levels Depth and Constraints
---------------------------------------

+ The **higher** the level the **deeper** and **costlier** the analysis

.. container:: latex_environment

   .. list-table::
      :header-rows: 1

      * - *Level*

        - *Description*
        - *Code size*
        - *False positives*

      * - *0*

        - Infer only (default)
        - No limits
        - Lowest

      * - *1*

        - Subprograms
        - No limits
        - Few

      * - *2*

        - Groups of units
        - No limits
        - Some

      * - *3*

        - Semi-global
        - < 1 million SLOC
        - High

      * -

        - Automatic partitioning
        - CC < 40
        -

      * - *4*

        - Global and **exhaustive**
        - < 200 KSLOC
        - Highest

      * -

        - Flag all issues
        - CC < 20
        -

+ *SLOC* : Source lines of code
+ *CC* : Cyclomatic Complexity

--------------------------
CodePeer Levels Use Case
--------------------------

+ The levels adapt to various **workflows** and **users**
+ The **lower** the level the **more frequently** it should be run

.. container:: latex_environment

   .. list-table::
      :header-rows: 1

      * - *Level*

        - *Condition*
        - *Workflow Step*
        - *Goal*

      * - *0*

        - None
        - Initial static analysis
        - Quick feedback

      * - *1*

        - Project set-up
        - After each commit
        - Sanity check

      * - *2*

        - Level 1 results clean
        - Integration, CI
        - Regular check

      * - *3*

        - Medium code base
        - Integration, Nightly
        - Manual review

      * -

        - Server run
        -
        - Baseline

      * - *4*

        - Small code base
        - Before production
        - Exhaustive review

      * -

        - Server run
        -
        -

--------------------------
"No False Positive" Mode
--------------------------

+ :command:`-level 0` or :command:`-messages min`
+ Suppresses messages most **likely** to be false positives
+ Allows programmers to **focus** initial work on likely problems
+ Can be combined with **any level** of analysis
+ :command:`-messages min` is default for levels 0, 1, and 2

----------------------------
Running CodePeer regularly
----------------------------

+ Historical database (SQLite) stores all results **per level**

  + Can be stored in Configuration Management

+ **Baseline run**

  + **Previous** run each new run is compared to
  + Differences of **messages** in :toolname:`CodePeer` report
  + Default: first run
  + :command:`-baseline` to change it

+ Typical use

  + **Nightly** :command:`-baseline` run on servers
  + **Daily** development compares to baseline

+ :command:`-cutoff` overrides it for a **single** run
+ Compare between two arbitrary runs with :command:`-cutoff` and :command:`-current`

---------------------
Messages Categories
---------------------

+ **Run-Time Checks**

  + Errors that will raise built-in exceptions at runtime
  + Or fail silently with :command:`-gnatp`

+ **User Checks**

  + Errors that will raise user exceptions at runtime
  + Or fail silently with :command:`-gnatp`

+ **Validity Checks**

  + Mishandled object scope and value

+ **Warnings**

  + Questionable code that seems to have logic flaws
  + Hints at logical errors

+ **Race Conditions**

  + Code unsafe due to multi-tasking

=================
Run-Time Checks
=================

-------------------------
Run-Time Check Messages
-------------------------

.. container:: latex_environment

   .. list-table::
        :header-rows: 1

        * - *Message*

          - *Definition*

        * - ``divide by zero``

          - The second operand could be zero

        * -

          - On a division, :ada:`mod` or :ada:`rem` operation

        * - ``range check``

          - A discrete could reach a value out of its :ada:`range`

        * - ``overflow check``

          - An operation could overflow its numeric type

        * -

          - NB: Depends on the `'Base` representation

        * - ``array index check``

          - Array index could be out of bounds

        * - ``access check``

          - A :ada:`null` access could be dereferenced

        * - ``aliasing check``

          - A subprogram call could cause an aliasing error

        * -

          - eg. passing a single reference as two parameters

        * - ``tag check``

          - A dynamic :ada:`'Class` or :ada:`'Tag` check could fail

        * - ``validity``

          - An uninitialized or invalid object could be read

        * - ``discriminant check``

          - The wrong variant could be used

        * -

          - eg. copy with the wrong discriminant

        * - ``precondition``

          - A subprogram call could violate its calculated precondition

-------------------
Array Index Check
-------------------

+ Index value could be outside the array bounds
+ Also known as **buffer overflow**.
+ Will generate a :ada:`Constraint_Error`

..
   codepeer example (4.1.1 - array index check)

.. code:: Ada
   :number-lines: 1

   procedure Buffer_Overflow is
      type Int_Array is array (0 .. 2) of Integer;
      X, Y : Int_Array;
   begin
      for I in X'Range loop
         X (I) := I + 1;
      end loop;

      for I in X'Range loop
         Y (X (I)) := I;  -- Bad when I = 2, since X (I) = 3
      end loop;
   end Buffer_Overflow;

| ``high: array index check fails here: requires (X (I)) in 0..2``

-----------------
Divide By Zero
-----------------

+ The second operand of a divide, :ada:`mod` or :ada:`rem` operation could be zero
+ Will generate a :ada:`Program_Error`

..
   codepeer example (4.1.1 - divide by zero)

.. code:: Ada
   :number-lines: 1

   procedure Div is
      type Int is range 0 .. 2**32 - 1;
      A : Int := Int'Last;
      X : Integer;
   begin
      for I in Int range 0 .. 2 loop
         X := Integer (A / I); -- division by zero when I=0
      end loop;
   end Div;

| ``high: divide by zero fails here: requires I /= 0``

--------------
Access Check
--------------

+ Attempting to dereference a reference that could be :ada:`null`
+ Will generate an :ada:`Access_Error`

..
   codepeer example (4.1.1 - access check)

.. code:: Ada
   :number-lines: 1

   procedure Null_Deref is
      type Int_Access is access Integer;
      X : Int_Access;
   begin
      if X = null then
         X.all := 1;  -- null dereference
      end if;
   end Null_Deref;

| ``high: access check fails here``

-------------
Range Check
-------------

+ Calculation may generate a value outside the :ada:`range` of an Ada type or subtype
+ Will generate a :ada:`Constraint_Error`

..
   codepeer example (4.1.1 - range check)

.. code:: Ada
   :number-lines: 1

   subtype Constrained_Integer is Integer range 1 .. 2;
   A : Integer;

   procedure Proc_1 (I : in Constrained_Integer) is
   begin
      A := I + 1;
   end Proc_1;
   ...
   A := 0;
   Proc_1 (I => A);  --  A is out-of-range of parameter I

| ``high: range check fails here: requires A in 1..2``

----------------
Overflow Check
----------------

+ Calculation may overflow the bounds of a numeric type.
+ Depends on the size of the underlying type (base type)
+ Will generate a :ada:`Constraint_Error`

..
   codepeer example (4.1.1 - overflow check)

.. code:: Ada
   :number-lines: 1

   is
      Attempt_Count : Integer := Integer'Last;
   begin
      -- Forgot to reset Attempt_Count to 0
      loop
         Put ("Enter password to delete system disk");
         if Get_Correct_Pw then
            Allow_Access;
         else
            Attempt_Count := Attempt_Count + 1;

| ``high: overflow check fails here: requires Attempt_Count /= Integer_32'Last``
| ``high: overflow check fails here: requires Attempt_Count in Integer_32'First-1..Integer_32'Last-1``

----------------
Aliasing Check
----------------

+ Some parameters could be passed as **reference**
+ For such parameters, **precondition** that they:

  + Are a **different object** from other parameters
  + Do **not match** the address of a global object

..
   codepeer example (4.1.1 - aliasing check)

.. code:: Ada
   :number-lines: 1

      procedure In_Out (A : Int_Array; B : out Int_Array) is
      begin
         B (1) := A (1) + 1;
         ...
         B (1) := A (1) + 2;
      end In_Out;
   ...
      In_Out (A, A); -- Aliasing!

| ``high: precondition (aliasing check) failure on call to alias.in_out: requires B /= A``

-----------
Tag Check
-----------

A tag check operation on a :ada:`tagged` object might fail

..
   codepeer example (4.1.1 - tag check)

.. code:: Ada
   :number-lines: 1

   is
      type T1 is tagged null record;
      type T2 is new T1 with null record;

      procedure Call (X1 : T1'Class) is
      begin
         An_Operation (T2'Class (X1));
      end Call;

      X1 : T1;
      X2 : T2;
   begin
      Call (X1); -- not OK, Call requires T2'Class

| ``high: precondition (tag check) failure on call to tag.call: requires X1'Tag in {tag.pkg.t2, tag.t3}``

--------------------
Discriminant Check
--------------------

A field for the wrong variant/discriminant is accessed

..
   codepeer example (4.1.1 - discriminant check)

.. code:: Ada
   :number-lines: 1

   type T (B : Boolean := True) is record
      case B is
         when True =>
            J : Integer;
         when False =>
            F : Float;
      end case;
   end record;

   X : T (B => True);

   function Create (F : Float) return T is
     (False, F);
   ...
   X := Create (6.0);  -- discriminant check failure

| ``high: discriminant check fails here: requires (Create (6.0).b = True)``

--------------
Precondition
--------------

+ Subprogram call could violate preconditions, either

  + Where the error may occur
  + Where a caller passes in a value causing the error

+ Need to check generated preconditions
+ :toolname:`GNAT Studio` or :command:`-show-backtraces` to analyze checks

..
   codepeer example (4.1.1 - precondition)

.. code:: Ada
   :number-lines: 1

   function Call (X : Integer) return Integer is
   begin
      if X < 0 then
         return -1;
      end if;
   end Call;
   ...
   for I in -5 .. 5 loop
      X := X + Call (I);
   end loop;

| ``high: precondition (conditional check) failure on call to precondition.call: requires X < 0``

=============
User Checks
=============

---------------------
User Check Messages
---------------------

.. container:: latex_environment

   .. list-table::
        :header-rows: 1

        * - *Message*

          - *Description*

        * - ``assertion``

          - A user assertion could fail

        * -

          - eg. :ada:`pragma Assert`

        * - ``conditional check``

          - An :ada:`exception` could be raised conditionally

        * - ``raise exception``

          - An :ada:`exception` is raised on a reachable path

        * - 

          - Same as *conditional check*, but unconditionally

        * - ``user precondition``

          - Potential violation of a specified precondition

        * -

          - As a :ada:`Pre` aspect or as a :ada:`pragma Precondition`

        * - ``postcondition``

          - Potential violation of a specified postcondition

        * -

          - As a :ada:`Post` aspect or as a :ada:`pragma Postcondition`

-----------
Assertion
-----------

A user assertion (using e.g. :ada:`pragma Assert`) could fail

..
   codepeer example (4.1.2 - assertion)

.. code:: Ada
   :number-lines: 1

   procedure Assert is

      function And_Or (A, B : Boolean) return Boolean is
      begin
         return False;
      end And_Or;

   begin
      pragma Assert (And_Or (True, True));
   end Assert;

| ``high: assertion fails here: requires (and_or'Result) /= false``

-------------------
Conditional Check
-------------------

An exception could be raised **conditionally** in user code

..
   codepeer example (4.1.2 - conditional check)

.. code:: Ada
   :number-lines: 1

   if Wrong_Password then
      Attempt_Count := Attempt_Count + 1;

      if Attempt_Count > 3 then
         Put_Line ("max password count reached");
         raise Program_Error;
      end if;
   end if;

| ``high: conditional check raises exception here: requires Attempt_Count <= 3``

-----------------
Raise Exception
-----------------

An exception is raised **unconditionally** on a **reachable** path.

..
   codepeer example (4.1.2 - raise exception)

.. code:: Ada
   :number-lines: 1

   procedure Raise_Exc is
      X : Integer := raise Program_Error;
   begin
      null;
   end Raise_Exc;

| ``low: raise exception unconditional raise``

-------------------
User Precondition
-------------------

A call might violate a subprogram's specified precondition.

..
   codepeer example (4.1.2 - user precondition)

.. code:: Ada
   :number-lines: 1

   procedure Pre is
      function "**" (Left, Right : Float) return Float with
         Import,
         Pre => Left /= 0.0;

      A : Float := 1.0;
   begin
      A := (A - 1.0)**2.0;
   end Pre;

| ``high: precondition (user precondition) failure on call to pre."**": requires Left /= 0.0``

---------------
Postcondition
---------------

The subprogram's body may violate its specified postcondition. 

..
   codepeer example (4.1.2 - postcondition)

.. code:: Ada
   :number-lines: 1

   type Stress_Level is (None, Under_Stress, Destructive);

   function Reduce (Stress : Stress_Level)
     return Stress_Level with
      Pre  => (Stress /= None),
      Post => (Stress /= Destructive)
      is (Stress_Level'Val (Stress_Level'Pos (Stress) + 1));
      --                                              ^
      --                                             Typo!
   ...
   Reduce (My_Component_Stress);

| ``high: postcondition failure on call to post.reduce: requires Stress /= Destructive``

=====================================
Uninitialized and Invalid Variables
=====================================

----------------------------------------------
Uninitialized and Invalid Variables Messages
----------------------------------------------

.. container:: latex_environment

   .. list-table::
        :header-rows: 1

        * - *Message*

          - *Description*

        * - ``validity check``

          - An uninitialized or invalid value could be read

----------------
Validity Check
----------------

The code may be reading an uninitialized or invalid value

..
   codepeer example (4.1.3 - validity check)

.. code:: Ada
   :number-lines: 1

   procedure Uninit is
      A : Integer;
      B : Integer;
   begin
      A := B;  --  we are reading B which is uninitialized!
   end Uninit;

| ``high: validity check: B is uninitialized here``

==========
Warnings
==========

------------------------
Warning Messages (1/2)
------------------------

.. container:: latex_environment

   .. list-table::
        :header-rows: 1

        * - *Message*

          - *Description*

        * - ``dead code``

          - Also called *unreachable code*. 

        * -

          - Assumed all code should be reachable

        * - ``test always false``

          - Code always evaluating to :ada:`False`

        * - ``test always true``

          - Code always evaluating to :ada:`True`

        * - ``test predetermined``

          - Choice evaluating to a constant value

        * -

          - For eg. :ada:`case` statements

        * - ``condition predetermined``

          - Constant RHS or LHS in a conditional 

        * - ``loop does not complete normally``

          - Loop :ada:`exit` condition is always :ada:`False`

        * - ``unused assignment``

          - Redundant assignment

        * - ``unused assignment to global``

          - Redundant global object assignment

        * - ``unused out parameter``

          - Actual parameter of a call is ignored 

        * - 

          - Either never used or overwritten

+ **RHS** : Right-Hand-Side of a binary operation
+ **LHS** : Left-Hand-Side of a binary operation

------------------------
Warning Messages (2/2)
------------------------

.. container:: latex_environment

   .. list-table::
        :header-rows: 1

        * - *Message*

          - *Description*

        * - ``useless reassignment``

          - Assignment does not modify the object

        * - ``suspicious precondition``

          - Precondition seems to have a logic flaw

        * -

          - eg. possible set of values is not contiguous

        * - ``suspicious input``

          - :ada:`out` parameter read before assignment

        * -

          - should be :ada:`in out`

        * - ``unread parameter``

          - :ada:`in out` parameter is never read

        * -

          - should be :ada:`out`

        * - ``unassigned parameter``

          - :ada:`in out` parameter is never assigned

        * -

          - should be :ada:`in`

        * - ``suspicious constant operation``

          - Constant result from variable operands

        * -

          - May hint at a typo, or missing operation

        * - ``subp never returns``

          - Subprogram will never terminate

        * - ``subp always fails``

          - Subprogram will always terminate in error

-----------
Dead Code
-----------
+ Also called **unreachable code**.
+ All code is expected to be reachable

..
   codepeer example (4.1.4 - dead code)

.. code:: Ada
   :number-lines: 1

   procedure Dead_Code (X : out Integer) is
      I : Integer := 10;
   begin
      if I < 4 then
         X := 0;
      elsif I >= 8 then
         X := 0;
      end if;
   end Dead_Code;

| ``medium warning: dead code because I = 10``

-------------------
Test Always False
-------------------

Redundant conditionals, always :ada:`False`

..
   codepeer example (4.1.4 - test always false)

.. code:: Ada
   :number-lines: 1

   procedure Dead_Code (X : out Integer) is
      I : Integer := 10;
   begin
      if I < 4 then
         X := 0;
      end if;
   end Dead_Code;

| ``low warning: test always false because I = 10``

------------------
Test Always True
------------------

Redundant conditionals, always :ada:`True`

..
   codepeer example (4.1.4 - test always true)

.. code:: Ada
   :number-lines: 1

   procedure Dead_Code (X : out Integer) is
      I : Integer := 10;
   begin
      if I >= 8 then
         X := 0;
      end if;
   end Dead_Code;

| ``medium warning: test always true because I = 10``

--------------------
Test Predetermined
--------------------

+ Similar to ``test always true`` and ``test always false``

  + When choice is not binary
  + eg. :ada:`case` statement

..
   codepeer example (4.1.4 - test predetermined)

.. code:: Ada
   :number-lines: 1

   procedure Predetermined is
      I : Integer := 0;
   begin
      case I is
         when 0 =>
            null;
         when 1 =>
            null;
         when others =>
            null;
      end case;
   end Predetermined;

| ``low warning: test predetermined because I = 0``

-------------------------
Condition Predetermined
-------------------------

+ Redundant condition inside a conditional
+ One operand of a boolean operation is always :ada:`True` or :ada:`False`

..
   codepeer example (4.1.4 - condition predetermined)

.. code:: Ada
   :number-lines: 1

      if V /= A or else V /= B then
         raise Program_Error;
      end if;

| ``medium warning: condition predetermined because (V /= B) is always true``

---------------------------------
Loop Does Not Complete Normally
---------------------------------

+ Indicates loops that either

  + runs forever
  + fails to terminate normally

..
   codepeer example (4.1.4 - loop does not complete normally)

.. code:: Ada
   :number-lines: 1

   procedure Loops is
      Buf : String := "The" & ASCII.NUL;
      Bp  : Natural;
   begin
      Buf (4) := 'a';   -- Eliminates null terminator
      Bp      := Buf'First;

      loop
         Bp := Bp + 1;
         exit when Buf (Bp - 1) = ASCII.NUL; -- Condition never reached
      end loop;
   end Loops;

| ``medium warning: loop does not complete normally``

-------------------
Unused Assignment
-------------------

+ Object assigned more than once between reads
+ Unintentional loss of result or unexpected control flow
+ The check ignores some names as temporary:

  + :ada:`ignore`, :ada:`unused`, :ada:`discard`, :ada:`dummy`, :ada:`tmp`, :ada:`temp`
  + Tuned via the :filename:`MessagePatterns.xml` file if needed.

+ :ada:`pragma Unreferenced` also ignored

..
   codepeer example (4.1.4 - unused assignment)

.. code:: Ada
   :number-lines: 1

   I := Integer'Value (Get_Line);
   I := Integer'Value (Get_Line);

| ``medium warning: unused assignment into I``

-----------------------------
Unused Assignment To Global
-----------------------------

+ Global variable assigned more than once between reads
+ Note: the redundant assignment may occur deep in the **call tree**

..
   codepeer example (4.1.4 - unused assignment to global)

.. code:: Ada
   :number-lines: 1

   procedure Proc1 is
   begin
      G := 123;
   end Proc1;

   procedure Proc is
   begin
      Proc1;
      G := 456;  -- override effect of calling Proc1
   end Proc;

| ``low warning: unused assignment to global G in unused_global.p.proc1``

----------------------
Unused Out Parameter
----------------------

+ Actual :ada:`out` parameter of a call is ignored

  + either never used
  + or overwritten

..
   codepeer example (4.1.4 - unused out parameter)

.. code:: Ada
   :number-lines: 1

   procedure Search (Success : out Boolean);
   ...
   procedure Search is
      Ret_Val : Boolean;
   begin
      Search (Ret_Val);
   end Search;

| ``medium warning: unused out parameter Ret_Val``

----------------------
Useless Reassignment
----------------------

+ Assignments do not modify the value stored in the assigned object

..
   codepeer example (4.1.4 - useless reassignment)

.. code:: Ada
   :number-lines: 1

   procedure Self_Assign (A : in out Integer) is
      B : Integer;
   begin
      B := A;
      A := B;
   end Self_Assign;

| ``medium warning: useless reassignment of A``

-------------------------
Suspicious Precondition
-------------------------

+ Set of allowed inputs is **not contiguous**

  + some values **inbetween** allowed inputs can cause **runtime errors**

+ Certain cases may be missing from the user's precondition
+ May be a **false-positive** depending on the algorithm

..
   codepeer example (4.1.4 - suspicious precondition)

.. code:: Ada
   :number-lines: 1

   if S.Last = S.Arr'Last then
      raise Overflow;
   end if;
   --  Typo: Should be S.Last + 1
   S.Last         := S.Last - 1; 
   --  Error when S.Last = S.Arr'First - 1
   S.Arr (S.Last) := V;

| ``medium warning: suspicious precondition for S.Last: not a contiguous range of values``

------------------
Suspicious Input
------------------

+ :ada:`out` parameter read before assignment
+ Should have been an :ada:`in out`
+ Ada standard allows it

  + but it is a bug most of the time

..
   codepeer example (4.1.4 - suspicious input)

.. code:: Ada
   :number-lines: 1

   procedure Take_In_Out (R : in out T);
   ...
   procedure Take_Out (R : out T; B : Boolean) is
   begin
      Take_In_Out (R);  -- R is 'out' but used as 'in out'
   end Take_Out;

| ``medium warning: suspicious input R.I: depends on input value of out-parameter``

------------------
Unread Parameter
------------------

+ :ada:`in out` parameter is not read

  + but is assigned on **all** paths
  + Could be declared :ada:`out`

..
   codepeer example (4.1.4 - unread parameter)

.. code:: Ada
   :number-lines: 1

   procedure Unread (X : in out Integer) is
   begin
      X := 0;  -- X is assigned but never read
   end Unread;

| ``medium warning: unread parameter X: could have mode out``

----------------------
Unassigned Parameter
----------------------

+ :ada:`in out` parameter is never assigned

  + Could be declared :ada:`in`

..
   codepeer example (4.1.4 - unassigned parameter)

.. code:: Ada
   :number-lines: 1

   procedure Unassigned
     (X : in out Integer; Y : out Integer) is
   begin
      Y := X;  -- X is read but never assigned
   end Unassigned;

| ``medium warning: unassigned parameter X: could have mode in``

-------------------------------
Suspicious Constant Operation
-------------------------------

+ Constant value calculated from **non-constant operands**
+ Hint that there is a **coding mistake**

  + either a **typo**, using the **wrong variable**
  + or an operation that is **missing**

    + eg :ada:`Float` conversion before division

..
   codepeer example (4.1.4 - suspicious constant operation)

.. code:: Ada
   :number-lines: 1

   type T is new Natural range 0 .. 14;

   function Incorrect (X : T) return T is
   begin
      return X / (T'Last + 1);
   end Incorrect;

| ``medium warning: suspicious constant operation X/15 always evaluates to 0``

--------------------
Subp Never Returns
--------------------

+ Subprogram will **never** return

  + presumably **infinite loop**

+ Typically, **another message** in the body can explain why

  + eg. ``test always false``

..
   codepeer example (4.1.4 - subp never returns)

.. code:: Ada
   :number-lines: 1

   procedure Infinite_Loop is
      X : Integer := 33;
   begin
      loop
         X := X + 1;
      end loop;
   end Infinite_Loop;

| ``medium warning: subp never returns: infinite_loop``

-------------------
Subp Always Fails
-------------------

+ A run-time problem could occur on **every** execution
+ Typically, **another message** in the body can explain why

..
   codepeer example (4.1.4 - subp always fails)

.. code:: Ada
   :number-lines: 1

   procedure P is
      X : Integer := raise Program_Error;
   begin
      null;
   end P;

| ``high warning: subp always fails: p fails for all possible inputs``

=================
Race Conditions
=================

-------------------------
Race Condition Messages
-------------------------

.. container:: latex_environment

   .. list-table::
        :header-rows: 1

        * - *Message*

          - *Description*

        * - ``unprotected access``

          - Shared object access without lock

        * - ``unprotected shared access``

          - Object is referenced is multiple tasks

        * -

          - And accessed without a lock

        * - ``mismatch protected access``

          - Mismatch in locks used

        * -

          - Checked for all shared objects access

        * -

          - eg. task1 uses lock1, task2 uses lock2

-------------------------
Race Condition Examples
-------------------------

..
   codepeer example (4.1.5 - race conditions)

.. code:: Ada
   :number-lines: 1

   procedure Increment is
   begin
      Mutex_Acquire;
      if Counter = Natural'Last then
         Counter := Natural'First;
      else
         Counter := Counter + 1;
      end if;
      Mutex_Release;
   end Increment;

   procedure Reset is
   begin
      Counter := 0; -- lock missing
   end Decrement;

| ``medium warning: mismatched protected access of shared object Counter via race.increment``
| ``medium warning: unprotected access of Counter via race.reset``

=====================================
Automatically Generated Annotations
=====================================


-----------------------
Generated Annotations
-----------------------

+ :toolname:`CodePeer` generates **annotations** on the code
+ Not errors
+ Express **properties** and **assumptions** on the code
+ Can be reviewed

    + But not necessarily
    + Can help spot **inconsistencies**

+ Can help understand and **debug** messages

------------------------
Annotations Categories
------------------------

.. container:: latex_environment

   .. list-table::
        :header-rows: 1

        * - *Annotation*

          - *Description*

        * - ``precondition``

          - Requirements imposed the subprogram's inputs

        * - ``presumption``

          - Display what :toolname:`CodePeer` presumes about the results of an external subprogram whose code is unavailable, or are in a separate partition. There are separate presumptions for each call site, with a string in the form ``@<line-number-of-the-call>`` appended to the name of the subprogram. Presumptions are not generally used to determine preconditions of the calling routine, but they might influence postconditions of the calling routine.

        * - ``postcondition``

          - Characterize the behavior of the subprogram in terms of its outputs and the presumptions made.

        * - ``unanalyzed call``

          - Display the external calls to subprograms that the :toolname:`CodePeer` has not analyzed, and so participate in the determination of presumptions. Note that these annotations include all directly unanalyzed calls as well as the unanalyzed calls in the call graph subtree that have an influence on the current subprograms.

        * - ``global inputs``

          - List of all global variables referenced by each subprogram. Note that this only includes enclosing objects and not e.g. specific components. In the case of pointers, only the pointer is listed. Dereference to pointers may be implied by the pointer listed.

        * - ``global outputs``

          - List of all global variables (objects and components) modified by each subprogram

        * - ``new objects``

          - list of heap-allocated objects, created by a subprogram, that are not reclaimed during the execution of the subprogram itself; these are new objects that are accessible after return from the subprogram

--------------
Precondition
--------------

Preconditions specify requirements that the subprogram imposes on its inputs. For example, a subprogram might require a certain parameter to be non-null for proper operation of the subprogram. These preconditions are checked at every call site. A message is given for any precondition that a caller might violate. Precondition messages include in parenthesis a list of the checks involved in the requirements.

.. code:: ada
    :number-lines: 1

    TBD

-------------
Presumption
-------------

Display what :toolname:`CodePeer` presumes about the results of an external subprogram whose code is unavailable, or are in a separate partition. There are separate presumptions for each call site, with a string in the form ``@<line-number-of-the-call>`` appended to the name of the subprogram. Presumptions are not generally used to determine preconditions of the calling routine, but they might influence postconditions of the calling routine.

---------------
Postcondition
---------------

Characterize the behavior of the subprogram in terms of its outputs and the presumptions made.

.. code:: ada
    :number-lines: 1

    TBD

-----------------
Unanalyzed Call
-----------------

Display the external calls to subprograms that the :toolname:`CodePeer` has not analyzed, and so participate in the determination of presumptions. Note that these annotations include all directly unanalyzed calls as well as the unanalyzed calls in the call graph subtree that have an influence on the current subprograms.

.. code:: ada
    :number-lines: 1

    TBD

-----------------------
Global Inputs/Outputs
-----------------------

List of all global variables referenced by each subprogram. Note that this only includes enclosing objects and not e.g. specific components. In the case of pointers, only the pointer is listed. Dereference to pointers may be implied by the pointer listed.

.. code:: ada
    :number-lines: 1

    TBD

-------------
New Objects
-------------

list of heap-allocated objects, created by a subprogram, that are not reclaimed during the execution of the subprogram itself; these are new objects that are accessible after return from the subprogram

.. code:: ada
    :number-lines: 1

    TBD

============================
External Tools Integration
============================

---------------
GNAT Warnings
---------------

+ GNAT warnings can be generated by :toolname:`CodePeer`

  :command:`--gnat-warnings= xxx` *(uses -gnatwxxx)*

+ Messages are stored in the database, displayed and filtered as any other message
+ Manual justification can be stored in the database
+ Manual justification in the source is achieved via pragma Warnings instead of :ada:`pragma Annotate`

------------------------
GNATcheck messages
------------------------

+ :toolname:`GNATcheck` messages can be generated by :toolname:`CodePeer`

  :command:`--gnatcheck`

+ Uses the :toolname:`GNATcheck` rules file as defined in your project file in package :ada:`Check`
+ Messages are stored in the database, displayed and filtered as any other message
+ Manual justification can be stored in the database
+ Manual justification in the source is achieved via :ada:`pragma Annotate (GNATcheck, ...)`

============================
Finding the Right Settings
============================

---------------------
System Requirements
---------------------

+ Fast 64bits machine with multiple cores and memory
+ **Server** :math:`\rightarrow` 24 to 48 cores with at least 2GB per core (48 to 96GB)
+ **Local desktop** :math:`\rightarrow` 4 to 8 cores, with at least 8 to 16GB
+ **Avoid slow filesystems** :math:`\rightarrow` networks drives (NFS, SMB), configuration management filesystems (e.g. ClearCase dynamic views).

  + If not possible, at least generate output file in a local disk via the *Output_Directory* and *Database_Directory* project attributes.

+ **Global analysis (-level max)** :math:`\rightarrow` At least 12GB + 1GB per 10K SLOC, e.g. At least 32GB for 200K SLOC.

------------------------
Analyze Messages (1/4)
------------------------

+ Start with default (level 0)
+ If the run is mostly clean/contains mostly interesting messages, run at next level (e.g. level 1) and iterate until number of false positive/timing is too high for your needs

.. code:: Ada

   project My_Project is
      for Source_Dirs use ...
      package CodePeer is
         for Switches use ( "-level", "1" );
      end CodePeer;
   end My_Project;

:command:`codepeer -Pmy_project -level 1 ...`

------------------------
Analyze Messages (2/4)
------------------------

+ If a run contains many messages, analyze some and identify groups of uninteresting messages
+ Exclude categories of uninteresting messages via e.g. :command:`--be-messages` (starting with level 1).

------------------------
Analyze Messages (3/4)
------------------------

+ Filtering of messages

  + :command:`-output-msg` :command:`-hide-low` on the command line
  + Check boxes to filter on message category / rank in :toolname:`GNAT Studio` and HTML
  + :command:`--be-messages` :command:`--gnat-warnings` :command:`--lal-checkers` switches
  + :command:`-messages min/normal/max`
  + Pattern-based automatic filtering (:filename:`MessagePatterns.xml`)

+ For example, to disable messages related to access check:

   :command:`--be-messages=-access_check`

+ If many uninteresting messages in the same file, you can exclude this file from analysis (see next slides)

------------------------
Analyze Messages (4/4)
------------------------

+ Choose relevant messages based on ranking

  + Rank = severity + certainty
  + **High** :math:`\rightarrow` certain problem
  + **Medium** :math:`\rightarrow` possible problem, or certain with low severity
  + **Low** :math:`\rightarrow` less likely problem (yet useful for exhaustivity)

+ When analysing existing code, start looking at *High* messages first, then *Medium*, and finally if it makes sense, *Low* messages.
+ A recommended setting is to consider High and Medium messages (default in :toolname:`GNAT Studio` and HTML interfaces).

---------------------
Run CodePeer faster
---------------------

+ Use a 64-bit machine with a lot of memory and cores
+ Lower analysis level (:command:`-level <num>`), use :command:`-j0` (default)
+ Identify files taking too long to analyze and disable analysis of selected subprograms or files

| ``analyzed main.scil in 0.05 seconds``
| ``analyzed main__body.scil in 620.31 seconds``
| ``analyzed pack1__body.scil in 20.02 seconds``
| ``analyzed pack2__body.scil in 5.13 seconds``

-----------------------------
Code-Based Partial Analysis
-----------------------------

+ Excluding Subprograms or Packages From Analysis

.. code:: Ada

   procedure Complex_Subprogram (...) is
      pragma Annotate (CodePeer, Skip_Analysis);
   begin
      ...
   end Complex_Subprogram;

   package Complex_Package is
      pragma Annotate (CodePeer, Skip_Analysis);
      ...
   end Complex_Package;

--------------------------------
Project-Based Partial Analysis
--------------------------------

+ Excluding Files From Analysis

   .. code:: Ada

      package CodePeer is
         for Excluded_Source_Files use ( "xxx.adb" );
         -- Analysis generates lots of timeouts, skip for now
      end CodePeer;

+ Excluding Directories From Analysis

   .. code:: Ada

      package CodePeer is
         for Excluded_Source_Dirs use ("directory1",
                                       "directory2");
      end CodePeer;

+ Excluding Projects From Analysis

   .. code:: Ada

      for Externally_Built use "True";

====================
CodePeer Workflows
====================

--------------------
CodePeer Use Cases
--------------------

+ Analyzing code locally prior to commit
+ Nightly runs on a server
+ Continuous runs on a server after each change
+ Combined desktop/nightly run
+ Combined continuous/nightly run
+ Combined desktop/continuous/nightly run
+ Software customization per project
+ Compare local changes with master
+ Multiple teams analyzing multiple subsystems
+ Use :toolname:`CodePeer` to generate a security report

----------------------------------------------
Analyzing Code Locally Prior To Commit (1/2)
----------------------------------------------

Fast analysis done at each developer's desk

+ Solution #1

  + Use :toolname:`GNAT Studio` menu :menu:`CodePeer` :math:`\rightarrow` :menu:`Analyze File` (or :menu:`Analyze File by File`) after each compilation, before testing.
  + Incremental, fast analysis

+ Solution #2

  + run :toolname:`CodePeer` with :command:`-level 1/2 -baseline`
  + Local :toolname:`CodePeer` database used for comparison
  + Look at Added messages only

----------------------------------------------
Analyzing Code Locally Prior To Commit (2/2)
----------------------------------------------

+ For each new message:

   Fix the code
      if a real issue is found

   Justify false positives
      via :ada:`pragma Annotate`

   Refine the settings
      e.g. to exclude some message kinds or subprograms/files from analysis

--------------------------
Nightly Runs On A Server
--------------------------

+ :toolname:`CodePeer` run daily on a dedicated server (highest suitable level) allowing users to justify messages manually via :toolname:`CodePeer` web server.
+ Messages already justified through :ada:`pragma Annotate` do not need to be justified again.
+ These runs will typically be run nightly to take into account commits of the day, and *provide results to users the next morning*
+ Developers can analyze the results via the web interface or from :toolname:`GNAT Studio` by accessing the database remotely.
+ Developers then *fix the code*, or *justify the relevant messages* using either :ada:`pragma Annotate` or via :toolname:`GNAT Studio` or the web interface.
+ *Optionally* for each release, results are committed under CM for traceability purposes.

-----------------------------------------------
Continuous Runs On A Server After Each Change
-----------------------------------------------

+ :toolname:`CodePeer` is run on a dedicated server with lots of resources at a level suitable for performing runs rapidly (e.g. level 0 or 1)
+ These runs do not need to be exhaustive: *focus is on differences from previous run*
+ Continuous runs *trigger on new repository changes* (e.g. via Jenkins)
+ A *summary is sent to developers* via email or a web interface:

.. container:: latex_environment tiny

    :command:`codepeer -Pprj -output-msg -only -show-added | grep "[added]"`

+ Developers then *fix the code*, or *justify the relevant messages*

  + via :ada:`pragma Annotate` in source code or via web interface.
  + or wait for the next nightly run to post a manual analysis via the HTML Output.

------------------------------
Combined Desktop/Nightly Run
------------------------------

+ *Fast analysis* of code changes done at each *developer's desk*
+ A longer and *more complete analysis* is performed nightly on a *powerful server*
+ Combination of *Analyzing code locally prior to commit* and *Nightly runs on a server*

---------------------------------
Combined Continuous/Nightly Run
---------------------------------

+ *Fast analysis* of code changes done after each commit *on a server*
+ A longer and more *complete analysis* is performed nightly on a *powerful server*
+ Or alternatively: a baseline run is performed nightly at same level as continuous runs (:command:`-baseline`).
+ Combination of *Analyzing code locally prior to commit* and *Continuous runs on a server after each change*

-----------------------------------------
Combined Desktop/Continuous/Nightly Run
-----------------------------------------

+ *Fast analysis* of code changes done at each *developer's desk*
+ An *analysis* (fast but potentially longer than the one performed by developers) is done after each commit *on a server*
+ A *more exhaustive analysis* performed nightly on a *powerful server*
+ Combination of *Analyzing code locally prior to commit*, *Nightly runs on a server* and *Continuous runs on a server after each change* .

--------------------------------------------
Software Customization Per Project/Mission
--------------------------------------------

+ A *core version* of your software gets branched out or instantiated and *modified on a per-project/mission* basis.
+ **Continuous solution**

  + Share message justifications via :ada:`pragma Annotate`
  + Merge of justifications handled via standard CM
  + Separate :toolname:`CodePeer` runs on all active branches, database used to compare runs on a given branch

+ **One shot solution**

  + Copy the justifications from the DB at branch point
  + Maintain it separately from there (*fork*)
  + Separate :toolname:`CodePeer` runs on all active branches, database used to compare runs on a given branch

-----------------------------------------
Compare Local Changes With Master (1/3)
-----------------------------------------

+ Analysis running on server with latest source version
+ The ("gold") database gets updated when sources are updated

  + :command:`-baseline` switch

+ Developers pre-validate changes locally with :toolname:`CodePeer` prior to commit, in a separate sandbox and using the same analysis settings.
+ **Continuous integration** :math:`\rightarrow` local user creates a separate branch and commit his change on this branch

-----------------------------------------
Compare local changes with master (2/3)
-----------------------------------------

A continuous builder (e.g. Jenkins) is monitoring user branches and triggers an analysis that will:

  + Copy in a separate sandbox the database from the reference (nightly) run.
  + Perform a run with the same settings as the reference run
  + Send results to the user either via its web server and the :toolname:`CodePeer` HTML interface, or by generating a textual report (-output-msg).
  + Can be combined with -show-added so that the user can concentrate on the new messages found:

      .. container:: latex_environment tiny

         :command:`codepeer -Pprj -output-msg -show-added | grep "[added]"`

  + Throw out this separate sandbox

-----------------------------------------
Compare local changes with master (3/3)
-----------------------------------------

+ Once the user receives the report he can *address the findings* by

  + Modifying the code
  + Using :ada:`pragma Annotate`
  + Posting an analysis on the gold database after his change is merged on the master branch and a new baseline run is available for review.

+ Another, more *manual alternative* involves

  + Make a local copy of the gold database in the user space
  + Run :toolname:`CodePeer` there
  + Look at differences then throw out this local environment.

----------------------------------------------
Multiple teams analyzing multiple subsystems
----------------------------------------------

+ Large software system composed of *multiple subsystems* maintained by *different teams*
+ Perform a *separate analysis for each subsystem*, using a separate workspace and database
+ Create *one project file (.gpr) per subsystem*
+ To resolve dependencies between subsystems, use :ada:`limited with`

   .. code:: Ada

      limited with "subsystem1";
      limited with "subsystem2";
      project Subsystem3 is
         ...
      end Subsystem3;

+ Run :toolname:`CodePeer` with:

   :command:`codepeer -Psubsystem1 --no-subprojects`

==============================
Justifying CodePeer Messages
==============================

------------------------------------
Justifying CodePeer messages (1/2)
------------------------------------

+ Add review status in database

  + :toolname:`GNAT Studio`: select review icon on message(s)
  + HTML web server: click on :menu:`Add Review` button above messages
  + Displayed with :command:`-output-msg-only -show-reviews (-only)`

+ Add message review pragma in code

  + :ada:`pragma Annotate` added next to code with message
  + 2 modalities: *False_Positive* or *Intentional*
  + Also added in the database

.. code:: Ada

   ...
   return (X + Y) / (X - Y);
   pragma Annotate (CodePeer,
                    False_Positive,
                    "Divide By Zero",
                    "reviewed by John Smith");

----------------------------------------
Justifying CodePeer messages (2/2)
----------------------------------------

+ Use spreadsheet tool

  + Export messages in CSV format

     :command:`codepeer -Pprj -output-msg-only -csv`

  + Review them via the spreadsheet tool (e.g. Excel)
  + Import back reviews into the :toolname:`CodePeer` database

     :command:`codepeer_bridge --import-reviews`

+ Use external justification connected to output

  + Textual output: compiler-like messages or CSV format

========================
CodePeer Customization
========================

------------------------------------------
CodePeer Specific Project Attributes
------------------------------------------

.. code:: Ada

  project Prj1 is
     ...

     package CodePeer is
        for Excluded_Source_Files use ("file1.ads", "file2.adb");
        --  similar to project-level attribute for compilation

        for Output_Directory use "project1.output";

        for Database_Directory use "/work/project1.db";
        --  can be local or on shared drive

        for Switches use ("-level", "1");
        --  typically -level -jobs

        for Additional_Patterns use "ExtraMessagePatterns.xml";
        --  also Message_Patterns to replace default one

        for Include_CWE use "true";
     end CodePeer;
   end Prj1;

-----------------------------------------
Project Specialization For CodePeer
-----------------------------------------

.. code:: Ada

   type Build_Type is ("Debug", "Production", "CodePeer");
   Build : Build_Type := External ("Build", "Debug");

   package Builder is
      case Build is
         when "CodePeer" =>
            for Global_Compilation_Switches ("Ada") use
            ("-gnatI",
             -- ignore representation clauses confusing analysis
             "-gnateT=" & My_Project'Project_Dir & "/target.atp",
             -- specify target platform for integer sizes, alignment, ...
             "--RTS=kernel");
             -- specify runtime library

         when others =>
            for Global_Compilation_Switches ("Ada") use ("-O", "-g");
            -- switches only relevant when building
      end case;
   end Builder;

+ Compile with :command:`gprbuild -P my_project.gpr -XBuild=Production`
+ Analyze with :command:`codepeer -P my_project.gpr -XBuild=CodePeer`

--------------------------------
Custom API For Race Conditions
--------------------------------

+ :ada:`pragma Annotate` can identify entry points and locks other than Ada tasks and protected objects

.. code:: Ada

   package Pkg is
      procedure Single;
      pragma Annotate (CodePeer,
                       Single_Thread_Entry_Point,
                       "Pkg.Single");
      procedure Multiple;
      pragma Annotate (CodePeer,
                       Multiple_Thread_Entry_Point,
                       "Pkg.Multiple");
   end Pkg;

.. code:: Ada

   package Locking is
      procedure Lock;
      procedure Unlock;
      pragma Annotate (CodePeer, Mutex,
                       "Locking.Lock",
                       "Locking.Unlock");
   end Locking;

-------------
Report File
-------------

.. columns::

   .. column::

      + You can combine some or all of the following switches to generate a report file
      + Mandatory switches:

        + :command:`-output-msg`
        + :command:`-out <report file>`

      + Optional switches

        + :command:`-show-header`
        + :command:`-show-info`
        + :command:`-show-removed`
        + :command:`-show-reviews`
        + :command:`-show-added`

   .. column::

    .. container:: latex_environment tiny

      .. code:: Ada

         package CodePeer is
            for Switches use ("-level", "max", "-output-msg",
                              "-out", "report_file.out",
                              "-show-header", "-show-info");
         end CodePeer;

      |
      | ``date : YYYY-MM-DD HH:MM:SS``
      | ``codepeer version : 18.2 (yyyymmdd)``
      | ``host : Windows 64 bits``
      | ``command line : codepeer -P my_project.gpr``
      | ``codepeer switches : -level max -output-msg -out report_file.out -show-header -show-info``
      | ``current run number: 4``
      | ``base run number : 1``
      | ``excluded file : /path/to/unit3.adb``
      | ``unit1.ads:1:1: info: module analyzed: unit1``
      | ``unit1.adb:3:1: info: module analyzed: unit1__body``
      | ``unit2.adb:12:25: medium: divide by zero might fail: requires X /= 0``
      | ``[...]``

============================
CodePeer for Certification
============================

----------------------
CodePeer and CWE
----------------------

+ MITRE's Common Weakness Enumeration (CWE) is a set of common vulnerabilities in software applications
+ It is referenced in many government contracts and cyber-security requirements
+ :toolname:`CodePeer` is officially CWE-compatible

  https://cwe.mitre.org/compatible/questionnaires/43.html

+ Mapping is provided between :toolname:`CodePeer` findings and CWE identifiers

---------------------------
CodePeer and DO178B/C
---------------------------

+ :toolname:`CodePeer` supports DO-178B/C Avionics Standard
+ DO-178C Objective A-5.6 (activity 6.3.4.f):

  **Code Accuracy and Consistency** The objective is to determine the correctness and consistency of the Source Code, including stack usage, memory usage, *fixed point arithmetic overflow and resolution*, *floating-point arithmetic*, resource contention and limitations, worst-case execution timing, exception handling, *use of uninitialized variables*, cache management, *unused variables*, and *data corruption due to task or interrupt conflicts*. The compiler (including its options), the linker (including its options), and some hardware features may have an impact on the worst-case execution timing and this impact should be assessed.

+ :toolname:`CodePeer` helps *reduce* the scope of manual review
+ See Booklet: *AdaCore Technologies for DO-178C/ED-12C*

  + Authored by Frederic Pothon & Quentin Ochem

------------------------------------
CodePeer and CENELEC - EN50128
------------------------------------

+ :toolname:`CodePeer` Qualified as a T2 tool for this CENELEC Rail Standard
+ :toolname:`CodePeer` Supports:

  + D.4 Boundary Value Analysis
  + D.8 Control Flow Analysis
  + D.10 Data Flow Analysis
  + D.14 Defensive Programming
  + D.18 Equivalence Classes and Input Partition Testing
  + D.24 Failure Assertion Programming
  + D.32 Impact Analysis

+ :toolname:`CodePeer` is uniquely supportive of Walkthroughs and Design Reviews via its as-built documentation
+ See Booklet: *AdaCore Technologies for CENELEC EN 50128:2011*

  + Authored by Jean-Louis Boulanger & Quentin Ochem

=========================
How Does CodePeer Work?
=========================

-------------------------
How Does CodePeer Work?
-------------------------

+ :toolname:`CodePeer` computes the possible value of every variable and every expression at each program point.
+ It starts with leaf subprograms and propagates information up in the call-graph, iterating to handle recursion.
+ For each subprogram:

  + It computes a precondition that guards against check failures.
  + It issues check/warning messages for the subprogram.
  + It computes a postcondition ensured by the subprogram.
  + It uses the generated subprogram contract (precondition + postcondition) to analyze calls.

-----------------------------
How Does CodePeer Work?
-----------------------------

See *CodePeer By Example* for more details

   From :toolname:`GNAT Studio` go to :menu:`Help` :math:`\rightarrow` :menu:`Codepeer` :math:`\rightarrow` :menu:`Examples` :math:`\rightarrow` :menu:`Codepeer By Example`

-----------------------------------------
CodePeer Limitations and Heuristics
-----------------------------------------

+ Let's explore section 7.13 of the User's Guide
+ http://docs.adacore.com/codepeer-docs/users_guide/_build/html/appendix.html#codepeer-limitations-and-heuristics

-------------------------
CodePeer References
-------------------------

+ :toolname:`CodePeer` User's Guide and Tutorial

  + Online: https://www.adacore.com/documentation#CodePeer
  + In local install at share/doc/codepeer/users_guide (or tutorial)
  + From :toolname:`GNAT Studio` go to :menu:`Help` :math:`\rightarrow` :menu:`Codepeer` :math:`\rightarrow` :menu:`Codepeer User's Guide` (or :menu:`Codepeer Tutorial`)

+ :toolname:`CodePeer` website

  + http://www.adacore.com/codepeer
  + Videos, product pages, articles, challenges

+ Book chapter on :toolname:`CodePeer`

  + In Static Analysis of Software: The Abstract Interpretation, published by Wiley (2012)