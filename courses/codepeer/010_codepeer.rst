
**********
CodePeer
**********

==========================
Advanced Static Analysis
==========================

--------------------------
What is Static Analysis?
--------------------------

+ Symbolic interpretation of source code, to find what could go wrong (and right) without executing it
+ Formally verifying high level or abstract properties on your application, giving strong guarantees
+ May be exhaustive

  + All possible errors are reported
  + No false negatives; there may be false positives
  + If the analyzer does not report a problem, there is no problem

---------------------------------
Why Static Analysis Saves Money
---------------------------------

Shifts costs from later, expensive phases to earlier, cheaper phases

.. image:: image:: ../../images/cost_to_fix_bugs.png

-------------------
Why Use CodePeer?
-------------------

+ An efficient, potentially exhaustive code reviewer

  + Identifies constructs that may lead to run-time errors with a level of certainty

    + E.g. buffer overflows, division by zero

  + Flags legal but suspect code, typical of logic errors

+ Produces a detailed analysis of each subprogram, including preconditions and postconditions

  + Compares implicit, deduced specification (what it really does) to explicit specification (what it is supposed to do) and alerts reviewer if they don't match

+ Can be used retrospectively on existing code, to detect and remove latent bugs

  + Legacy code
  + Code from external sources

===================
CodePeer Overview
===================

------------------------------
CodePeer In A Nutshell (1/2)
------------------------------

+ :toolname:`CodePeer` is a static analysis tool

  + It provides feedback prior to execution and test
  + It provides "as-built" documentation to support source code review

+ It helps to identify and eliminate vulnerabilities and bugs in the application

  + And it does it early in the lifecycle

+ It is modular and scalable

  + Can be used on an entire project or a single file
  + Can be configured to be more or less strict, from very picky to laid back
  + Can be adapted to filter out or emphasize certain issues
  + Can concentrate on differences between baselines / versions

------------------------------
CodePeer In A Nutshell (2/2)
------------------------------

+ Large Ada support

  + Usable with Ada 83, 95, 2005, 2012
  + Usable with other compilers, including Apex, GHS, ObjectAda, VADS

+ It comes bundled with a Coding Standards Checker and a Metrics Tool

  + :toolname:`GNATCheck` and :toolname:`GNATMetric`

+ Detects runtime and logic errors

  + Checks (exhaustive): Initialization errors, run-time errors and assertion failures (16)
  + Warnings: dead code and suspicious code (17)
  + Race condition errors (exhaustive): unprotected access to global variables (3)

----------------------
CodePeer Integration
----------------------

+ Output: textual, XML, CSV, HTML
+ Command-line tool (uses GNAT project files)
+ Interactive use in :toolname:`GNAT Studio` and :toolname:`GNATbench` IDEs
+ Integration with Jenkins (continuous builder)
+ Integration with SonarQube (code quality visualization)

-----------------------------
Typical Users And Use Cases
-----------------------------

Developers
   While writing the code, so as to minimize introduction of (local) problems prior to integration of their work

Reviewers
   To annotate code with analysis of potential problems, including analysis of specific CWE issues

Project managers and quality engineers
   Who can track reported vulnerabilities day after day, and identify quickly newly introduced problems

Software auditors
   Who can run a one-shot analysis to identify overall vulnerabilities, hot spots, or issues with compliance to quality standards

=================
Getting Started
=================

--------------------------------------
Running CodePeer on the Command Line
--------------------------------------

:command:`codepeer -P <project> [-level <level>] [-output-msg[-only]] [-html[-only]]`

-P ``<project-file>``
   Specify the project file name.
   All files from the specified project tree (projects and subprojects) will be analyzed.

-level ``0|1|2|3|4|min|max``
   Specify the level of analysis performed: 0 for fast and light checkers,
   1 for fast and per subprogram analysis, 2 for slightly more
   accurate/slower (per small set of units), 3 for more accurate and much
   slower, and 4 for global analysis with no automatic partitioning (may
   exceed memory capacity and take a very long time).
   Default is level 0; min is equivalent to 0; max is equivalent to 4.

-output-msg[-only] [-output-msg switches]
   If specified, :toolname:`CodePeer` will output its results, in various formats.
   If -output-msg is given, :toolname:`CodePeer` will perform a new analysis, and output
   its results. Conversely, if -output-msg-only is specified, no new
   analysis is performed, and the results from the previous run (of the same
   level) will be emitted. You can control this output by adding switches
   (e.g. "-output-msg -csv -out report.csv" to generate a CSV file). See
   the following section for all relevant switches.


-html, -html-only
   Generate HTML output. If -html-only, do not run any analysis.

---------------------------------
Running CodePeer in GNAT Studio
---------------------------------

.. image:: image:: ../../images/codepeer_from_gs.jpg

---------------------
Project File Set Up
---------------------

Let's explore sections 1.4, 1.5 and 1.6 of the User's Guide

   * `Link: Basic Project File Setup<http://docs.adacore.com/codepeer-docs/users_guide/_build/html/introduction.html#basic-project-file-setup>`_

   * `Link: Project File Setup<http://docs.adacore.com/codepeer-docs/users_guide/_build/html/introduction.html#project-file-setup>`_

   * `Link: Advanced Project File Setup<http://docs.adacore.com/codepeer-docs/users_guide/_build/html/introduction.html#advanced-project-file-setup>`_

-------------------
CodePeer Tutorial
-------------------

+ Live Demo
+ If you want to reproduce on your side:

  + Get a fresh copy of the :toolname:`GNAT Studio` tutorial directory

    + From :filename:`GNATPRO/xxx/share/examples/gnatstudio/tutorial`
    + Contains the :filename:`sdc` project
    + Be sure to do this, the :toolname:`CodePeer` tutorial requires it

  + Put this copy in the :filename:`sources/codepeer` directory

    + Thus :filename:`sources/codepeer/tutorial` as a result

  + Open :toolname:`GNAT Studio` on this copy of the :filename:`sdc` project file
  + Open the :toolname:`CodePeer` Tutorial from :toolname:`GNAT Studio`

     + :menu:`Help` :math:`\rightarrow` :menu:`CodePeer` :math:`\rightarrow` :menu:`CodePeer Tutorial`

  + Walk through the steps of the :toolname:`CodePeer` tutorial

-----------------
CodePeer Levels
-----------------

.. container:: latex_environment tiny

   .. list-table::

      * - *Level 0*

        - Default level

      * -

        - Light and fast analysis performed via the Libadalang Light Checkers Integration

      * -

        - Very few false alarms

      * - *Level 1*

        - Run :toolname:`CodePeer`'s core engine subprogram by subprogram

      * -

        - Few false alarms

      * - *Level 2*

        - Analyze by groups of units

      * -

        - Slower analysis, more precise

      * -

        - Few false alarms

      * - *Level 3*

        - Semi-global analysis

      * -

        - Automatic partitioning to complete the analysis within the memory constraints of the machine

      * - *Level 4*

        - Global and exhaustive analysis

      * -

        - Analyze all units together with no partitioning and with all potential issues flagged (potentially high false alarms)

      * -

        -  May require large amounts of memory and time

--------------------------
CodePeer Levels Use Case
--------------------------

.. container:: latex_environment tiny

   .. list-table::

      * - *Level 0*

        - Get initial static analysis results.

      * -

        - At each developer's desk or after each commit.

      * -

        - Can enable *--simple-project* switch to avoid a full setup of your project.

      * - *Level 1*

        - After setting up the project file.

      * -

        - Includes light checkers from level 0.

      * -

        - At each developer's desk or after each commit.

      * - *Level 2*

        - After having clean results at level 1.

      * -

        - More detailed analysis with some level of interprocedural analysis.

      * -

        - At each developer's desk for small to medium code bases (e.g. less than 100K SLOC).

      * -

        - On servers automatically for larger code bases.

      * - *Level 3*

        - Semi-global analysis, to be used for code bases no larger than 1 million lines of code.

      * -

        - More detailed interprocedural analysis.

      * -

        - Suitable for automatic runs on servers.

      * - *Level 4*

        - Suitable for small to medium code bases (typically less than 200K SLOC).

      * -

        - Exhaustive analysis (all possible errors are reported).

      * -

        - When systematic review of all potential run-time checks is required.

------------------------------
"No False Positive" Mode
------------------------------

+ Enabled via :command:`-level 0` or :command:`messages min`
+ Suppresses output of messages more likely to be false positives
+ Allows programmers to focus initial work on likely problems
+ Can be combined with any level of analysis
+ :command:`-messages min` is default for levels 0, 1, and 2

--------------------------------
Running CodePeer regularly
--------------------------------

+ Historical database (SQLite) stores all past results per level
+ Can be stored in CM
+ Notion of baseline run:

  + Each run compared to some previous baseline run (default: first run)
  + Differences of messages shown in :toolname:`CodePeer` report window
  + :command:`-cutoff` to override baseline for a given run
  + :command:`-baseline` to set the reference baseline for future runs
  + Typical use: nightly run with :command:`-baseline`, daily development without

+ Can compare between two runs
+ Combine :command:`-cutoff` and :command:`-current` switches

===================================
CodePeer Messages and Annotations
===================================

--------------
LAL Checkers
--------------


--------------
LAL Checkers
--------------

+ procedure Check  is X : Integer :=  0 ; begin  pragma Assert ( X >  0 ) ; end Check ;
+ check.adb:4:4: medium warning: contract check (LAL checker): assertion failure

--------------
LAL Checkers
--------------

+ function Variant  return Integer is  type RGB is  ( Red, Green, Blue ) ;  type Rec ( Color : RGB := Red )  is  record  case Color is  when Red => Red_Value : Integer;  when Green => Green_Value : Integer;  when Blue => Blue_Value : Integer;  end case ;  end record ; Value : Rec with Import; begin  case Value.Color is  when Red =>  return Value.Red_Value;  when Green =>  return Value.Green_Value;  when Blue =>  return Value.Green_Value; *--  Oops copy/paste error*  end case ; end Variant ;
+ variant.adb:24:17: medium warning: discriminant check (LAL checker): invalid field 'Green_Value'

--------------
LAL Checkers
--------------

+ procedure Null_Deref  is  type Int_Access is  access Integer; X : Int_Access; Var : Integer; begin  if X =  null  then Var := X.all; *-- null dereference*  end if ; end Null_Deref ;
+ null_deref.adb:7:14: medium warning: access check (LAL checker): null dereference of 'X'

--------------
LAL Checkers
--------------

+ function Same_Op  ( X : Natural )  return Integer is begin  return  ( X +  1 )  /  ( X +  1 ) ; *--  Copy/paste error? Always return 1* end Same_Op ;
+ same_op.adb:3:11: medium warning: same operands (LAL checker):
+ operands of '/' are identical

--------------
LAL Checkers
--------------

+ function Dup  ( X : Integer )  return Integer is begin  if X >  0  then  declare A : Integer := X; B : Integer := A +  1 ; C : Integer := B +  1 ; D : Integer := C +  1 ;  begin  return D;  end ;  else  declare A : Integer := X; B : Integer := A +  1 ; C : Integer := B +  1 ; D : Integer := C +  1 ;  begin  return D; *-- Suspicious duplicated code*  end ;  end if ; end Dup ;
+ dup.adb:13:7: medium warning: code duplicated (LAL checker): code duplicated with line 4

--------------
LAL Checkers
--------------

+ function Always  ( X : Integer )  return Integer is  procedure Compute  with Import; begin  if X >  0  then Compute;  if X >  0  then  *--  Always True*  return  1 ;  end if ;  end if ;  return  0 ; end Always ;
+ always.adb:7:10: medium warning: test always true (LAL checker): 'X > 0' is always true

--------------
LAL Checkers
--------------

+ function Always  ( X : Integer )  return Integer is  procedure Compute  with Import; begin  if X >  0  then Compute;  if X <  0  then  *--  Always false*  return  1 ;  end if ;  end if ;  return  0 ; end Always ;
+ always.adb:7:10: medium warning: test always true (LAL checker):
+ 'X > 0' is always false

---------------------
Run-Time Checks
---------------------


-----------------
Run-Time Checks
-----------------

+ procedure Buffer_Overflow  is  type Int_Array is  array  ( 0  ..  2 )  of Integer; X, Y : Int_Array; begin  for I in X'Range loop X ( I )  := I +  1 ;  end loop ;  for I in X'Range loop Y ( X ( I ))  := I; *-- Bad when I = 2, since X (I) = 3*  end loop ; end Buffer_Overflow ;
+ buffer_overflow.adb:10:7: high: array index check fails here: requires (X (I)) in 0..2

-----------------
Run-Time Checks
-----------------

+ procedure Div  is  type Int is  range  0  ..  2  32 - 1 ; A : Int := Int'Last; X : Integer; begin  for I in Int range  0  ..  2  loop X := Integer ( A / I ) ; *--  division by zero when I = 0*  end loop ; end Div ;
+ div.adb:7:23: high: divide by zero fails here: requires I >= 1

-----------------
Run-Time Checks
-----------------

+ procedure Tag  is  type T1 is  tagged  null  record ;  package Pkg  is  type T2 is  new T1 with  null  record ;  procedure Op  ( X : T2 )  is  null ;  end Pkg ;  use Pkg;  type T3 is  new T2 with  null  record ;  procedure Call  ( X1 : T1'Class )  is  begin Op ( T2'Class ( X1 )) ;  end Call ; X1 : T1; X2 : T2; X3 : T3; begin Call ( X1 ) ; *-- not OK, Call requires T2'Class* Call ( X2 ) ; *-- OK* Call ( X3 ) ; *-- OK* end Tag ;
+ tag.adb:21:4: high: precondition (tag check) failure on call to downward.call: requires X1'Tag in {tag.pkg.t2, tag.t3}

-----------------
Run-Time Checks
-----------------

+ procedure Discr  is  subtype Length is Natural range  0  ..  10 ;  type T ( B : Boolean := True; L : Length :=  1 )  is  record I : Integer;  case B is  when True => S : String ( 1  .. L ) ; J : Integer;  when False => F : Float :=  5 . 0 ;  end case ;  end record ; X : T ( B => True, L =>  3 ) ;  function Create  ( L : Length; I : Integer; F : Float )  return T is  begin  return  ( False, L, I, F ) ;  end Create ; begin X := Create ( 3 , 2 , 6 . 0 ) ; *-- discriminant check failure* end Discr ;
+ div.adb:7:23: high: divide by zero fails here: requires I >= 1

-----------------
Run-Time Checks
-----------------

+ procedure Null_Deref  is  type Int_Access is  access Integer; X : Int_Access; begin  if X =  null  then X.all :=  1 ; *-- null dereference*  end if ; end Null_Deref ;
+ null_deref.adb:6:7: high: access check fails here: requires X /= null

-----------------
Run-Time Checks
-----------------

+ procedure Out_Of_Range  is  subtype Constrained_Integer is Integer range  1  ..  2 ; A : Integer;  procedure Proc_1  ( I :  in Constrained_Integer )  is  begin A := I +  1 ;  end Proc_1 ; begin A :=  0 ; Proc_1 ( I => A ) ; *--  A is out-of-range of parameter I* end Out_Of_Range ;
+ out_of_range.adb:12:17: high: range check fails here: requires A in 1..2

-----------------
Run-Time Checks
-----------------

+ with Ada.Integer_Text_IO; use Ada.Integer_Text_IO; with Ada.Text_IO; use Ada.Text_IO; procedure Overflow  is Attempt_Count : Integer := Integer'Last;  *--  Gets reset to zero before attempting password read* PW : Natural; begin  *--  Oops forgot to reset Attempt_Count*  loop Put ( "Enter password to delete system disk" ) ; Get ( PW ) ;  if PW =  42  then Put_Line ( "system disk deleted" ) ;  exit ;  else Attempt_Count := Attempt_Count +  1 ;  if Attempt_Count >  3  then Put_Line ( "max password count reached" ) ;  raise Program_Error;  end if ;  end if ;  end loop ; end Overflow ;
+ overflow.adb:17:41: high: overflow check fails here: requires Attempt_Count <= Integer_32'Last-1

-----------------
Run-Time Checks
-----------------

+ procedure Alias  is  type Int_Array is  array  ( 1  ..  10 )  of Integer; A, B : Int_Array :=  ( others  =>  1 ) ;  procedure In_Out  ( A : Int_Array; B : Int_Array; C :  out Int_Array )  is  begin  *--  Read A multiple times, and write C multiple times:*  *--  if A and C alias and are passed by reference, we are in trouble!* C ( 1 )  := A ( 1 )  + B ( 1 ) ; C ( 1 )  := A ( 1 )  + B ( 1 ) ;  end In_Out ; begin  *--  We pass A as both an 'in' and 'out' parameter: danger!* In_Out ( A, B, A ) ; end Alias ;
+ alias.adb:18:4: high: precondition (aliasing check) failure on call to alias.in_out: requires C /= A

-----------------
Run-Time Checks
-----------------

+ Checks are reported in 2 possible places:

  + Where the error may occur, or
  + Where a caller passes in a value causing the error in a call

+ Understanding case 2 above:

  + Look at generated preconditions
  + Precondition check: look at associated checks and backtrace (in :toolname:`GNAT Studio` or via -show-backtr aces )
+ procedure Alias  is  type Int_Array is  array  ( 1  ..  10 )  of Integer; A, B : Int_Array :=  ( others  =>  1 ) ;
+ *-- (generated) precondition (aliasing check): C /= A*
+ *-- (generated) precondition (aliasing check): C /= B*
+  procedure In_Out  ( A : Int_Array; B : Int_Array; C :  out Int_Array )  is  begin  *--  Read A multiple times, and write C multiple times:*  *--  if A and C alias and are passed by reference, we are in trouble!* C ( 1 )  := A ( 1 )  + B ( 1 ) ; C ( 1 )  := A ( 1 )  + B ( 1 ) ; *-- aliasing check*  end In_Out ; begin  *--  We pass A as both an 'in' and 'out' parameter: danger!* In_Out ( A, B, A ) ; end Alias ;
+ alias.adb:18:4: high: precondition (aliasing check) failure on call to alias.in_out: requires C /= A ; aliasing check at alias.adb:13:16

-----------------------------------------------
Uninitialized and Invalid Variable Checks
-----------------------------------------------

+ procedure Uninit  is A : Integer; B : Integer; begin A := B; *--  we are reading B which is uninitialized!* end Uninit ;
+ uninit.adb:5:9: high: validity check: B is uninitialized here

-----------------
User Checks
-----------------


-------------
User Checks
-------------

+ procedure Assert  is  function And_Or  ( A, B : Boolean )  return Boolean is  begin  return False;  end And_Or ; begin  pragma Assert ( And_Or ( True, True )) ; end Assert ;
+ assert.adb:9:19: high: assertion fails here

-------------
User Checks
-------------

+ with Ada.Integer_Text_IO; use Ada.Integer_Text_IO; with Ada.Text_IO; use Ada.Text_IO; procedure Overflow  is Attempt_Count : Integer := Integer'Last;  *--  Gets reset to zero before attempting password read* PW : Natural; begin  *--  Oops forgot to reset Attempt_Count*  loop Put ( "Enter password to delete system disk" ) ; Get ( PW ) ;  if PW =  42  then Put_Line ( "system disk deleted" ) ;  exit ;  else Attempt_Count := Attempt_Count +  1 ;  if Attempt_Count >  3  then Put_Line ( "max password count reached" ) ;  raise Program_Error;  end if ;  end if ;  end loop ; end Overflow ;
+ overflow.adb:21:13: high: conditional check raises exception here: requires Attempt_Count <= 3

-------------
User Checks
-------------

+ procedure Raise_Exc  is X : Integer :=  raise Program_Error; begin  null ; end Raise_Exc ;
+ raise_exc.adb:2:19: low: raise exception unconditional raise

-------------
User Checks
-------------

+ procedure Pre  is  function "" ( Left, Right : Float )  return Float  with Import, Pre => Left /=  0 . 0 ; A : Float :=  1 . 0 ; begin A :=  ( A -  1 . 0 )    2 . 0 ; end Pre ;
+ pre.adb:7:19: high: precondition (user precondition) failure on call to pre."": requires Left /= +0

-------------
User Checks
-------------

+ procedure Post  is  type States is  ( Normal_Condition, Under_Stress, Bad_Vibration ) ; State : States;  function Stress_Is_Minimal  return Boolean is  ( State = Normal_Condition ) ;  function Stress_Is_Maximal  return Boolean is  ( State = Bad_Vibration ) ;  procedure Decrement  with Pre =>  not Stress_Is_Minimal, Post =>  not Stress_Is_Maximal;  procedure Decrement  is  begin State := State'Val ( State'Pos ( State )  +  1 ) ;  end Decrement ; begin  ... end Post ;
+ post.adb:16:8: high: postcondition failure on call to post.decrement: requires State <= Under_Stress

---------------------------
Race Condition Checks
---------------------------


---------------------------
Race Condition Checks
---------------------------

+  procedure Increment  is  begin Acquire;  if Counter = Natural'Last then Counter := Natural'First;  else Counter := Counter +  1 ;  end if ; Release;  end Increment ;
+  procedure Decrement  is  begin  if Counter = Natural'First then  *--  reading Counter without any lock* Counter := Natural'Last; *--  writing without any lock*  else Counter := Counter -  1 ; *--  reading and writing without any lock*  end if ;  end Decrement ; end Race ;
+ package Race  is  procedure Increment ;  pragma Annotate
+  ( CodePeer, Multiple_Thread_Entry_Point, "Race.Increment" ) ;  *--  This is a task type: there will be multiple*
+ *--  threads calling this subprogram*  procedure Decrement ;  pragma Annotate
+  ( CodePeer, Multiple_Thread_Entry_Point, "Race.Decrement" ) ;  *--  Ditto* end Race ;
+ package body Race  is Counter : Natural :=  0 ;  procedure Acquire with Import;  procedure Release with Import;  pragma Annotate
+  ( CodePeer, Mutex, "Race.Acquire", "Race.Release" ) ;
+ race.adb:20:10: mismatched protected access of shared object Counter via race.increment race.adb:20:10: unprotected access of Counter via race.decrement race.adb:21:18: mismatched protected access of shared object Counter via race.increment race.adb:21:18: unprotected access of Counter via race.decrement race.adb:23:18: mismatched protected access of shared object Counter via race.increment race.adb:23:21: mismatched protected access of shared object Counter via race.increment race.adb:23:18: unprotected access of Counter via race.decrement race.adb:23:21: unprotected access of Counter via race.decrement

------------------------------
R edundant Code Warnings
------------------------------


-----------------------------
Redundant Code Warnings
-----------------------------

+ procedure Dead_Code  ( X :  out Integer )  is I : Integer :=  10 ; begin  if I <  4  then X :=  0 ;  elsif I >=  10  then X :=  0 ;  else X :=  0 ;  end if ; end Dead_Code ;
+ dead_code.adb:5:9: medium warning: dead code because I = 10 dead_code.adb:9:9: medium warning: dead code because I = 10

-----------------------------
Redundant Code Warnings
-----------------------------

+ procedure Predetermined is I : Integer :=  0 ; begin case I is when 0 => ... when 1 => ... when others => ... end case; end Predetermined ;
+ predetermined.adb:4:4: low warning: test predetermined because I = 0

-----------------------------
Redundant Code Warnings
-----------------------------

+ procedure Dead_Code  ( X :  out Integer )  is I : Integer :=  10 ; begin  if I <  4  then X :=  0 ;  elsif I >=  10  then X :=  0 ;  else X :=  0 ;  end if ; end Dead_Code ;
+ dead_code.adb:4:9: low warning: test always false because I = 10
+ dead_code.adb:6:4: medium warning: test always true because I = 10

-----------------------------
Redundant Code Warnings
-----------------------------

+ procedure Condition  is  type L is  ( A, B, C ) ;  procedure Or_Else  ( V : L )  is  begin  if V /= A or  else V /= B then  return ;  else  raise Program_Error;  end if ;  end Or_Else ;
+ condition.adb:6:25: medium warning: condition predetermined because (V /= B) is always true

-----------------------------
Redundant Code Warnings
-----------------------------

+ procedure Loops  is Buf : String :=  "The"  & ASCII.NUL; BP : Natural; begin Buf ( 4 )  :=  'a' ; *-- Eliminates null terminator* BP := Buf 'First ;  while  True  loop BP := BP +  1 ;  exit  when Buf ( BP -  1 )  = ASCII.NUL; *-- Condition never reached*  end loop ; end Loops ;
+ loops.adb:8:10: medium warning: loop does not complete normally

-----------------------------
Redundant Code Warnings
-----------------------------

+ procedure Unused_Assignment  is C : Character := Get_Character; begin  null ; *-- C is not used in this subprogram* end Unused_Assignment ;
+ unused_assignment.adb:2:4: medium warning: unused assignment into C

-----------------------------
Redundant Code Warnings
-----------------------------

+ package body Unused_Global  is G : Integer;  procedure Proc0  is  begin G :=  123 ;  end Proc0 ;  procedure Proc1  is  begin Proc0;  end Proc1 ;  procedure Proc2  is  begin Proc1; G :=  456 ; *-- override effect of calling Proc1*  end Proc2 ; end Unused_Global ;
+ unused_global.adb:16:7: low warning: unused assignment to global G in unused_global.proc1

-----------------------------
Redundant Code Warnings
-----------------------------

+ with Search; procedure Unused_Out  is Table : Int_Array ( 1 .. 10 )  :=  ( others  =>  0 ) ; Found : Boolean; Index : Integer; begin Search.Linear_Search ( Table, 0 , Found, Index ) ; end Unused_Out ;
+ unused_out.adb:8:25: medium warning: unused out parameter Found unused_out.adb:8:25: medium warning: unused out parameter Index

-----------------------------
Redundant Code Warnings
-----------------------------

+ procedure Self_Assign  ( A :  in  out Integer )  is B : Integer; begin B := A; A := B; end Self_Assign ;
+ self_assign.adb:5:6: medium warning: useless self assignment into A

--------------------------
Suspicious Code Warnings
--------------------------


------------------------------
Suspicious Code Warnings
------------------------------

+ package body Stack  is  procedure Push  ( S :  in  out Stack_Type; V : Value )  is  begin  if S.Last = S.Tab 'Last  then  raise Overflow;  end if ; S.Last := S.Last -  1 ; *--  Should be S.Last + 1* S.Tab ( S.Last )  := V;  end Push ;
+ stack.adb:3:4: medium warning: suspicious precondition for S.Last: not a contiguous range of values

------------------------------
Suspicious Code Warnings
------------------------------

+ procedure In_Out  is  type T is  record I : Integer;  end record ;  procedure Take_In_Out  ( R :  in  out T )  is  begin R.I := R.I +  1 ;  end Take_In_Out ;  procedure Take_Out  ( R :  out T; B : Boolean )  is  begin Take_In_Out ( R ) ; *-- R is 'out' but used as 'in out'*  end Take_Out ;
+ in_out.adb:13:7: medium warning: suspicious input R.I: depends on input value of out-parameter

------------------------------
Suspicious Code Warnings
------------------------------

+ procedure Unread  ( X :  in  out Integer )  is begin X :=  0 ; *-- X is assigned but never read* end Unread ;
+ unread.adb:1:1: medium warning: unread parameter X: could have mode out

------------------------------
Suspicious Code Warnings
------------------------------

+ procedure Unassigned  ( X :  in  out Integer; Y :  out Integer )  is begin Y := X; *-- X is read but never assigned* end Unassigned ;
+ unread.adb:1:1: medium warning: unassigned parameter X: could have mode in

------------------------------
Suspicious Code Warnings
------------------------------

+ procedure Constant_Op  is  type T is  new Natural range  0  ..  14 ;  function Incorrect  ( X : T )  return T is  begin  return X /  ( T 'Last  +  1 ) ;  end ; begin  null ; end Constant_Op ;
+ constant_op.adb:6:16: medium warning: suspicious constant operation X/15 always evaluates to 0

-----------------------------------------
Automatically Generated Annotations
-----------------------------------------


-----------------------------------------
Automatically Generated Annotations
-----------------------------------------

+ External Tools Integration

-------------------
GNAT Warnings
-------------------

+ GNAT warnings can be generated by :toolname:`CodePeer` (>= 18. x )

  + --gnat-warnings= xxx (uses -gnatw xxx )

+ Messages are stored in the database, displayed and filtered as any other message
+ Manual justification can be stored in the database
+ Manual justification in the source is achieved via pragma Warnings instead of pragma Annotate

------------------------
GNATcheck messages
------------------------

+ GNATcheck messages can be generated by :toolname:`CodePeer` (>= 19. x)

  + --gnatcheck

+ Uses the GNATcheck rules file as defined in your project file in package Check
+ Messages are stored in the database, displayed and filtered as any other message
+ Manual justification can be stored in the database
+ Manual justification in the source is achieved via pragma Annotate (GNATcheck, ...)

------------------------
GNATcheck messages
------------------------

+ Finding the Right Settings

---------------------
System Requirements
---------------------

+ Fast 64bits machine with multiple cores and memory
+ Server : 24 to 48 cores with at least 2GB per core (48 to 96GB)
+ Local desktop : 4 to 8 cores, with at least 8 to 16GB
+ Avoid slow filesystems : networks drives (NFS, SMB), configuration management filesystems (e.g. ClearCase dynamic views).

  + If not possible, at least generate output file in a local disk via the Output_Directory and Database_Directory project attributes.

+ Global analysis ( -level max ): At least 12GB + 1GB per 10K SLOC, e.g. At least 32GB for 200K SLOC.

--------------------------
Analyze Messages ( 1/4 )
--------------------------

+ Start with default (level 0)
+ If the run is mostly clean/contains mostly interesting messages, run at next level (e.g. level 1) and iterate until number of false alarms/timing is too high for your needs
+ project My_Project is for Source_Dirs use ... package CodePeer is for Switches use ( "-level" , "1" ); end CodePeer ; end My_Project ;
+ $ codepeer -Pmy_project -level 1 ...

------------------------
Analyze Messages (2/4)
------------------------

+ If a run contains many messages, analyze some and identify groups of uninteresting messages
+ Exclude categories of uninteresting messages via e.g. --be-messages (starting with level 1).

------------------------
Analyze Messages (3/4)
------------------------

+ Filtering of messages

  + -output-msg -hide-low  on the command line
  + Check boxes to filter on message category / rank in :toolname:`GNAT Studio` and HTML
  + --be-messages --gnat-warnings --lal-checkers  switches
  + -messages min/normal/max
  + Pattern-based automatic filtering ( MessagePatterns.xml )

+ For example to disable messages related to access check:
+ --be-messages=-access_check
+ If many uninteresting messages in the same file, you can exclude this file from analysis (see next slides)

------------------------
Analyze Messages (4/4)
------------------------

+ C hoose relevant messages based on ranking

  + Rank = severity + certainty
  + High : certain problem
  + Medium : possible problem, or certain with low severity
  + Low : less likely problem (yet useful for exhaustivity)

+ When analysing existing code, start looking at High messages first, then Medium , and finally if it makes sense, Low messages.
+ A recommended setting is to consider High and Medium messages (default in :toolname:`GNAT Studio` and HTML interfaces).

-------------------------
Run CodePeer faster
-------------------------

+ Use a 64-bit machine with a lot of memory and cores
+ Lower analysis level ( -level <num> ), use -j0 (default)
+ Identify files taking too long to analyze and d isable analysis of selected subprograms or files
+ analyzed main.scil in 0.05 seconds analyzed main__body.scil in 620.31 seconds analyzed pack1__body.scil in 20.02 seconds analyzed pack2__body.scil in 5.13 seconds

----------------------
Partial Analysis
----------------------

+ Excluding Subprograms or Packages From Analysis
+ procedure Complex_Subprogram (...) is
+  pragma Annotate (CodePeer, Skip_Analysis);
+ begin
+ ...
+ end Complex_Subprogram;
+ package Complex_Package is
+  pragma Annotate (CodePeer, Skip_Analysis);
+ ...
+ end Complex_Package;
+ pragma Annotate (CodePeer, Skip_Analysis);

----------------------
Partial Analysis
----------------------

+ Excluding Files From Analysis
+ Excluding Directories From Analysis
+ Excluding Projects From Analysis
+ package CodePeer is
+  for Excluded_Source_Files use ( "xxx.adb" );
+  -- Analysis of xxx.adb generates lots of timeouts, skip it for now
+ end CodePeer;
+ package CodePeer is
+  for Excluded_Source_Dirs use ( "directory1" , "directory2" );
+ end CodePeer;
+ for Externally_Built use  "True" ;

----------------------
Partial Analysis
----------------------

+ CodePeer Workflows

--------------------
CodePeer Workflows
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

------------------------------------------------
Analyzing code locally prior to commit ( 1/2 )
------------------------------------------------

+ Fast analysis done at each developer's desk
+ Solution #1

  + Use :toolname:`GNAT Studio` menu CodePeer>Analyze File (or File by File ) after each compilation, before testing.
  + Incremental, fast analysis

+ Solution #2

  + run :toolname:`CodePeer` with -level 1/2 -baseline ( CodePeer>Analyze... )
  + Local :toolname:`CodePeer` database used for comparison
  + Look at Added messages only

----------------------------------------------
Analyzing code locally prior to commit (2/2)
----------------------------------------------

+ For each new message:

  + Fix the code if a real issue is found
  + Justify false positives via pragma Annotate
  + Refine the settings to e.g. exclude some message kinds or subprograms/files from analysis

--------------------------
Nightly runs on a server
--------------------------

+ :toolname:`CodePeer` run daily on a dedicated server (highest suitable level) allowing users to justify messages manually via :toolname:`CodePeer` web server.
+ Messages already justified through pragma Annotate do not need to be justified again.
+ These runs will typically be run nightly to take into account commits of the day, and provide results to users the next morning .
+ Developers can analyze the results via the web interface or from :toolname:`GNAT Studio` by accessing the database remotely.
+ Developers then fix the code, or justify the relevant messages using either pragma Annotate or via :toolname:`GNAT Studio` or the web interface.
+ *Optionally* : for each release, results are committed under CM for traceability purposes.

-----------------------------------------------
Continuous runs on a server after each change
-----------------------------------------------

+ :toolname:`CodePeer` is run on a dedicated server with lots of resources at a level suitable for performing runs rapidly (e.g. level 0 or 1 )
+ These runs do not need to be exhaustive: focus is on differences from previous run .
+ Continuous runs trigger on new repository changes (e.g. via Jenkins)
+ A summary is sent to developers via email or a web interface:
+ codepeer -Pprj -output-msg -only -show-added | grep "\[added\]"
+ Developers then fix the code, or justify the relevant messages

  + via pragma Annotate in Source Code or via web interface.
  + or wait for the next nightly run to post a manual analysis via the HTML Output.

------------------------------
Combined desktop/nightly run
------------------------------

+ Fast analysis of code changes done at each developer's desk.
+ A longer and more complete analysis is performed nightly on a powerful server .
+ Combination of *Analyzing code locally prior to commit* and *Nightly runs on a server* .

---------------------------------
Combined continuous/nightly run
---------------------------------

+ Fast analysis of code changes done after each commit on a server
+ A longer and more complete analysis is performed nightly on a  powerful server .
+ Or alternatively: a baseline run is performed nightly at same level as continuous runs ( -baseline ).
+ Combination of *Analyzing code locally prior to commit* and *Continuous runs on a server after each change* .

-----------------------------------------
Combined desktop/continuous/nightly run
-----------------------------------------

+ Fast analysis of code changes done at each developer's desk.
+ An analysis (fast but potentially longer than the one performed by developers) is done after each commit on a server
+ A more exhaustive analysis performed nightly on a powerful server .
+ Combination of *Analyzing code locally prior to commit* , *Nightly runs on a server* and *Continuous runs on a server after each change* .

--------------------------------------------
Software customization per project/mission
--------------------------------------------

+ A core version of your software gets branched out or instantiated and modified on a per-project /mission basis.
+ *Continuous solution*

  + Share message justifications via pragma Annotate
  + Merge of justifications handled via standard CM
  + Separate :toolname:`CodePeer` runs on all active branches, database used to compare runs on a given branch

+ *One shot solution*

  + Copy the justifications from the DB at branch point
  + Maintain it separately from there ( *fork* )
  + Separate :toolname:`CodePeer` runs on all active branches, database used to compare runs on a given branch

-------------------------------------------
Compare local changes with master ( 1/3 )
-------------------------------------------

+ Analysis running on server with latest source version
+ The (gold) database gets updated when sources are updated ( -baseline switch)
+ Developers pre-validate changes locally with :toolname:`CodePeer` prior to commit, in a separate sandbox and using the same analysis settings.
+ Continuous integration : local user creates a separate branch and commit his change on this branch

-----------------------------------------
Compare local changes with master (2/3)
-----------------------------------------

+ A continuous builder (e.g. Jenkins) is monitoring user branches and triggers an analysis that will:
+ Copy in a separate sandbox the database from the reference (nightly) run.
+ Perform a run with the same settings as the reference run
+ Send results to the user either via its web server and the :toolname:`CodePeer` HTML interface, or by generating a textual report (-output-msg).
+ Can be combined with -show-added so that the user can concentrate on the new messages found:
+ codepeer -Pprj -output-msg -show-added | grep "\[added\]"
+ Throw out this separate sandbox

-----------------------------------------
Compare local changes with master (3/3)
-----------------------------------------

+ Once the user receives the report he can address the findings by modifying the code, or using pragma Annotate, or post an analysis on the gold database after his change is merged on the master branch and a new baseline run is available for review.
+ Another, more manual alternative involves doing a local copy of the gold database to the user space, run :toolname:`CodePeer` there, look at differences then throw out this local environment.

----------------------------------------------
Multiple teams analyzing multiple subsystems
----------------------------------------------

+ Large software system composed of multiple subsystems maintained by different teams
+ Perform a separate analysis for each subsystem , using a separate workspace and database
+ Create one project file (.gpr) per subsystem
+ To resolve dependencies between subsystem, use limited with :
+ Run :toolname:`CodePeer` with:
+ codepeer -Psubsystem1 --no-subprojects
+ limited with "subsystem1" ;
+ limited with "subsystem2" ;
+ project  Subsystem3  is
+ [...] end Subsystem3 ;

--------------------------------------------
Use CodePeer to generate a security report
--------------------------------------------

+ Perform a security oriented analysis and generate a separate report, taking advantage :toolname:`CodePeer` support for CWE ( http://cwe.mitre.org )
+ Achieved via the --security-report switch
+ Use generated codepeer-security-report.html as is or convert it to PDF

--------------------------------------------
Use CodePeer to generate a security report
--------------------------------------------

+ Justifying :toolname:`CodePeer` Messages

----------------------------------------
Justifying CodePeer messages (1/2)
----------------------------------------

+ Add review status in database

  + :toolname:`GNAT Studio`: select review icon on message(s)
  + HTML web server: click on Add Review button above messages
  + Displayed with -output-msg-only -show-reviews (-only)

+ Add message review pragma in code

  + pragma Annotate  added next to code with message
  + 2 modalities: False_Positive  or Intentional
  + Also added in the database

----------------------------------------
Justifying CodePeer messages (2/2)
----------------------------------------

+ Use spreadsheet tool

  + Export messages in CSV format
  +  codepeer -Pprj -output-msg-only -csv
  + Review them via the spreadsheet tool (e.g. Excel)
  + Import back reviews into the :toolname:`CodePeer` database
  +  codepeer_bridge --import-reviews

+ Use external justification connected to output

  + Textual output: compiler-like messages or CSV format

----------------------------------------
Justifying CodePeer messages (2/2)
----------------------------------------

+ CodePeer Customization

------------------------------------------
CodePeer specific project attributes
------------------------------------------


-----------------------------------------
Project specialization for CodePeer
-----------------------------------------

+ then: compile with gprbuild -P my_project.gpr -XBuild=Production
+ analyze with :toolname:`CodePeer` -P my_project.gpr -XBuild=CodePeer

------------------------------------
Custom API for Race Conditions
------------------------------------

+ pragma Annotate  can identify entry points and locks other than Ada tasks and protected objects

-----------------
Report file
-----------------

+ You can combine some or all of the following switches to generate a report file
+ Mandatory switches:

  + -output-msg
  + -out <report file>

+ Optional switches

  + -show-header
  + -show-info
  + -show-removed
  + -show-reviews
  + -show-added
+ package CodePeer is
+  for Switches use
+ ("-level", "max", "-output-msg",
+ "-out", "report_file.out",
+ "-show-header", "-show-info");
+ end CodePeer;
+ date : YYYY-MM-DD HH:MM:SS
+ codepeer version : 18.2 (yyyymmdd)
+ host : Windows 64 bits
+ command line : codepeer -P my_project.gpr
+ codepeer switches : -level max -output-msg -out report_file.out -show-header -show-info
+ current run number: 4
+ base run number : 1
+ excluded file : /path/to/unit3.adb
+ unit1.ads:1:1: info: module analyzed: unit1
+ unit1.adb:3:1: info: module analyzed: unit1__body
+ unit2.adb:12:25: medium: divide by zero might fail: requires X /= 0
+ [...]

-----------------
Report file
-----------------

+ :toolname:`CodePeer` for Certification

----------------------
CodePeer and CWE
----------------------

+ MITRE's Common Weakness Enumeration is a set of common vulnerabilities in software applications
+ It is referenced in many government contracts and cyber-security requirements
+ :toolname:`CodePeer` is officially CWE-compatible

  + https://cwe.mitre.org/compatible/questionnaires/43.html

+ Mapping is provided between :toolname:`CodePeer` findings and CWE identifiers

---------------------------
CodePeer and DO178B/C
---------------------------

+ :toolname:`CodePeer` supports DO-178B/C Avionics Standard
+ DO-178C Objective A-5.6 (activity 6.3.4.f):

  + Code Accuracy and Consistency: The objective is to determine the correctness and consistency of the Source Code, including stack usage, memory usage, fixed point arithmetic overflow and resolution , floating-point arithmetic , resource contention and limitations, worst-case execution timing, exception handling, use of uninitialized variables , cache management, unused variables , and data corruption due to task or interrupt conflicts . The compiler (including its options), the linker (including its options), and some hardware features may have an impact on the worst-case execution timing and this impact should be assessed.

+ :toolname:`CodePeer` helps reduce the scope of manual review
+ See Booklet: "AdaCore Technologies for DO-178C/ED-12C"

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
+ See Booklet: "AdaCore Technologies for CENELEC EN 50128:2011"

  + Authored by Jean-Louis Boulanger & Quentin Ochem

------------------------------------
CodePeer and CENELEC - EN50128
------------------------------------

+ How Does CodePeer Work?

-----------------------------
How does CodePeer work?
-----------------------------

+ :toolname:`CodePeer` computes the possible value of every variable and every expression at each program point.
+ It starts with leaf subprograms and propagates information up in the call-graph, iterating to handle recursion.
+ For each subprogram:

  + It computes a precondition that guards against check failures.
  + It issues check/warning messages for the subprogram.
  + It computes a postcondition ensured by the subprogram.
  + It uses the generated subprogram contract (precondition + postcondition) to analyze calls.

-----------------------------
How does CodePeer work?
-----------------------------

+ See *CodePeer By Example* for more details

-----------------------------------------
CodePeer limitations and heuristics
-----------------------------------------

+ Let's explore section 7 .12 of the User's Guide
+ http://docs.adacore.com/codepeer-docs/users_guide/_build/html/appendix.html#codepeer-limitations-and-heuristics

-------------------------
CodePeer references
-------------------------

+ :toolname:`CodePeer` User's Guide and Tutorial

  + online: https://www.adacore.com/documentation#CodePeer
  + in local install at share/doc/codepeer/users_guide (or tutorial)
  + from :toolname:`GNAT Studio` in Help=>CodePeer=>CodePeer User's Guide (or Tutorial)

+ :toolname:`CodePeer` website

  + http://www.adacore.com/codepeer
  + videos , product pages, articles, challenges

+ Book chapter on :toolname:`CodePeer`

  + In Static Analysis of Software: The Abstract Interpretation , published by Wiley (2012)
