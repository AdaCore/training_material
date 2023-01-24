***************
Flow Analysis
***************

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
What is Flow Analysis?
------------------------

* **First** static analysis performed by :toolname:`GNATprove`
* Models the **variables** used by a subprogram

  - Global variables
  - Scope variables (local variables of enclosing scope)
  - Local variables
  - Formal parameters

* Models how **information flows** through the statements in the subprogram

  - From initial values of variables
  - To final values of variables

* Performs checks and detects **violations**

--------------------------
Control Flow Graph (CFG)
--------------------------

* A representation, using **graph notation**, of all **paths** that might be traversed
  through a program during its execution [Wikipedia]

.. container:: columns

 .. container:: column

    .. code:: ada

       function Is_Positive
         (X : Integer)
         return Boolean
       with Post =>
         Is_Positive'Result = (X > 0)
       is
       begin
          if X > 0 then
             return True;
          else
             return False;
          end if;
       end Is_Positive;

 .. container:: column

    .. image:: control_flow_graph.jpg

--------------------------------
Program Dependence Graph (PDG)
--------------------------------

* **Extension** of the CFG with information on **data flows**
* Control Dependence Graph

  - Compute post-dominators nodes: a node z is said to post-dominate a node n
    if **all** paths to the exit node of the graph starting at n must go through z

* Data Dependence Graph

  - Compute def-use chains rooted at variable definitions

* Transitive Dependence Graph

  - Compute how outputs of a call depend on its inputs

* Flow analysis checks are translated into **queries** on the PDG

===============
Flow Analysis
===============

--------------------------------------
Uncontrolled Data Visibility Problem
--------------------------------------

.. image:: subprograms_accessing_global.png

* Effects of changes are **potentially pervasive** so one must understand
  everything before changing anything

---------------------------
Data Dependency Contracts
---------------------------

* Introduced by aspect :ada:`Global`
* Optional, but must be complete if specified
* Optional mode can be :ada:`Input` (default), :ada:`Output`, :ada:`In_Out`
  or :ada:`Proof_In`

  .. code:: Ada

     procedure Proc
     with
       Global => (Input    => X,
                  Output   => (Y, Z),
                  In_Out   => V,
                  Proof_In => W);

* :ada:`Proof_In` used for inputs **only** referenced in **assertions**
* :ada:`Global => null` used to state that no global variable is read/written
* Functions can have only :ada:`Input` and :ada:`Proof_In` global variables

  - Remember: no side-effects in functions!

----------------------------
Data Initialization Policy
----------------------------

* Subprogram :dfn:`inputs` are input parameters and globals

  - parameters of mode :ada:`in` and :ada:`in out`
  - global variables of mode :ada:`Input` and :ada:`In_Out`

* Subprogram :dfn:`outputs` are output parameters and globals

  - parameters of mode :ada:`out` and :ada:`in out`
  - global variables of mode :ada:`Output` and :ada:`In_Out`

* Inputs should be completely initialized on a call
* Outputs should be completely initialized after a call
* Stricter policy than in Ada

  - Allows **modular analysis** of initialization
  - Relaxed initialization will be seen in course on Advanced Proof

--------------------------
Stricter Parameter Modes
--------------------------

**Initial Read** - Initial value read

**Partial Write** - Object partially written: either part of the object
written, or object written only on some paths, or both

**Full Write** - Object fully written on all paths

 .. list-table::
   :header-rows: 1

  * - Initial Read

    - Partial Write
    - Full Write
    - Parameter Mode

  * - |checkmark|

    -

    -

    - in

  * - |checkmark|

    - |checkmark|

    -

    - in out

  * - |checkmark|

    -

    - |checkmark|
    - in out

  * -

    - |checkmark|

    -

    - in out

  * -

    -

    - |checkmark|
    - out

* Similar rules for modes of global variables

----------------------------------------------
Violations of the Data Initialization Policy
----------------------------------------------

.. container:: columns

 .. container:: column

    * Parameter only partially written should be of mode :ada:`in out`

    |

    .. code:: ada

       procedure Cond_Init
         (X    : out T;
          -- Incorrect
          Cond : Boolean) is
       begin
          if Cond then
             X := ..;
          end if;
       end Cond_Init;

 .. container:: column

    * Global variable only partially written should be of mode :ada:`In_Out`

    .. code:: ada

       X : T;
       procedure Cond_Init
         (Cond : Boolean)
       with
         Global => (Output => X)
         -- Incorrect
       is
       begin
          if Cond then
             X := ..;
          end if;
       end Cond_Init;

-----------------------------------------
Generation of Data Dependency Contracts
-----------------------------------------

* :toolname:`GNATprove` computes a correct approximation

  - Based on the implementation
  - Using either specified or generated contracts for calls
  - More precise generation for SPARK code than for Ada code

* Generated contract may be imprecise

  - Output may be computed as both input and output

    + Because it is not known if the initial value is read
    + Because it is not known if the object is fully written on all paths

  - Precision can be recovered by adding a user contract

--------------
Bronze Level
--------------

* Check that each object read has been initialized
* Check that code respects data dependency contrats

  .. code:: Ada

     procedure Swap (X, Y : in out Integer)
     with
       Global => null; -- Wrong

     procedure Swap (X, Y : in out Integer) is
     begin
        Temp := X;
        X := Y;
        Y := Temp;
     end Swap;

* Errors for most serious issues, need fixing for proof
* Warn on unused variables, ineffective statements

---------------
Flow Warnings
---------------

* Ineffective statement = statement without effects

  - Dead code
  - Or statement does not contribute to an output
  - Or effect of statement is hidden from :toolname:`GNATprove`

* Warnings can be suppressed with pragma :ada:`Warnings`

  .. code:: ada

     pragma Warnings (Off, "statement has no effect",
                      Reason => "debug");
     Debug_Print (X);
     pragma Warnings (On, "statement has no effect");

* Optional first pragma argument :ada:`GNATprove` indicates it is specific to
  :toolname:`GNATprove`

==============================
Limitations of Flow Analysis
==============================

-----------------------------------
Analysis of Value-Dependent Flows
-----------------------------------

* Flow analysis depends only on control flow, not on values
* Flow analysis is imprecise on value-dependent flows

  .. code:: Ada

     procedure Absolute_Value
       (X : Integer;
        R : out Natural) -- Initialization check fails
     is
     begin
       if X < 0 then
         R := -X;
       end if;
       if X >= 0 then
         R := X;
       end if;
     end Absolute_Value;

* Use control flow instead: use :ada:`if-then-else` above

----------------------------------------
Analysis of Array Initialization (1/2)
----------------------------------------

* Array indexes are values
* Flow analysis does not depend on values
* Flow analysis treats array assignment as a partial write

  - When assigning to an array index
  - When assigning to an array slice

  .. code:: Ada

     type T is array (1 .. 10) of Boolean;

     procedure Init_Array (A : out T) is -- Initialization check fails
     begin
        A (1) := True;
        A (2 .. 10) := (others => False);
     end Init_Array;

* No such imprecision for record components

----------------------------------------
Analysis of Array Initialization (2/2)
----------------------------------------

* Use array aggregates when possible

  .. code:: Ada

     type T is array (1 .. 10) of Boolean;

     procedure Init_Array (A : out T) is -- Initialization check proved
     begin
        A := (1 => True, 2 .. 10 => False);
     end Init_Array;

* Do not please the tool! :ada:`A` is not :ada:`in out` here!

  - Otherwise, caller is forced to initialize :ada:`A`

* Some built-in heuristics recognize an initializing loop

  .. code:: Ada

     procedure Init_Array (A : out T) is -- Initialization check proved
     begin
        for J in A'Range loop
           A (J) := False;
        end loop;
     end Init_Array;

---------------------------
Dealing with False Alarms
---------------------------

* Check messages can be justified with pragma :ada:`Annotate`

  .. code:: Ada

     procedure Init_Array
       (A : out T) -- Initialization check justified
     is
        pragma Annotate (GNATprove, False_Positive,
                         """A"" might not be initialized",
                         "value-dependent init");

* Justification inserted immediately after the check message location
* Relaxed initialization will be seen in course on Advanced Proof

=====
Lab
=====

.. include:: labs/5_flow_analysis.lab.rst

=========
Summary
=========

---------------
Flow Analysis
---------------

* Flow analysis builds a Program Dependence Graph
* Flow analysis detects:

  - Interferences between parameters and global variables
  - Read of uninitialized variable
  - Violation of data dependency contracts (:ada:`Global`)

* Flow analysis allows to reach Bronze level
* Flow analysis is imprecise

  - On value-dependent flows
  - On array assignment to index/slice
  - During generation of data dependency contracts
