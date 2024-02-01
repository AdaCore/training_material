************************
Advanced Flow Analysis
************************

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
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

------------------------------------
Data and Information Flow Analysis
------------------------------------

* Data flow analysis

  - Models the variables used by a subprogram
  - Enforces data initialization policy
  - Detects reads of uninitialized data

* Data dependencies can be specified

  - Introduced by aspect :ada:`Global`

* Information flow analysis

  - Models the flow of information from inputs to outputs
  - Can be very useful for security analysis

* Flow dependencies can be specified

  - Introduced by aspect :ada:`Depends`

===========================
Information Flow Analysis
===========================

---------------------------
Direct and Indirect Flows
---------------------------

* A direct flow occurs when assigning :ada:`A` to :ada:`B`

  .. code:: ada

     B := A;

* An indirect flow occurs when assigning :ada:`B` conditioned on :ada:`A`

  .. code:: ada

     if A then
        B := ...
     end if;

* A direct flow can be masquerading as indirect flow

  .. code:: ada

     if A then
        B := True;
     else
        B := False;
     end if;

* :toolname:`GNATprove` handle both flows together in flow analysis

-------------------------------------
Self-Dependency on Array Assignment
-------------------------------------

* Flow analysis is not value-dependent

* Assigning an array component or slice preserves part of the original value

  .. code:: ada

     type T is array (1 .. 2) of Boolean;
     A : T := ...

     A (1) := True;
     -- intermediate value of A seen as dependent on
     -- original value
     A (2) := False;
     -- final value of A seen as dependent on original value

* This holds also for slices

  .. code:: ada

     A (1 .. 2) := True;
     -- final value of A seen as dependent on original value

===========================
Flow Dependency Contracts
===========================

---------------------------------
Basic Data Dependency Contracts
---------------------------------

* Introduced by aspect :ada:`Depends`

* Optional, but must be complete if specified

* Describes how outputs depend on inputs

  .. code:: Ada

     procedure Proc
     with
       Depends => (X => (X, Y),
                   Z => V);

* Not very interesting for functions which have only their result as output

  .. code:: Ada

     function Func (X : Integer)
     with
       Depends => (Func'Result => (X, Y, Z));

-----------------------------------
Some Outputs May Appear as Inputs
-----------------------------------

* Parts of outputs are in fact inputs:

  - Bounds of arrays

  - Discriminants of records

  - Tags of tagged records

* These output objects will appear as inputs in ``Depends`` when
  bounds/discriminants/tags not implied by the object subtype

  .. code:: Ada

     procedure Proc (Tab : out Table)
     with
       Global => (Output => Glob),
       Depends => (Tab  => Tab,
                   Glob => Glob);

---------------
Special Cases
---------------

* Some outputs may depend on no input

  - Typically when initializing data to some constant value
  - Thus, output depends on *null*

  .. code:: Ada

     procedure Init (T : out Table)
     with
       Depends => (T => null);

* Some inputs may not flow into any output

  - Typically when effect hidden from analysis
  - Or input used only for debug
  - Also the case for global variables of mode :ada:`Proof_In`
  - Must be last line of flow dependencies

  .. code:: Ada

     procedure Debug (T : Table)
     with
       Depends => (null => T);

------------------
Special Notation
------------------

* Outputs can also be grouped

  .. code:: Ada

     procedure Init (T1, T2 : out Table)
     with
       Depends => ((T1, T2) => null);

* Symbol **+** indicates a self-dependency

  .. code:: Ada

     procedure Update (T : in out Table)
     with
       Depends => (T => +null);  -- same as (T => T)

* Most useful with grouped outputs

  .. code:: Ada

     procedure Update (T1, T2 : in out Table)
     with
       Depends => ((T1, T2) => +null);
                  -- same as (T1 => T1, T2 => T2)

======================
Automatic Generation
======================

------------------------
From Data Dependencies
------------------------

* Data dependencies may be specified or generated

|

* If flow dependencies are not specified, they are generated

  - All outputs depend on all inputs
  - All globals of mode :ada:`Proof_In` have no effect on outputs

|

* This is a correct over-approximation of actual flow dependencies

  - This might be too imprecise for analysis of callers
  - In that case, add explicit flow dependencies

------------------------
From Flow Dependencies
------------------------

* If only flow dependencies are specified

|

* Data dependencies are generated

  - All variables only on the left-hand side are outputs
  - All variables only on the right-hand side are inputs
  - All other variables are both inputs and outputs

|

* This is the exact data dependencies consistent with flow dependencies

  - Except some globals of mode :ada:`Proof_In` may be classified as inputs

=====
Lab
=====

.. include:: labs/11_advanced_flow_analysis.lab.rst

=========
Summary
=========

------------------------
Advanced Flow Analysis
------------------------

* Flow dependencies can be specified

  - This can be important for security

* Flow analysis detects:

  - Violation of flow dependency contracts (:ada:`Depends`)
  - Inconsistency between data and flow dependency contracts

* Flow analysis is imprecise

  - On value-dependent flows
  - On array assignment to index/slice
