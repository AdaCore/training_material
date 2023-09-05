**************************
Formal Methods and SPARK
**************************

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

-------------------------
High-Integrity Software
-------------------------

* Also known as (safety- or security- or mission-) :dfn:`critical software`
* Has reliability as the most important requirement

  - More than cost, time-to-market, etc.

* Must be known to be **reliable** before being deployed

  - With **extremely** low failure rates

        + e.g., 1 in 10:superscript:`9` hours (114,080 **years**)

  - Testing alone is insufficient and/or infeasible for such rates

* Is not necessarily safety-critical (no risk of human loss)

  - Satellites
  - Remote exploration vehicles
  - Financial systems

------------------------------------
Developing High-Integrity Software
------------------------------------

* Software quality obtained by a combination of

  - Process

        + Specifications
        + Reviews
        + Testing
        + Others: audits, independence, expertise...

  - Arguments

        + System architecture
        + Use cases
        + Programming language
        + Static code analysis
        + Dynamic code analysis
        + etc...

* Need to comply with a certification regime

  - Process-based or argument-based
  - Independently assessed (avionics, railway) or not (automotive)

================
Formal Methods
================

----------------
Formal Methods
----------------

* Mathematical techniques applied to the development or verification of
  software

  - :dfn:`Heavyweight formal methods` expose the maths to users
  - :dfn:`Lightweight formal methods` hide the maths from users

* Industrially usable formal methods

  - Are applicable to **existing** development **artifacts** (models, code, etc.)
  - Are automated and integrated in **existing processes**
  - Provide value for **certification**
  - Explicitly **encouraged** by some standards

     + Railway (EN 50128)
     + Avionics (DO-178C + DO-333 Formal Methods Supplement)
     + Security (Common Criteria)

-----------------------------
Static Analysis of Programs
-----------------------------

* :dfn:`Abstract interpretation` (AbsInt)

  - AbsInt analyzes an **abstraction** of the program

* :dfn:`Symbolic execution` (SymExe) and :dfn:`bounded model checking` (BMC)

  - Both analyze possible traces of execution of the program
  - SymExe explores traces **one by one**
  - BMC explores traces **all at once**

* :dfn:`Deductive verification` (Proof)

  - Proof analyzes functions **against their specification**

* Static analysis is a formal method when it is :dfn:`sound`

  - Soundness means no missing alarms

* All techniques have different costs and benefits

--------------------------------------
Goals of Static Analysis of Programs
--------------------------------------

* **Automation** is better with AbsInt and SymExe/BMC

  - Proof incurs the cost of writing specification of functions

|

* **Precision** is better with SymExe/BMC and Proof

  - Automatic provers are **more powerful** than abstract domains
  - SymExe/BMC explore infinitely many traces

    + Limit the exploration to a subset of traces

|

* **Soundness** is better with AbsInt and Proof

  - Soundness is not missing alarms (aka :dfn:`false negatives`)
  - AbsInt may cause false alarms (aka :dfn:`false positives`)
  - Sound handling of loops and recursion in AbsInt and Proof

---------------------------------------------
Capabilities of Static Analysis of Programs
---------------------------------------------

* **Modularity** is the ability to analyze a partial program

  - Most programs are partial

    + Libraries themselves
    + Use of external libraries
    + Program during development

  - Proof is inherently modular

|

* **Speed** of the analysis drives usage

  - Unsound analysis can be much faster than sound one
  - For sound analysis, modular analysis is faster

|

* **Usage** depends on capabilities

  - Fast analysis with no false alarms is better for :dfn:`bug-finding`
  - Modular analysis with no missing alarms is better for :dfn:`formal verification`

---------------------------------------
Comparing Techniques on a Simple Code
---------------------------------------

* Consider a simple loop-based procedure

.. code:: ada

   procedure Reset (T : in out Table; A, B : Index) is
   begin
      for Idx in A .. B loop
         T(Idx) := 0;
      end loop;
   end;

* :ada:`T(Idx)` is safe |equivalent| :ada:`Idx in Table'Range`
* As a result of calling :ada:`Reset`:

  - Array :ada:`T` is initialized between indexes :ada:`A` and :ada:`B`
  - Array :ada:`T` has value zero between indexes :ada:`A` and :ada:`B`

-------------------------
Abstract Interpretation
-------------------------

* :ada:`Reset` is analyzed in the context of each of its calls

  - If the values of :ada:`Table`, :ada:`A`, :ada:`B` are precise enough,
    AbsInt can deduce that :ada:`Idx in Table'Range`

  - Otherwise, an **alarm** is emitted (for sound analysis)

|

* Initialization and value of individual array cells is **not** tracked

  - The assignment to a cell is a :dfn:`weak update`

    + The abstract value for the whole array now includes value zero
    + ... but is also possibly uninitialized or keeps a previous value

  - After the call to :ada:`Reset`, the analysis does **not** know that :ada:`T`
    is initialized with value zero between indexes :ada:`A` and :ada:`B`

-----------------------------------------------
Symbolic Execution and Bounded Model Checking
-----------------------------------------------

* :ada:`Reset` is analyzed in the context of **program traces**

  - If the values of :ada:`A` and :ada:`B` are *close enough*, SymExe/BMC can
    analyze all loop iterations and deduce that :ada:`Idx in Table'Range`

  - Otherwise, an **alarm** is emitted (for sound analysis)

|

* Analysis of loops is limited to few iterations (same for recursion)

  - The other iterations are ignored or approximated, so the value of :ada:`T`
    is **lost**

  - After the call to :ada:`Reset`, the analysis does **not** know that :ada:`T`
    is initialized with value zero between indexes :ada:`A` and :ada:`B`

------------------------
Deductive Verification
------------------------

* :ada:`Reset` is analyzed in the context of a :dfn:`precondition`

  - Predicate defined by the user which restricts the calling context
  - Proof checks if the precondition entails :ada:`Idx in Table'Range`
  - Otherwise, an **alarm** is emitted

* Initialization and value of individual array cells is tracked
* Analysis of loops is based on user-provided :dfn:`loop invariants`

  :ada:`T(A .. Idx)'Initialized and T(A .. Idx) = (A .. Idx => 0)`

* Code after the call to :ada:`Reset` is analyzed in the context of a
  :dfn:`postcondition`

  :ada:`T(A .. B)'Initialized and T(A .. B) = (A .. B => 0)`

  - So the analysis now **knows** that :ada:`T` is initialized with value zero between
    indexes :ada:`A` and :ada:`B`

=======
SPARK
=======

--------------------------
SPARK is a Formal Method
--------------------------

* **Soundness** is the most important requirement (no missing alarms)

|

* Analysis is a **combination of techniques**

  - :dfn:`Flow analysis` is a simple form of modular abstract interpretation
  - :dfn:`Proof` is modular deductive verification

|

* Inside proof, abstract interpretation is used to compute **bounds** on arithmetic
  expressions

  - Based on type bounds information
  - e.g if :ada:`X` is of type :ada:`Natural`
  - Then :ada:`Integer'Last - X` cannot overflow

----------------------------
SPARK is a Language Subset
----------------------------

* Static analysis is **very tied** to the programming language

  - Strong typing **simplifies** analysis
  - Some language features **improve** analysis precision

    + e.g. first-class arrays with bounds
      :ada:`Table'First` and :ada:`Table'Last`

  - Some language features **degrade** analysis precision

    + e.g. arbitrary aliasing of pointers, dispatching calls in
      OOP

|

* SPARK hits the **sweet spot** for proof

  - Based on strongly typed feature-rich Ada programming language
  - **Restrictions** on Ada features to make proof easier

    1. Simplify user's effort for annotating the code

    2. Simplify the job of automatic provers

|

* "SPARK" originally stands for "SPADE Ada Ratiocinative Kernel"

------------------
History of SPARK
------------------

* *Vintage SPARK* followed Ada revisions

  - SPARK 83 based on Ada 83
  - SPARK 95 based on Ada 95
  - SPARK 2005 based on Ada 2005

|

* Since 2014, *SPARK* is updated annually

  - OO programming added in 2015
  - Concurrency added in 2016
  - Type invariants added in 2017
  - Pointers added in 2019
  - Exceptions added in 2023

============================
Applying SPARK in Practice
============================

------------------------------
Levels of Software Assurance
------------------------------

* Various reasons for using SPARK

|

* Levels of software assurance

  1. **Stone level** - valid SPARK

  2. **Bronze level** - initialization and correct data flow

  3. **Silver level** - absence of run-time errors (AoRTE)

  4. **Gold level** - proof of key integrity properties

  5. **Platinum level** - full functional proof of requirements

|

* Higher levels are more costly to achieve

|

* Higher levels build on lower levels

  - Project can decide to move to higher level later

------------------------------------------
Levels of Software Assurance in Pictures
------------------------------------------

.. image:: software_assurance_levels.png

---------------------------
Objectives of Using SPARK
---------------------------

* **Safe** coding standard for critical software

  - Typically achieved at **Stone or Bronze** levels

* Prove absence of run-time errors (:dfn:`AoRTE`)

  - Achieved at **Silver** level

* Prove correct **integration** between components

  - Particular case is correct API usage

* Prove **functional correctness**
* Ensure correct behavior of parameterized software
* Safe **optimization** of run-time checks
* Address data and control coupling
* Ensure portability of programs

.. container:: speakernote

   Details of objectives are in section 8.2 of SPARK UG.

-------------------
Project Scenarios
-------------------

* Maintenance and evolution of existing Ada software

  - Requires migration to SPARK of a part of the codebase
  - Fine-grain control over parts in SPARK or in Ada
  - Migration guide available

:url:`https://www.adacore.com/books/implementation-guidance-spark`

  - Can progressively move to higher assurance levels

* New developments in SPARK

  - Either completely in SPARK
  - More often interfacing with other code in Ada/C/C++, etc.

======
Quiz
======

-----------------------
Quiz - Formal Methods
-----------------------

Which statement is correct?

   A. A formal method analyses code.
   B. :answer:`A formal method has no missing alarms.`
   C. A formal method has no false alarms.
   D. Static analysis of programs should be automatic, precise and sound.

.. container:: animate

   Explanations

   A. Formal methods can also apply to requirements, models, data, etc.
   B. Correct
   C. To achieve soundness, it may be impossible to avoid false alarms.
   D. Not all three at the same time.

--------------
Quiz - SPARK
--------------

Which statement is correct?

   A. SPARK is a recent programming language.
   B. SPARK is based on proof.
   C. SPARK analysis can be applied to any Ada program.
   D. :answer:`SPARK requires annotating the code with specifications`.

.. container:: animate

   Explanations

   A. SPARK is a subset of Ada dating back to the 80s.
   B. SPARK is also based on flow analysis which is a form of abstract interpretation.
   C. SPARK subset restricts the features of Ada for proof.
   D. Correct

--------------------------
Quiz - SPARK in Practice
--------------------------

Which statement is correct?

   A. :answer:`There are 5 levels of software assurance with SPARK.`
   B. Proving absence of run-time errors is hard with SPARK.
   C. Full functional correctness is impossible to prove with SPARK.
   D. SPARK code cannot be mixed with other programming languages.

.. container:: animate

   Explanations

   A. Correct
   B. AoRTE is a common objective with SPARK because it is simple.
   C. Full functional correctness is hard but can be achieved.
   D. SPARK code can be interfaced with code in Ada/C/C++, etc.

=========
Summary
=========

--------------------------
Formal Methods and SPARK
--------------------------

* Development of large, complex software is **difficult**

  - Especially so for high-integrity software

|

* Formal methods **can** be used industrially

  - During development and verification
  - To address objectives of certification
  - They must be sound (no missing alarm) in general

|

* SPARK is an **industrially** usable formal method

  - Based on flow analysis and proof
  - At various levels of software assurance
