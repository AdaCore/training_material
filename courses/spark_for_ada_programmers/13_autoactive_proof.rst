*******************
Auto-Active Proof
*******************

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
Not All Proofs Are Easy
-------------------------

* correct spec + correct code |rightarrow| proof?

* We saw already limitations of automatic provers:

  - Arithmetic - non-linear and mixed arithmetic
  - Quantifiers - existential quantifiers and induction
  - Proof context - may become too large

* :dfn:`Auto-active proof` overcomes these limitations

  - Based on **automatic** provers
  - Using human **interaction**

* Akin to *developing the proof* like we develop code

  - Still much lower effort than required in proof assistants (Coq, Lean,
    Isabelle...)
  - Special code supporting the proof is called :dfn:`ghost code`

-------------------------------
Investigating Unproved Checks
-------------------------------

* Maybe spec is incorrect? Maybe code is incorrect? Or both?

* Need to investigate unproved checks

  - Easiest way is to get runtime failure in spec or code

    + Test the code+spec with assertions enabled!
    + Then debug with the usual debugging tools

  - Increase the proof effort

    + More provers and time to attempt proof

  - Break down property to prove into easier ones

    + Add intermediate assertions
    + Extract proof of a property in a lemma

* Need to understand the messages output by :toolname:`GNATprove`!

  - Tool tries to help you help it

-----------------
The Proof Cycle
-----------------

.. image:: fortify_analyze_prove_cycle.png

.. container:: speakernote

   For SPARK (or other verification systems users) the basic process is the same, but details vary

================================
:toolname:`GNATprove` Messages
================================

--------------------------
Parts of a Check Message
--------------------------

* Messages adapted to usage with switch :command:`--output=`

  - Message in colors with code excerpts in terminal
  - Message on one line in IDEs (further separated by IDE)

* Typical check message consists in multiple parts

  .. code:: console

     file:line:col: severity: check "might fail"
       "cannot prove" this-part
       "e.g. when" counterexample
       "reason for check:" check-is-here-for-that-reason
       "possible fix:" this-or-that-could-fix-it
       continuation-message-with-another-source-location

-----------------------
Check Message Example
-----------------------

What is the problem with this code?

.. code:: ada

   procedure Incr (X : in out Integer) is
   begin
      X := X + 1;
   end Incr;

.. container:: animate

   .. code:: console

      incr.adb:3:11: high: overflow check might fail
        cannot prove upper bound for X + 1
        e.g. when X = Integer'Last
        reason for check: result of addition must fit in
          a 32-bits machine integer
        possible fix: subprogram at line 1 should mention X in
          a precondition

-----------------
Counterexamples
-----------------

* A :dfn:`counterexample` is input values that lead to check failure

* Different displays in a terminal and in IDEs

  - In :toolname:`GNAT Studio`, :toolname:`GNATprove` displays the full path

    + Magnify icon next to check message to display path
    + Values of variables displayed along the path

  - In terminal and other IDEs, :toolname:`GNATprove` displays final values

    + Values of variables in the check expression
    + At the point where the check is failing

* Feature is activated with switch :command:`--counterexamples=on`

  - Off by default at proof levels 0, 1
  - On by default at proof levels 2, 3, 4

* Automatic prover cvc5 is asked for a counterexample on unproved checks

  - Counterexample is re-checked twice by :toolname:`GNATprove`

    + Once by simulating the execution interprocedurally
    + Once by simulating the execution intraprocedurally

  - Result of simulations allows to refine message

    + :command:`high` message when execution is known to fail
    + message points at missing contracts otherwise

--------------
Possible Fix
--------------

* Suggestion of a possible way to fix the problem

  - This might not be the right way!
  - Based on heuristics and most likely reasons

* In general, suggest missing precondition or loop invariant

  - Because some variable in check is not constrained at all

  .. code:: console

     possible fix: precondition of subprogram should mention Var
     possible fix: precondition of subprogram should mention Var'Initialized
     possible fix: add precondition (Expr in Integer) to subprogram
     possible fix: loop should mention Var in a loop invariant

* Also suggests missing postcondition

  .. code:: console

     possible fix: call should mention Var in a postcondition
     possible fix: you should consider adding a postcondition to function
       or turning it into an expression function in its unit spec

* Other suggestions for arithmetic and representation

  .. code:: console

     possible fix: use pragma Overflow_Mode or switch -gnato13
       or unit SPARK.Big_Integers
     possible fix: overlaying object should have an Alignment
       representation clause

-----------------------
Continuation Messages
-----------------------

* Typically points to another relevant source location

* Specific instantiation for code in generics

  .. code:: console

     in instantiation at...

* Specific call for code in inlined subprogram

  .. code:: console

     in call inlined at...

* Specific contract when inherited

  .. code:: console

     for inherited predicate at...
     for inherited default initial condition at...
     in inherited contract at...

* Original contract when inlined

  .. code:: console

     in inlined expression function body at...
     in inlined predicate at...
     in default value at...

----------------------
Information Messages
----------------------

* Information messages about proved or justified checks

  - With switch :command:`--report=all/provers/statistics`
  - Checks justified with pragma :ada:`Annotate`

  .. code:: ada

     file:line:col: check proved
     file:line:col: check justified

* Information about analysis

  - With switch :command:`--info`
  - Subprograms that are inlined or not
  - Loops that are unrolled or not
  - Function contracts not available for proof (termination)
  - Imprecise value for some attributes and functions

=============================
Increasing the Proof Effort
=============================

-----------------------------
Control of the Proof Effort
-----------------------------

* Automatic provers have different strengths

  - More provers = more likely to prove checks
  - From one prover to four (Alt-Ergo, COLIBRI, cvc5, Z3)
  - Use switch :command:`--provers` e.g. :command:`--provers=all`

* Automatic provers heuristically search for a proof

  - More time = more likely to prove checks
  - Time given in seconds (:command:`--timeout`) or prover-specific steps
    (:command:`--steps`)

* Default proof effort is minimal (one prover, 100 steps)

* Timeout vs steps

  - Timeout is best to bound the running time
  - Steps are useful for reproducible results across machines

    + Still use timeout to avoid runaway proofs

--------------
Proof Levels
--------------

* Switch :command:`--level` bundles lower-level switches

  - :command:`--level=0` uses 1 prover and 1sec timeout
  - :command:`--level=1` uses 3 provers and 1sec timeout
  - :command:`--level=2` uses 3 provers and 5sec timeout
  - :command:`--level=3` uses 3 provers and 20sec timeout
  - :command:`--level=4` uses 3 provers and 60sec timeout

* Level 2 is the recommended one to start

  - Activation of counterexamples also starts at level 2

* Levels do not use steps (:command:`--steps=0`) and increase memory limit
  (:command:`--memlimit`)

* Specific values for lower-level switches take precedence

  - e.g. :command:`--level=2 --timeout=120 --steps=10000`

----------------------
Running Proof Faster
----------------------

* During development, run :toolname:`GNATprove` on relevant part

  - On given file

    + With :menu:`SPARK` |rightarrow| :menu:`Prove File` in :toolname:`GNAT Studio`
    + With task :menu:`Prove file` in Visual Studio Code
    + With :command:`-u file` in terminal

  - On given subprogram, selected region of code, selected line of code

    + With corresponding menus in IDEs and switches in terminal

* Use parallelism with :command:`-j` e.g. :command:`-j0` for all cores

  - Proof faster on more powerful machines: more cores, more memory, faster
    clock

* Sharing session files by setting attribute :code:`Proof_Dir` in project file

  - This also allows to simply replay proofs with :command:`--replay`

* Sharing proof results via a cache

  - Can store database in a file, or connect to a Memcached server

============
Ghost Code
============

-------------------------
Intermediate Assertions
-------------------------

* Intermediate assertions can help provers

  .. code:: ada

     pragma Assert (Intermediate_Assertion_1);
     pragma Assert (Intermediate_Assertion_2);
     pragma Assert (Complex_Assertion);

* In addition, each assertion can be proven by different prover

* Intermediate assertions help prove each path separately

  .. code:: ada

     if Cond then
        pragma Assert (Assertion_1);
        return;
     end if;

     if Other_Cond then
        pragma Assert (Assertion_2);
     else
        pragma Assert (Assertion_3);
     end if;

* Intermediate assertions are essential to investigate unproved checks

------------
Ghost Code
------------

* :dfn:`Ghost code` is code meant only for verification

  - Intermediate assertions are a special case of ghost code
  - Contracts are also part of ghost code

* Special aspect :ada:`Ghost` used to identify ghost entities

  - Ghost functions express properties used in contracts

    .. code:: ada

       function Is_Valid (X : T) return Boolean is (...)
         with Ghost;
       procedure Proc (X : T) with Pre => Is_Valid (X);

  - Ghost variables hold intermediate values referred to in assertions

    .. code:: ada

       X_Saved : constant T := X with Ghost;
       ...
       pragma Assert (X = 3 * X_Saved);

  - But also ghost types, procedures, packages

* Ghost statements are:

  - Calls to ghost procedures
  - Assignments to ghost variables

---------------------------
Compilation of Ghost Code
---------------------------

* Ghost code compiled by GNAT

  - When using switch :command:`-gnata`
  - Or pragma :ada:`Assertion_Policy (Ghost => Check)`

* :toolname:`GNATprove` checks that ghost code has no effect

  .. code:: ada

     X_Saved : constant T := X with Ghost;
     ...
     X_Saved := X; -- ghost assignment
     X := X_Saved; -- error

* Same behavior with or without ghost code

  - Proof using ghost code
  - Even if execution without ghost code

-----------------
Ghost Functions
-----------------

* Most common ghost entities

|

* Ghost functions express properties used in contracts

  - Typically as expression functions
  - Complete the existing API with queries only for verification

|

* Ghost functions can be very costly in running time

  - If objective is not to execute them!
  - Typically when creating models of the actual types
  - e.g. using SPARK functional containers (sets, maps, etc)
  - e.g. like it is done for SPARK formal containers

-----------------
Ghost Variables
-----------------

* Local ghost variable or constant

  - Typically to store intermediate values

    + e.g. value of variable at subprogram entry

  - Also used to build useful data structure supporting proof

    .. code:: Ada

       procedure Sort (T : in out Table)
         with Post => Is_Permutation (T, T'Old)
       is
         Permutation : Index_Array := (for J in T'Range => J)
           with Ghost;
       begin

* Global ghost variable

   - Help specify and verify interprocedural properties
   - Maintain a model of a complex or private data structure
   - Specify properties over sequence of calls

------------------
Ghost Procedures
------------------

* Inlined local ghost procedure without contract

  - Used to group operations on ghost variables
  - Guarantees removal of all the code (e.g. loops, conditionals)

* Ghost procedure with contract and no effects

  - Also called :dfn:`lemma`
  - Isolates the proof that the precondition implies the postcondition
  - Proof of lemma can be full automatic

    .. code:: Ada

       procedure Lemma (X : T)
       with
         Pre  => ...,
         Post => ...;
       procedure Lemma (X : T) is null;

  - Lemma is used by calling it on relevant arguments

    .. code:: Ada

       pragma Assert (precondition-of-lemma);
       Lemma (Y);
       -- postcondition of lemma known here

---------------------
SPARK Lemma Library
---------------------

* Part of SPARK Library in :ada:`SPARK.Lemmas.<unit>`

* Mostly non-linear arithmetic lemmas

  - Generics instantiated for standard numerical types
  - On signed and modular integer arithmetic

    .. code:: Ada

       procedure Lemma_Div_Is_Monotonic
         (Val1  : Int;
          Val2  : Int;
          Denom : Pos)
       with
         Global => null,
         Pre  => Val1 <= Val2,
         Post => Val1 / Denom <= Val2 / Denom;

  - On fixed-point arithmetic (specific to GNAT)

  - On floating-point arithmetic

    + Monotonicity of operations, conversions with integer, rounding

=====
Lab
=====

.. include:: labs/13_autoactive_proof.lab.rst

=========
Summary
=========

-------------------
Auto-active Proof
-------------------

* Not all proofs are easy

* Understand tool messages

  - Messages guide you to help the tool
  - Many useful parts in a message

* Auto-active proof needed for harder proofs

  - Intermediate assertions
  - Ghost code for specification and verification
  - Lemmas to separately prove properties

* Ghost code has no effect

  - Compiler can ignore it or compile it
