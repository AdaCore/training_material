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

