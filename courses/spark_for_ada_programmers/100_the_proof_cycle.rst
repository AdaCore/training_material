
*****************
The Proof Cycle
*****************

==============
Introduction
==============

-----------------------------------
Analysis of Failed Proof Attempts
-----------------------------------

.. container:: columns

 .. container:: column

    * It should work!?
    * Did I get something wrong?

    * Error in code or spec?

    * Need to strengthen proof?

 .. container:: column

    .. image:: failed_proof_bus_example.png

-----------------
The Proof Cycle
-----------------

.. image:: fortify_analyze_prove_cycle.png

.. container:: speakernote

   For SPARK (or other verification systems users) the basic process is the same, but details vary

=================
Unproved Checks
=================

----------------------------------
How to Deal with Unproved Checks
----------------------------------

* Two sources of unproved checks:

   - Incorrectness of

      + Code
      + Contract (just as often!)

* Great, you have found a bug!

   - The tools are not smart enough and need some help

      + Guide the tools...
      + ... improve contracts and code organization at the same time

.. container:: speakernote

   Which source of unproved checks is more common?

----------------------------------
How to Deal with Unproved Checks
----------------------------------

* The tools will show you the source code location of the unproved check
* Clicking on the magnifying glass next to the source line number will highlight the path leading to the unproved check.

|
.. image:: unproved_check_example.png
   :width: 85%

.. container:: speakernote

   The tricks we see on the following slides helps dealing with incompleteness, something we can not avoid entirely.

----------------------------------
How to Deal with Unproved Checks
----------------------------------

* Unless the check is very obviously not provable at first glance then it is usually a good idea to try increasing the proof level

   - If that works, great!
   - If not, reduce it again and try something else...

----------------------------------
How to Deal with Unproved Checks
----------------------------------

.. image:: prove_dialog-basic-proof_level.png

----------------------------------
How to Deal with Unproved Checks
----------------------------------

* Try executing the code in question - can you get the check to fail? (If you can, that should give a big hint as to how to proceed!)
* Is it the execution of the contract itself that gives rise to a potential run-time exception?
* See :command:`gnatprove --help` for advanced proof options

----------------------------------
How to Deal with Unproved Checks
----------------------------------

* Having identified the problem, what next?

   - Strengthen preconditions
   - Better use of subtypes
   - Fix bugs in code!
   - Use another prover

      + Automatic
      + Manual

   - Intermediate assertions
   - Lemmas

-------------------------
Intermediate Assertions
-------------------------

* As contracts for functional correctness can be complex, users may need to guide the proof tool

   - Intermediate assertions may help the tool verify a complex reasoning
   - It may be useful to express the property we want to verify in a different way, even if it is theoretically equivalent
   - Remaining unproved assertions can be discharged by test or by review

.. code:: Ada

   pragma Assert (Assertion_Checked_By_The_Tool);
   --  info: assertion proved
   pragma Assert (Assumption_Validated_By_Other_Means);
   --  medium: assertion might fail
   pragma Assume (Assumption_Validated_By_Other_Means);
   --  The tool does not attempt to check this expression.
   --  It is recorded as an assumption.

.. container:: speakernote

   As properties of interest for functional correctness are more complex than those involved in proof of program integrity, it is expected that GNATprove may not be able to verify them right away even though they are valid.
   Techniques for debugging failed proof attempts explained in the proof of program integrity course will come in handy here.
   We don't go over them again in this course but rather focus on improving results on the remaining cases where the property is valid but is not proved by GNATprove in a reasonable amount of time.
   In these cases, users may want to try and guide GNATprove in order either to complete the proof or strip it down to a small number of easily reviewable assumptions.
   For this purpose, assertions can be added to break complex proofs into smaller steps.
   In particular, it may be a good idea, as an intermediate step, to try and prove a theoretically equivalent version of the desired property where things have been simplified for the prover, for example by splitting different cases or by inlining the definitions of functions.
   Finally, it can be the case that some intermediate assertions are not discharged by GNATprove, either because it is missing some information or because it gets lost in the amount of information available.
   Those remaining assertions can then be verified by other means like testing, since they are executable, or review.
   Users can choose to instruct GNATprove to ignore them, either by turning them into assumptions, like in our example, or by justifying the check using a pragma Annotate.
   In both cases, the assumption will still be checked at runtime when assertions are enabled.

--------
Lemmas
--------

* A lemma is a ghost procedure with

  - a precondition to state the hypotheses
  - a postcondition to state the conclusion

* The lemma can be independently proved or justified

* Lemma is used by inserting call where needed

  - Or add annotation `Automatic_Instantiation`

* SPARK Lemma Library is a set of predefined proved lemmas

=======================
:toolname:`GNATprove`
=======================

---------------------------------------------
Interpreting :toolname:`GNATprove` Messages
---------------------------------------------

* :toolname:`GNATprove` issues four different kinds of messages: **errors**, **warnings**, **checks** and **information** messages.

* Errors are issued for SPARK violations or other language legality problems, or any other problem which does not allow proceeding to analysis.

      + Errors cannot be suppressed and must be fixed before proceeding with analysis.

* Warnings are issued for any suspicious situation

      + Examples: unused values of variables, useless assignments
      + Warnings are prefixed with the text **warning:** and can be suppressed with `pragma Warnings`

---------------------------------------------
Interpreting :toolname:`GNATprove` Messages
---------------------------------------------

* Checks are issued for any potential problem in the code which could affect the correctness of the program

   - Examples: missing initialization, possible failing run-time checks or unproved assertions.

   - Checks come with a severity and depending on the severity the message text is prefixed with **low**, **medium** or **high**.
   - Check messages cannot be suppressed like warnings, but they can be individually justified with `pragma Annotate`

* Information messages are issued for proved checks in some modes of :toolname:`GNATprove`

------------------------------------------
:toolname:`GNATprove` Messages - Ranking
------------------------------------------

* Check messages get a rank of **high**, **medium** or **low** according to both the severity of the problem, and the likelihood that it corresponds to a true problem

   - Example 1: reading a certain locally uninitialized variable gets a *high* rank,

   - Example 2: returning an OUT parameter with a component uninitialized gets a *medium* rank if this occurs on all paths, and a *low* rank if this occurs only on some path.

--------------------------
Some Notes About Ranking
--------------------------

* Ranking helps the user to direct and prioritize review effort
* Helpful when running :toolname:`GNATprove` as a bug-finding static analysis tool
* **NOTE** Regardless of ranking, a failed check always means that there is a potential soundness issue!

.. container:: speakernote

   If you want sound analysis, you have to deal with all checks, regardless of ranking!

-----------------
Warning Control
-----------------

* SPARK warnings are controlled with switch :command:`--warnings`:

   - :command:`--warnings=off` suppresses all warnings
   - :command:`--warnings=error` treats warnings as errors
   - :command:`--warnings=continue` issues warnings but does not stop analysis (default)
   - The default is that :toolname:`GNATprove` issues warnings but does not stop.

* Warnings can be suppressed selectively by the use of `pragma Warnings` in the source code

-----------------
Pragma Warnings
-----------------

.. code:: Ada

   package body Warnings_Example is
      procedure Mumble (X : Integer) is
      begin
         null;
      end Mumble;
   end Warnings_Example;

* Warning issues by :toolname:`GNATprove`

   .. code:: console

      warnings_example.ads:2:11:
      warning: subprogram "Mumble" has no effect

-----------------
Pragma Warnings
-----------------

.. code:: Ada

   pragma Warnings (Off, "subprogram ""Mumble"" has no effect");
   procedure Mumble (X : Integer);
   pragma Warnings (On, "subprogram ""Mumble"" has no effect");

or better for :toolname:`GNATprove`:

.. code:: Ada

   pragma Warnings (GNATprove, Off,
                    "subprogram ""Mumble"" has no effect");
   procedure Mumble (X : Integer);
   pragma Warnings (GNATprove, On,
                    "subprogram ""Mumble"" has no effect");

or better here:

.. code:: Ada

   procedure Mumble (X : Integer)
     with Global => null;

---------------------------
Control of Check Messages
---------------------------

* You can suppress check messages using pragma Annotate:

   .. code:: Ada

      return (X + Y) / (X - Y);
      pragma Annotate (GNATprove, False_Positive,
         "divide by zero", "reviewed by John Smith");

* The pragma has the following form:

   .. code:: Ada

    pragma Annotate (GNATprove, Category, Pattern, Reason);

   - `GNATprove` here is a fixed identifier
   - `Category` is one of `False_Positive` or `Intentional`
   - `Pattern` is a string literal describing the pattern of the messages which shall be suppressed
   - `Reason` is a string literal providing a reason for the suppression.
   - All arguments should be provided.

--------
Assume
--------

* No verification condition is generated to prove that the boolean expression is true
* But it is carried forward as though it had been proved true
* Will be checked at run time if assertion checks are on

   .. code:: Ada

      pragma Assume (Ticks < Time_Type'Last);

* Soundness alert - use with great care!

==========
Summary
==========

------------------------
We're Still Debugging!
------------------------

* If we cannot prove a subprogram

   + Try a different prover

      - Or increase proof level

   + Verify preconditions

      - So prover has a valid foundation

   + Verify code

      - Make sure code does what you think it does

   + Help prover

      - "Hold its hand" - with simple assertions
      - "Because I said so" - with assumptions
