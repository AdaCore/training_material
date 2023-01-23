**************************
Proving Programs Correct
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

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

-----------------------------------
Verification Condition Generation
-----------------------------------

* Verification Condition/Proof Obligation/Check

   - Logical formula

* Is the verification condition (VC) valid?

   - Can be checked by an automatic prover

* What does it mean if a VC is not valid?
* How are verification conditions generated?

   - Weakest precondition calculus
   - Strongest postcondition calculus (in SPARK)

---------------
Hoare Triples
---------------

* Hoare triples used to reason about program correctness

   - With pre- and postconditions

* Syntax: ``{P}S{Q}``

   - ``S`` is a program
   - ``P`` and ``Q`` are predicates
   - ``P`` is the precondition
   - ``Q`` is the postcondition

* Meaning of ``{P}S{Q}`` triple:

   - If we start in a state where ``P`` is true and execute ``S``, then ``S`` will terminate in a state where ``Q`` is true.

-----------------------
Example Hoare Triples
-----------------------

* Are these valid?

  .. code:: Ada

     {true} x := 5 {x = 5}
     {x = y} x := x + 3 {x = y + 3}
     {x > 0} x := x * 2 {x > -2}
     {x = a} if (x < 0) then x := -x {x = |a|}
     {false} x := 3 {x = 8}

* False implies anything

  - Beware inconsistent preconditions like `X < 0 and X > 5` (and use
  :toolname:`GNATprove` switch :command:`--proof-warnings`)

  - Beware unrealizable contracts on *functions* like a postcondition that `X <
    0 and X > 5` (functions should be proven, including their termination)

-------------------------
Stronger Postconditions
-------------------------

* Example of weak postcondition (precondition is fixed):

  .. code:: Ada

     {X = 5} X := X * 2 {X > 0}

* Is this triplet valid?

* How can we make a stronger postcondition?

   - What about `5 < X and X < 20` ?

* What is the strongest possible postcondition for this precondition and program statement?

  - A weaker postcondition is always provable

.. container:: speakernote

   Strongest postcondition for this precondition and program is X = 10.

----------------------
Weaker Preconditions
----------------------

* Example of strong precondition (postcondition is fixed):

  .. code:: Ada

     {X = 5} X := X * 2 {X > 0}

* How can we make a weaker precondition?

  - Accept more calling contexts as valid

* What is the weakest possible precondition for this postcondition and program statement?

  - In general, precondition is fixed by specifications

.. container:: speakernote

   Weakest precondition for this postcondition and program is X > 0.

-------------------------
Strongest Postcondition
-------------------------

* If ``{P}S{Qstrong}`` and for all ``Q`` such that ``{P}S{Q}``, ``Qstrong => Q``, then ``Qstrong`` is the strongest postcondition of ``S`` with respect to ``P``

* Strongest postcondition computation computes automatically ``Qstrong`` given
  ``P`` and ``S``

* Then, to know if ``{P}S{Q}`` is true, we just check if ``Qstrong => Q``

  - Automatic provers actually check if ``Qstrong and (not Q)`` is satisfiable

* Similar notion of weakest precondition computation

----------------------
Modular Verification
----------------------

.. image:: call_cycle-pre_and_post_condition.png

=========================
Combining Proof and Test
=========================

---------------------------
Comparison Test and Proof
---------------------------

* Pros and Cons

   - Presence/Absence of Bugs
   - State-of-the-art, State-of-practice

* Both techniques imperfect
* Both techniques can be expensive
* Industry standards

   - DO-178C, DO-333

* Another problem - program not all SPARK, not even all Ada - some COTS, Libraries, C???  What can you do?
* How to combine?

---------------------------
Combining Proof and Test
---------------------------

* Same contract for test and proof
* Modular verification
* Combination should be at least as strong as testing
* What happens when some subprograms are tested and some are proved?

--------------------------------------
Proof and Test - Hybrid Verification
--------------------------------------

* Scenario: **tested** procedure calls proved procedure
  - Core proved module in SPARK
* Still modular verification
* Responsibilities!

.. image:: call_cycle-test_pre_prove_post.png

--------------------------------------
Proof and Test - Hybrid Verification
--------------------------------------

* Scenario: **proved** procedure calls tested procedure
  - Application proved in SPARK wrt SPARK API
* Still modular verification
* Responsibilities!

.. image:: call_cycle-prove_pre_test_post.png

------------------------------------------
Combining Proof and Test - Cost Benefit
------------------------------------------

.. container:: columns

 .. container:: column

    * 80/20 rule holds for both test and proof activities
    * Same area of code is usually not simultaneously difficult to prove and difficult to test

 .. container:: column

    .. image:: 80-20_provable_or_testable.png

--------------------------
Combining Proof and Test
--------------------------

* Contracts are executable - they can be checked at runtime and an error is raised when a check fails
* Compilation options to support integration of test and proof

   - Assertion checks enabled via :command:`-gnata` compiler switch
   - Aliasing can be checked at run time with the :command:`-gnateA` switch.
   - Initialization and Validity of Data can be checked at run time with the :command:`-gnateV` and :command:`-gnatVa` switches.
   - See the *SPARK User's Guide* for more details.

-----------------------------------------
:toolname:`GNATprove` Tool Architecture
-----------------------------------------

.. image:: gnatprove-actual_tool_flow.png

==============
Summary
==============

---------
Summary
---------

* Proving correctness of a subprogram

   - Extent to which it complies with its specification.
   - Proof that it can't fail (AoRTE)

* We now need to test/prove contracts

   - We've just moved where the bugs can be found
