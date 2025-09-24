***************************************
GNAT Static Analysis Suite (GNAT SAS)
***************************************

.. container:: PRELUDE BEGIN

.. container:: PRELUDE ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. role:: rust(code)
    :language: Rust

.. container:: PRELUDE SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. container:: PRELUDE REQUIRES

.. container:: PRELUDE PROVIDES

.. container:: PRELUDE END

==========================
Advanced Static Analysis
==========================

--------------------------
What Is Static Analysis?
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

* Costs shift

    + From later, **expensive** phases
    + To earlier, **cheaper** phases

.. image:: relative_cost_to_fix_bugs.svg

-------------------------------
Why Use :toolname:`GNAT SAS`?
-------------------------------

+ Efficient automated code reviewer

  + Identifies runtime errors with a **level of certainty**

    + E.g. buffer overflows, division by zero

  + Flags legal but **suspect** code

    + Typically logic errors

+ Detailed subprograms analysis
+ Can analyze existing code bases

  + Detect and remove **latent bugs**
  + Legacy code
  + Code from external sources

------------------------------
Detailed Subprogram Analysis
------------------------------

+ **Explicit** specification

    + Written in the code
    + Types
    + Contracts
    + Assertions
    + etc...

+ **Implicit** specification

    + Assumptions by :toolname:`GNAT SAS`
    + :dfn:`Deduced preconditions`
