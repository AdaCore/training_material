**********
Overview
**********

.. container:: PRELUDE BEGIN

.. container:: PRELUDE ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

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

===================
About This Course
===================

--------
Styles
--------

* :dfn:`This` is a definition
* :filename:`this/is/a.path`
* :ada:`code is highlighted`
* :command:`commands are emphasised --like-this`

=======================================
GNAT Static Analysis Suite (GNAT SAS)
=======================================

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

--------------------------------------
How Does Static Analysis Save Money?
--------------------------------------

* Costs shift

    + From later, **expensive** phases
    + To earlier, **cheaper** phases

.. image:: relative_cost_to_fix_bugs.jpg
    :width: 100%

-------------------
What Is GNAT SAS?
-------------------

* Set of analysis engines with complementary capabilities

* Able to detect range of issues spanning from breaking coding style standards to deep logic errors

* Designed to support large systems and to detect wide range of programming errors such as

  + Misuse of pointers
  + Indexing out of arrays
  + Buffer overflows
  + Numeric overflows
  + Numeric wraparounds
  + Improper use of Application Programming Interfaces (APIs)
  + and more

------------------------
What Does GNAT SAS Do?
------------------------

* Pinpoints root cause of each error to the source line of code

* Analyzes partial or full systems to produce reports

* Maintains history to compare current results to a baseline
