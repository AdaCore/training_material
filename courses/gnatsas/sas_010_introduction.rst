***************************************
GNAT Static Analysis Suite (GNAT SAS)
***************************************

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

==========================
Advanced Static Analysis
==========================

--------------------------
What is Static Analysis?
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

.. image:: relative_cost_to_fix_bugs.jpg
    :width: 100%

-------------------------------
Why Use :toolname:`GNAT SAS`?
-------------------------------

+ Efficient, potentially exhaustive code reviewer

  + Identifies run-time errors with a **level of certainty**

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
