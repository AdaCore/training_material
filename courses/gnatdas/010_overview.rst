*******************
GNAT DAS Overview
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

========================================
GNAT Dynamic Analysis Suite (GNAT DAS)
========================================

---------------------------
What Is Dynamic Analysis?
---------------------------

+ Process of testing and evaluating an application while it is running

+ Dynamic analysis finds properties that hold for one or more executions

  + Can't prove a program satisfies a particular property
  + But can detect violations and provide useful information

-------------------
What Is GNAT DAS?
-------------------

+ Two tools that can work together to analyze code execution

  + :toolname:`GNATcoverage`

    + Indicates which lines/decisions/branches have been reached during execution

  + :toolname:`GNATtest`

    + Creates framework to build software tests for your codebase
