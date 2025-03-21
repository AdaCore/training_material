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

+ Tools that can work together to analyze code execution

  + :toolname:`GNATcoverage`

    + Indicates which lines/decisions/branches have been reached during execution

  + :toolname:`GNATtest`

    + Creates framework to build software tests for your codebase

  + :toolname:`GNATfuzz`

    + Automates dynamic testing by generating and executing test cases to discover software vulnerabilities
